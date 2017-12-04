rm(list=ls());
graphics.off();
require(RMySQL);
require(ts);
require("xts");
require("fGarch");
require("sde");
source("~/cake/libeix.r");

## Number of shares to buy/short. Always positive.
pstn.sz.shape <- function(x, dist)
{
    lower <- if (x < 0) -Inf else x;
    upper <- if (x < 0) x else Inf;

    loss <- Inf;
    if (dist$dist == "norm") {
        loss <- integrate(f=function(x) x * dnorm(x=x, mean=dist$mu, sd=dist$sig),
                          lower=lower, upper=upper
                          );
    } else if (dist$dist == "t") {
        loss <- integrate(f=function(x) x * dt(x=x - dist$mu, df=dist$df),
                          lower=lower, upper=upper
                          );
    }
    return(x/loss$value);
}

## Assume the deviations are normally distributed
## Trade size increases exponentially with the deviation
## Determine the rate of exponential growth such that the
## expected trade size is a fixed number for a given threshold.
## expected.pos.sz <- function(lambda, sig, threshold)
## {
##     -exp(lambda^2 * sig^2 / 2) * (
##         erf((threshold - lambda * sig^2)/sqrt(2 * sig)) - 1
##     )/2
## }

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");


stmt <- paste(
    "select ko_daily.tm as tm,",
    "ko_daily.closing as ko,",
    "pep_daily.closing as pep,",
    "spy_daily.closing as spy,",
    "xlp_daily.closing as xlp",
    "from ko_daily join pep_daily",
    "join spy_daily",
    "join xlp_daily",
    "on ko_daily.tm = pep_daily.tm",
    "and ko_daily.tm = spy_daily.tm",    
    "and ko_daily.tm = xlp_daily.tm"    
);

results <- dbSendQuery(database, stmt);
data <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

X <- as.matrix(data[, -1]);
tm <- data[, 1];
rm(data);

n <- dim(X)[1];
p <- dim(X)[2];

ret <- matrix(NA, nrow=n, ncol=p);
ret[-1, ] <- apply(log(X), MARGIN=2, FUN=diff);

look.back <- 60;
prob.trig <- 0.05;
## expected shortfall of 1 share of the portfolio
ES <- rep(0, n);
deviations <- rep(NA, n);
## Initial capital
cap.init <- 1;
## free cash
cash <- rep(NA, n); 
wealth <- rep(NA, n);
## KO, reserved cash, PEP, SPY, XLP
holding <- matrix(0, nrow=n, ncol=p+1);
purchase <- matrix(0, nrow=n, ncol=p+1);
ratio.cal <- matrix(NA, nrow=n, ncol=p+1);
prob.es <- 0.05;
target.risk <- 0.05;
for (t in (look.back + 1):n) {
    if (is.na(wealth[t-1])) {
        wealth[t-1] = cap.init;
        cash[t-1] <- cap.init;
    }
    
    period <- (t-look.back):(t-1);
    model <- lm(ko~pep+spy+xlp, data=X[period, ]);

    R <- residuals(model);
    F.R <- fit.dist(R);

    shares <- c(1, -coef(model)[-1], -coef(model)[1]);
    ratio.cal[t, ] <- shares;
    deviations[t] <- X[t, 1] - predict(model, newdata=X[t, ], n.ahead=1);
    
    value <- deviations[t];
    ## QRM: CCC-GARCH with diagonal-matrix coefficients
    inno <- matrix(NA, nrow=look.back, ncol=p);
    vol.pred <- rep(NA, p);
    mean.pred <- rep(NA, p);
    for (i in 1:p) {
        mdl <- fit.garch(ret[period+1, i]);
        inno[, i] <- mdl$inno;
        vol.pred[i] <- sum(mdl$alpha * mdl$r.t^2) + sum(mdl$beta * mdl$sigma.t^2) + mdl$omega;
        mean.pred[i] <- mdl$mu;
    }
    C <- cov(inno);

    ## Now compute the portfolio distribution parameters of day t+1
    E <- eigen(C);
    A <- E$vectors %*% diag(sqrt(E$values));
    ## Take the current price of KO as the unit of curreny.
    W <- shares * c(X[t, ], 1) / X[t, 1];
        
    sd.ptfl <- sqrt(sum(((W[1:p] * vol.pred) %*% A)^2));
    mean.ptfl <- sum(mean.pred * W[1:p]);
    ES[t] <- expected.shortfall(c(mean.ptfl, sd.ptfl), alpha=prob.es);
    ## conditional expected loss for one share of the portfolio
    ES[t] <- ES[t] * X[t, 1];

    ## When the deviation is at F^{-1}(pnrom(2)),
    ## our total ES in percentage should be target.risk
    wealth[t] <- sum(X[t, ] * holding[t-1, 1:p]) + holding[t-1, p+1] + cash[t-1];
    pstn.sz.anchor <- target.risk * wealth[t]/ES[t];
    if (F.R$dist == "norm") {
        Q.2s <- F.R$mu + 2 * F.R$sig;
        Q.1s <- F.R$mu + F.R$sig;
    } else if (F.R$dist == "t") {
        Q.2s <- qt(p=pnorm(2), df=F.R$df) + F.R$mu;
        Q.1s <- qt(p=pnorm(1), df=F.R$df) + F.R$mu;
    }
    pstn.sz.const <- pstn.sz.anchor/pstn.sz.shape(Q.2s, F.R);
    pstn.sz.min <- pstn.sz.const * pstn.sz.shape(Q.1s, F.R);
    pstn.sz <- pstn.sz.const * pstn.sz.shape(value, F.R);


    ## As long as we don't exceed the risk limit, adjust the position without further
    ## cash commitment.
    ## Otherwise align the postion to neutrality and adjust its size to the risk limit.
    
    ## adjustment to neutrality without changing KO shares
    ## only use this adjustment if a neutrality adjustment with zero cash flow is impossible
    ## inc.neutral <- rep(NA, p+1);
    ## if (holding[t-1, 1] != 0) {
    ##     inc.neutral[-1] <- shares[-1] * holding[t-1, 1] - holding[t-1, -1];
    ##     inc.neutral[1] <- 0;
    ## } else {## currently neutral on KO
    ##     inc.neutral <- rep(0, p+1);
    ## }
    if (holding[t-1, 1] == 0 && pstn.sz > pstn.sz.min) { ## cash-only
        purchase[t, ] <- shares * pstn.sz * sign(-value);
    } else if (holding[t-1, 1] != 0) {
        pm <- sign(holding[t-1, 1]);
        if (pm != sign(value)) {
            b <- sum(holding[t-1, ] * X[t, ]);
            a <- sum(shares * X[t, ]);
            if (sign(b) == sign(b) && b/a < pstn.sz) {
                ## We can adjust without further cash commitment
                purchase[t, ] <- pm * shares * (b/a) - holding[t-1, ];
            } else if {
                ## Either zero-cash adjustment is impossible or the risk limit is exceeded
                ## Adjust to the recommended position size
                purchase[t, ] <- pm * pstn.sz * shares - holding[t-1, ];
            }
        } else { ## pm == sign(value) The portfolio has shifted to the other side
            purchase[t, ] <- -holding[t-1, 1];
            if (pstn.sz > pstn.sz.min) {
                purchase[t, ] <- purchase[t, ] - sign(value) * pstn.sz * shares;
            }
        }
    }
    holding[t, ] <- holding[t-1, ] + purchase[t, ];
    cash[t] <- cash[t-1] - sum(purchase[t, -2] * X[t, ]) - purchase[t, 2];
    wealth[t] <- cash[t] + sum(holding[t, -2] * X[t, ]) + holding[t, 2];
}

I <- 1370:1400;
par(mfrow=c(3, 1));
plot(I, wealth[I], type="b");
plot(I, deviations[I], type="b");
plot(2:n, ret[1:(n-1)], type="l");

J <- !is.na(deviations);
qqnorm(deviations[J]);



plot(I, wealth[I], type="l");
plot(1:n, cash, type="l");
plot(1:n, loadings[, 1], type="l");
plot(1:n, deviations[, 1], type="l");
