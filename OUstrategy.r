rm(list=ls());
graphics.off();
require(RMySQL);
require(ts);
require("xts");
require("fGarch");
require("sde");
require("zoo");
require("TTR");
source("libeix.r");


## Assume the deviations are normally distributed
## Trade size increases exponentially with the deviation
## Determine the rate of exponential growth such that the
## expected trade size is a fixed number for a given threshold.
expected.trade.size <- function(lambda, sig, threshold)
{
    -exp(lambda^2 * sig^2 / 2) * (
        erf((threshold - lambda * sig^2)/sqrt(2 * sig)) - 1
    )/2
}

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

X <- data[, -1];
tm <- data[, 1];
rm(data);

n <- dim(X)[1];
p <- dim(X)[2];

ret <- matrix(NA, nrow=n, ncol=p);
ret[-1, ] <- apply(log(X), MARGIN=2, FUN=diff);

look.back <- 60;
prob.trig <- 0.05;
## free cash
cash <- rep(0, n); 
wealth <- rep(0, nrow=n, ncol=p);
deviations <- rep(NA, n);
## KO, reserved cash, PEP, SPY, XLP
holding <- matrix(0, nrow=n, ncol=p+1);
purchase <- matrix(0, nrow=n, ncol=p+1);
ratio.cal <- matrix(NA, nrow=n, ncol=p+1);
size.target <- 1;
ccc.garch <- vector("list", p);
confidence.risk <- 0.95;
limit.risk <- 5.0e-2;
allocation <- 1;
for (t in (2*look.back + 1):n) {
    period <- (t-look.back):(t-1);
    model <- lm(ko~pep+spy+xlp, data=X[period, ]);

    R <- residuals(model);
    sig <- sd(R);
    F <- ecdf(R);
    ## Find the curve of trade size
    ub <- 2;
    lb <- 0.5;
    threshold <- -quantile(F, prob.trig);
    while (expected.size.target(ub, sig, threshold) < size.target)
        ub <- ub * 2;
    while (expected.size.target(lb, sig, threshold) > size.target)
        lb <- lb / 2;
    lambda <- uniroot(function(x) expected.trade.size(x, sig=sig, threshold=threshold) - size.target, c(lb, ub))$root;
    
    shares <- c(1, -coef(model));
    ratio.cal[t, ] <- shares;
    deviations[t] <- X[t, 1] - predict(model, newdata=X[t, ], n.ahead=1);
    
    value <- deviations[t];
    size.cur <- exp(lambda * value);

    ## QRM: CCC-GARCH with diagonal-matrix coefficients
    inno <- matrix(NA, nrow=look.back, ncol=p);
    vol.pred <- rep(NA, p);
    mean.pred <- rep(NA, p);
    for (i in 1:p) {
        ccc.garch[[i]] <- garchFit(formula=~garch(1,1),
                                   ret[period+1, i],
                                   include.mean=TRUE,
                                   trace=FALSE);
        inno[, i] <- ret[period + 1, i]/ccc.garch[[i]]@sigma.t;
        vol.pred[i] <- predict(ccc.garch[[i]], n.ahead=1)$standardDeviation;
        mean.pred[i] <- coef(ccc.garch[[i]])[1];
    }
    C <- cov(inno);

    ## Now compute the portfolio distribution parameters of day t+1

    E <- eigen(C);
    A <- E$vectors %*% diag(sqrt(E$values));
    ## W <- abs(holding[t-1, -2]) * X[t-1, ];
    W <- abs(shares) * X[t, ];
    W <- W/sum(W);

    sd.ptfl <- sqrt(sum(((W * vol.pred) %*% A)^2));
    mean.ptfl <- sum(mean.pred * W);
    VaR <- qnorm(p=confidence.risk, mean=mean.ptfl, sd=sd.ptfl);
    ## expected shortfall
    ES <- integrate(f=function(x) x * dnorm(x, mean=mean.ptfl, sd=sd.ptfl), lower=VaR, upper=Inf)$value/(1 - confidence.risk);
    
    ## adjustment to neutrality without changing KO shares
    ## only use this adjustment if a neutrality adjustment with zero cash flow is impossible
    inc.neutral <- rep(NA, p+1);
    if (holding[t-1, 1] != 0) {
        inc.neutral[-1] <- shares[-1] * holding[t-1, 1] - holding[t-1, -1];
        inc.neutral[1] <- 0;
    } else {## currently neutral on KO
        inc.neutral <- rep(0, p+1);
    }    

    if (holding[t-1, 1] > 0) {
        if (value < quantile(F, prob.trig)) {
            ## bottom: Long KO
            purchase[t, ] <- inc.neutral + shares;
        } else if (value > quantile(F, 0.5 - prob.trig) && value <= quantile(F, 1 - prob.trig)) {
            ## middle and upper: clear the portfolio, ignoring the neutrality increment
            purchase[t, ] <- -holding[t-1, ];
        } else if (value > quantile(F, 1 - prob.trig)) {
            ## top: clear the portfolio and take the short KO position
            purchase[t, ] <- -holding[t-1, ] - shares;
        } else { ## value > quantile(F, prob.trig) && value < quantile(F, 0.5 - prob.trig)
            ## lower: adjust the portfolio
            ## NOTE: value < 0
            intercept <- sum(X[t, -1] * tail(ratio.cal[t-1, ], n=-2)) - ratio.cal[t, 2] + ratio.cal[t-1, 2];
            if (sign(intercept) < 0) {
                k <- intercept / value;
                purchase[t, ] <- k * shares - holding[t-1, ];
            } else if (sign(intercept) >= 0) {
                ## Adjust to market neutrality
                purchase[t, ] <- inc.neutral;
            }
        }
    } else if (holding[t-1, 1] < 0) {    ## currently short on KO
        if (value > quantile(F, 1 - prob.trig)) {
            ## top: short KO
            purchase[t, ] <- inc.neutral - shares;
        } else if (value < quantile(F, 0.5 + prob.trig) && value > quantile(F, prob.trig)) {
            ## middle and lower: clear the portfolio, ignoring the neutrality increment
            purchase[t, ] <- -holding[t-1, ];
        } else if (value < quantile(F, prob.trig)) {
            ## bottom: clear the portfolio and take the long KO position
            purchase[t, ] <- -holding[t-1, ] + shares;
        } else { ## value > quantile(F, prob.trig) && value > quantile(F, 0.5 + prob.trig)
            ## upper: adjust the portfolio
            ## NOTE: value > 0
            intercept <- sum(X[t, -1] * tail(ratio.cal[t-1, ], n=-2)) - ratio.cal[t, 2] + ratio.cal[t-1, 2];
            if (sign(intercept) > 0) {
                k <- intercept / value;
                purchase[t, ] <- -k * shares - holding[t-1, ];
            } else if (sign(intercept) <= 0) {
                ## Adjust to market neutrality
                purchase[t, ] <- inc.neutral;
            }
        }
    } else { ## cash-only
        if (value < quantile(F, prob.trig)) {
            ## bottom: Long KO
            purchase[t, ] <- shares;
        } else if (value > quantile(F, 1 - prob.trig)) {
            ## top: short KO
            purchase[t, ] <- -shares;
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
