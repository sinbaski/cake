rm(list=ls());
graphics.off();
require(RMySQL);
source("~/cake/libeix.r");

algo1 <- function(tm, params, holding)
{
    wealth <- holding[1] * prices[tm] + holding[2];

    ret <- sapply(((tm - params$T1 + 1):tm), FUN=function(k) prices[k]/prices[k-1] - 1);
    vol <- sapply(((tm - params$T1 + 1):tm), FUN=function(k) log(highs[k]) - log(lows[k]));
    ret.model <- fit.arma(ret, order.max=c(2,2));
    vol.model <- fit.arma(log(vol), order.max=c(2,2));
    if (length(ret.model$coef) > 0) {
        mu <- predict(ret.model, n.ahead=1)$pred[1];
        sig <- exp(predict(vol.model, n.ahead=1)$pred[1]);
        I <- integrate(
            f=function(x) x * dnorm(x, mean=mu, sd=sig),
            lower=if (mu > 0) -Inf else 0,
            upper=if (mu > 0) 0 else Inf,
            rel.tol=1.0e-2
        );
        score <- mu + params$gda * I$value;
        ## if (sign(score) != sign(mu)) score <- 0;
        ## score <- mu/sig;
    } else {
        score <- 0;
    }
    if (abs(score) > params$score.min) {
        shares <- sign(score) * wealth * params$exposure/prices[tm];
    } else {
        shares <- 0;
    }
    holding[2] <- holding[2] - (shares - holding[1]) * prices[tm];
    holding[1] <- shares;
    return(holding);
}

gen.strat <- function(holding)
{
    T1 <- runif(1, min=15, max=25);
    exposure <- runif(1, min=0.5, max=0.9);
    gda <- runif(1, min=0.05, max=1);
    ## score.min <- runif(1, min=0.5, max=2);
    score.min <- runif(1, min=0.5e-2, max=1.0e-2);
    params <- list(T1=round(T1),
                   gda=gda,
                   score.min=score.min,
                   exposure=exposure
                   );
    return(list(fun=algo1, params=params, holding=holding));
}

log.mutation <- function(x, sig)
{
    stopifnot(min(x) > 0);
    exp(log(x) + rnorm(n=length(x), mean=0, sd=sig));
}

## database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
##                      dbname='market', host="192.168.154.1");
database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");


rs <- dbSendQuery(database, sprintf("select high, low, closing from spy_daily;", "spy"));
Data <- fetch(rs, n=-1);
dbClearResult(rs);
dbDisconnect(database);

prices <- Data$closing;
highs <- Data$high;
lows <- Data$low;
prices.ref <- prices;

sys.holding <- matrix(NA, nrow=length(prices), ncol=2);

t0 <- 25;
V <- rep(NA, length(prices));
strats <- vector("list", length=500);
for (i in 1:length(strats)) {
    holding <- c(0, 1);
    strats[[i]] <- gen.strat(holding);
}

V.max <- rep(length(strats), length(prices));

## require(foreach);
## require(doMC);
## require(sde);
## registerDoMC(detectCores());
## cl <- makeCluster(detectCores());


t1 <- t0;
wealths <- matrix(1, ncol=2, nrow=length(strats));
for (tm in 265:1000) {
    cat(sprintf("At time %d\n", tm));
    holding <- matrix(NA, nrow=length(strats), ncol=2);
    for (i in 1:length(strats)) {
        holding[i, ] <- strats[[i]]$fun(tm, strats[[i]]$params, strats[[i]]$holding);
    }

    ## holding <- foreach(i=1:length(strats), .combine=rbind) %dopar% {
    ##     strats[[i]]$fun(tm, strats[[i]]$params, strats[[i]]$holding);
    ## }
    ## sys.holding[tm, ] <- parApply(cl, holding, MARGIN=2, FUN=sum);

    sys.holding[tm, ] <- apply(holding, MARGIN=2, FUN=sum);
    V[tm] <- sys.holding[tm, 1] * prices[tm] + sys.holding[tm, 2];
    V.max[tm] = if (V[tm] > V.max[tm-1]) V[tm] else V.max[tm-1];
    ret <- sapply((t1+1):tm, FUN=function(k) V[k]/V[k-1] - 1);
    if (tm - t1 >= 5) {
        F <- fit.dist(ret);
        if (F$bic < Inf) {
            prob <- F$fun.p(0);
        } else {
            prob <- max(sum(ret < -2.0e-3)/length(ret), 0.1);
        }
    } else {
        prob <- 0.1;
    }
    flag <- sample(x=c(TRUE, FALSE), size=1, replace=TRUE, prob=c(prob, 1- prob));
    ## flag <- if (tm - t1 == 10) TRUE else FALSE;
    if (!flag) {
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- holding[i, ];
        }
        next;
    }

    ## who survive to the next period?
    wealths[, 2] <- holding[, 1] * prices[tm] + holding[, 2];
    R <- wealths[, 2]/wealths[, 1] - 1;
    mu <- mean(R);
    fitness <- 4^((R - mu)/0.01);
    winners <- sample(1:length(strats), size=length(strats),
                      prob=fitness, replace=TRUE);
    worth <- 0;
    for (i in winners) {
        worth <- worth + sum(holding[i, 1] * prices[tm]) + holding[i, 2];
    }
    k <- sum(wealths[, 2])/worth;
    holding <- holding[winners, ] * k;
    strats <- strats[winners];
    sys.holding[tm, ] <- apply(holding, MARGIN=2, FUN=sum);
    wealths[, 1] <- holding[, 1] * prices[tm] + holding[, 2];

    ## Survivors mutate
    sig <- 0.05 * exp(1 - V[tm]/V[t1]);
    t1 <- tm;
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- holding[i, ];
        l <- round(log.mutation(strats[[i]]$params$T1, sig));
        l <- min(tm, max(l, 5));
        strats[[i]]$params$T1 <- l;
        strats[[i]]$params$exposure <- min(log.mutation(strats[[i]]$params$exposure, sig), 1);
        strats[[i]]$params$gda <- log.mutation(strats[[i]]$params$gda, sig);
        strats[[i]]$params$score.min <- log.mutation(strats[[i]]$params$score.min, sig);
    }
}
## stopCluster(cl);

W <- holding[, 1] * prices[tm] + holding[, 2];
DD <- 1 - V[t0:tm]/V.max[t0:tm];

T1 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1);
exposure <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$exposure);
GDA <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$gda);
score.min <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$score.min);

graphics.off();
par(mfrow=c(2, 1));
plot(t0:tm, prices[t0:tm],
     col="#0000FF",
     type="l", main="V");
abline(v=seq(from=50, by=50, to=tm));
plot(t0:tm, V[t0:tm], col="#FF0000", type="l")
abline(v=seq(from=50, by=50, to=tm));

save(V, t0, t1, tm, holding,
     T1, exposure, GDA, score.min,
     file="spy_trading.RData");




