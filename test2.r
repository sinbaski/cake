rm(list=ls());
library(RMySQL);
library(tseries)
library(xts);

source("~/kkasi/r/libxxie.r");
source("~/cake/libeix.r");

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market');

symbols <- c(
    "spy",  ## S&P 500
    ## "dia",  ## Dow Jones
    ## "qqq",  ## Nasdaq
    ## "ezu",  ## Euro zone equities
    "ewg",  ## Germany
    "ewj"  ## Japan
    ## "ewl",  ## Switzerland
    ## "ewn",  ## Netherlands
    ## "ewp",  ## Spain
    ## "ewq",  ## France
    ## "ewu",  ## UK
    ## "uup",  ## USD
    ## "fxb",  ## British pound
    ## "fxc",  ## Canadian dollar
    ## "fxe",  ## euro
    ## "fxy"  ## Japanese yen
    ## "goog", ## Google
    ## "aapl",    ## Apple inc.
    ## "iau",  ## gold
    ## "slv",  ## silver
    ## "uso",  ## US oil fund
    ## "ung"   ## US natural gas fund
    ## "dba",
    ## "jjg",
    ## "corn",
    ## "weat",
    ## "soyb"
    ## "cane"
    ## "nib"
    ## "vxx"  ## SP500 short term volatility
    ## "vixy", ## SP500 short term volatility
    ## "vxz",  ## SP500 mid term volatility
    ## "viix"  ## SP500 short term volatility
);

stmt <- paste(
    "select A.tm, A.closing, B.closing,",
    "C.closing from",
    "spy_daily as A join ewg_daily as B",
    "join ewj_daily as C",
    "on A.tm = B.tm and B.tm = C.tm"
);

results = dbSendQuery(
    database, stmt
);
data <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

qrm <- function(thedate)
{
    p <- dim(prices)[2];
    n <- lookback-1;
    if (use.database) {
        database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                             dbname='market', host="localhost");
    }
    S <- array(NA, dim=c(n+1, p, 3));
    vl <- matrix(NA, n, p);
    ret <- matrix(NA, n, p);
    ## col 1: forecast of the mean return
    ## col 2: forecast of the s.d. of the return
    ## We use univariate models, which don't account for covariances
    ## and hence are inaccurate, but multivariate models are
    ## computationally infeasible due to a large number of parameters.
    prediction <- matrix(NA, nrow=p, ncol=2);
    for (i in 1:p) {
        if (use.database) {
            stmt <- sprintf(paste(
                "select high, low, closing from %s_daily where",
                "datediff('%s', tm) >= 0 order by tm desc limit %d;"),
                symbols[i], thedate, dim(S)[1]);
            rs <- dbSendQuery(database, stmt);
            S[, i, ] <- apply(as.matrix(fetch(rs, n=-1)), MARGIN=2, FUN=rev);
            dbClearResult(rs);
        } else {
            S[, i, ] <- tail(price.data[, , i], n=dim(S)[1]);
        }
        S[-1, i, 1] <- sapply(2:dim(S)[1], FUN=function(j) {
            max(S[j, i, 1], S[j-1, i, 3])
        });
        S[-1, i, 2] <- sapply(2:dim(S)[1], FUN=function(j) {
            min(S[j, i, 2], S[j-1, i, 3])
        });

        vl[, i] <- sapply(2:(n+1), FUN=function(k) {
            log(log(S[k, i, 1]/S[k, i, 2]))
        });
        ret[, i] <- tail(S[, i, 3], n=-1)/head(S[, i, 3], n=-1) - 1;
        ## model.ret <- fit.arma(ret[, i]);
        ## prediction[i, 1] <- predict(model.ret, n.ahead=1)$pred[1];
        ## model.vol <- fit.arma(vl[, i]);
        ## prediction[i, 2] <- exp(predict(model.vol, n.ahead=1)$pred[1]);
        model.ret <- auto.arima(ret[, i]);
        prediction[i, 1] <- forecast(model.ret, h=1)$mean[1];
        model.vol <- auto.arima(vl[, i]);
        prediction[i, 2] <- exp(forecast(model.vol, h=1)$mean[1]);
    }
    if (use.database) {
        dbDisconnect(database);
    }
    C <- cor(ret) * (prediction[, 2] %*% t(prediction[, 2]));
    return(list(mean=prediction[, 1], cov.mtx=C));
}

p <- dim(data)[2] - 1;
prices <- as.matrix(data[, -1]);
R <- apply(data[, -1], MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
R <- rbind(rep(0, p), R);
T <- 20;

assets <- matrix(NA, dim(data)[1], p+1);
assets[1:T, 1:p] <- 0;
assets[1:T, p+1] <- 1;
V <- rep(NA, dim(data)[1]);
V[1:T] <- 1;
for (tm in (T+1):dim(data)[1]) {
    V[tm] <- sum(assets[tm-1, 1:p] * prices[tm, ]) + assets[tm-1, p+1];
    X <- R[(tm-T+1):tm, ];
    C <- cov(X);
    E <- eigen(C);
    Y <- X %*% E$vectors;
    P <- matrix(NA, p, 2);
    for (i in 1:p) {
        model <- auto.arima2(Y[, i]);
        P[i, ] <- forecast2(model);
    }
    alloc <- diag(1/E$values) %*% P[, 1];
    if (sum(abs(alloc)) == 0) {
        assets[tm, ] <- assets[tm-1, ];
        cat(sprintf("On day %d, %s, worth %.4f\n", tm, data[tm, 1], V[tm]));
        next;
    }
    loading <- E$vectors %*% alloc;
    loading <- loading/sum(abs(loading)) * V[tm];
    assets[tm, 1:p] <- loading/prices[tm, ];
    assets[tm, p+1] <- V[tm] - sum(loading);

    cat(sprintf("On day %d, %s, worth %.4f\n", tm, data[tm, 1], V[tm]));
}
n <- dim(data)[1];
ret <- tail(V[(T+1):n], n=-1)/head(V[(T+1):n], n=-1) - 1;
