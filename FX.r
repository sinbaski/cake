rm(list=ls());
library(abind);
library(RMySQL);
source("common/libxxie.r");

currencies <- c(
    "AUD_SEK_Rates",
    "CAD_SEK_Rates",
    "CHF_SEK_Rates",

    "CNY_SEK_Rates",
    "CZK_SEK_Rates",
    "DKK_SEK_Rates",

    "EUR_SEK_Rates",
    "GBP_SEK_Rates",
    "HKD_SEK_Rates",

    "HUF_SEK_Rates",
    "JPY_SEK_Rates",
    "KRW_SEK_Rates",

    "MAD_SEK_Rates",
    "MXN_SEK_Rates",
    "NOK_SEK_Rates",

    "NZD_SEK_Rates",
    ## "PLN_SEK_Rates",
    ## "SAR_SEK_Rates",

    "SGD_SEK_Rates",
    ## "THB_SEK_Rates",
    ## "TRY_SEK_Rates",

    "USD_SEK_Rates"
    );

ret <- getAssetReturns("2010-01-04", "2016-04-01",
                       currencies, 1,
                       "rate", "31.208.142.23");
n <- dim(ret)[1];

window <- 60;
period <- window/2;
horizon <- 5;
p <- 4;
R <- matrix(NA, window + period, p);
J <- rep(NA, n-window);
K <- rep(NA, n-window);
has.model <- FALSE;
for (t in (window:(n-5))) {
    if ((t - window) %% period == 0 || !has.model) {
        ## We need to refit the model
        q <- p;
        C <- cov(ret[(t-window+1):t, ]);
        E <- eigen(C);
        while (!has.model && q <= 10) {
            tryCatch(
                expr={
                    if (t > window) {
                        k <- ceiling((t - window)/period)*period + window;
                    } else {
                        k <- window + period;
                    }
                    R <- ret[(t-window+1):k, ] %*% E$vectors[, 1:q];
                    model <- ar(R[1:window, ]);
                    if (model$order == 0) {
                        q <- q + 1;
                    } else {
                        has.model <- TRUE;
                    }
                },
                error=function(cond) {
                    message(sprintf("An error occured while fitting the model. t=%d. q=%d\n", t, q));
                    message(cond);
                    q <- q + 1;
                },
                warning=function(cond) {
                    message(sprintf("A warning occured while fitting the model. t=%d\n", t));
                    message(cond);
                    has.model <- TRUE;
                    next;
                }
            );
        }
        if (!has.model) {
            message(sprintf("No AR model is found at t=%d.\n", t));
            next;
        }
    }
    i <- window + (t - window) %% period;
    J[t-window+1] <- R[i+1, 1];
    tryCatch(
        expr={
            K[t-window+1] <- predict(model, newdata=R[(i-model$order+1):i, ])$pred[1];
        },
        error=function(cond) {
            message(sprintf("An error occured while predicting with the model. t=%d", t));
            message(cond);
            break;
        },
        warning=function(cond) {
            message(sprintf("A warning occured while predicting with the model. t=%d", t));
            message(cond);
        }
    )
}



## ## A <- computeCovCorr(eigen.ptfl);
## ## B <- computeCovCorr(ret);

## opt <- eigen.ptfl[,1:4];
## M <- ar(opt);
## M$order
## ## acf(diff(opt, lag=20, differences=2), lag.max=50);

## p <- 4;
## model <- ar(eigen.ptfl[,1:p]);
## L <- 10;
## h <- 4;
## predicted <- array(NA, dim=c(L, p, h));

## for (i in 1:L) {
##     x <- predict(model, newdata=eigen.ptfl[(n-model$order):(n-1), 1:p], n.ahead=h);
##     predicted[i,,] <- t(x$pred);
##     ## new <- eigen.ptfl[n,];
##     ## x <- predict(model, newdata=new, n.ahead=h);
## }

## plot(1:L, predicted[,p,1], type="l", col="#00FF00");
## lines(1:L, eigen.ptfl[(n-L+1):n, p], col="#000000");
