rm(list=ls());
library(MTS);
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

window <- 240;
period <- 20;
p <- 4;
J <- rep(NA, n-window);
K <- rep(NA, n-window);
F <- rep(NA, n-window);
price <- 1;
w <- 1;
has.model <- FALSE;
for (t in (window:(n-1))) {
    if ((t - window) %% period == 0 || !has.model) {
        ## We need to refit the model
        q <- p;
        has.model <- FALSE;
        C <- cov(ret[(t-window+1):t, ]);
        E <- eigen(C);
        for (q in p:(p+4)) {
            tryCatch(
                expr={
                    model <- ar(ret[(t-window+1):t, ] %*% E$vectors[, 1:q]);
                    if (model$order > 0) {
                        has.model <- TRUE;
                        break;
                    } else {
                        message(sprintf("Null model at t=%d, q=%d\n", t, q));
                    }
                },
                error=function(cond) {
                    message(sprintf("An error occured while fitting the model. t=%d. q=%d\n", t, q));
                    message(cond);
                }
            );
        }
        if (!has.model) {
            J[t-window+1] <- ret[t+1, ] %*% E$vectors[, w];
            message(sprintf("No AR model is found at t=%d.\n", t));
            next;
        }
    }
    J[t-window+1] <- ret[t+1, ] %*% E$vectors[, w];
    tryCatch(
        expr={
            R <- predict(model,
                         newdata=ret[(t-model$order+1):t, ]
                         %*%
                         E$vectors[, 1:q])$pred;
            K[t-window+1] <- R[w];
            F[t-window+1] <- price*exp(R[w]);
        },
        error=function(cond) {
            message(sprintf("An error occured while predicting with the model. t=%d", t));
            message(cond);
        },
        finally={
            price <- price * exp(ret[t+1, ] %*% E$vectors[, w]);
        }
    );
    
}
I <- which(is.na(K));
K[I] = 0;
F[I] <- F[I-1];

plot(1:length(J), exp(cumsum(J)), type="l");
lines(1:length(F), F, col="#00FF00");
dens <- density(K-J);
plot(dens$x, dens$y, type="l");


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
