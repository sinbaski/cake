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

p <- length(currencies);

ret <- getAssetReturns("2010-01-01", "2016-04-01",
                       currencies, 5,
                       "rate", "localhost");
n <- dim(ret)[1];

C <- cov(ret);
E <- eigen(C);
eigen.ptfl <- ret %*% E$vectors;

## A <- computeCovCorr(eigen.ptfl);
## B <- computeCovCorr(ret);

opt <- eigen.ptfl[,p];
M <- ar(opt);
## acf(diff(opt, lag=20, differences=2), lag.max=50);

model <- ar(eigen.ptfl);
L <- 50;
h <- 4;
predicted <- array(NA, dim=c(L, p, h));

for (i in 1:L) {
    x <- predict(model, newdata=t(eigen.ptfl[n-L+i-1,]), n.ahead=h);
    predicted[i,,] <- t(x$pred);
    ## new <- eigen.ptfl[n,];
    ## x <- predict(model, newdata=new, n.ahead=h);
}

plot(1:L, predicted[,p,1], type="l", col="#00FF00");
lines(1:L, eigen.ptfl[(n-L+1):n, p], col="#000000");
