rm(list=ls());
library(MTS);
library(abind);
library(RMySQL);
library(TTR);
library(zoo);
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

names <- c(
    "AUD",
    "CAD",
    "CHF",

    "CNY",
    "CZK",
    "DKK",

    "EUR",
    "GBP",
    "HKD",

    "HUF",
    "JPY",
    "KRW",

    "MAD",
    "MXN",
    "NOK",

    "NZD",
    ## "PLN_SEK_Rates",
    ## "SAR_SEK_Rates",

    "SGD",
    ## "THB_SEK_Rates",
    ## "TRY_SEK_Rates",

    "USD"
    );


X <- getAssetPrices("2014-01-01", "2016-04-01", currencies, 1, "rate", "localhost");
# Y <- apply(log(X), MARGIN=2, FUN=diff);
n <- dim(X)[1];
m <- floor(n/2);
p <- dim(X)[2];

a <- 6;
b <- 11;
period <- 12;

Y <- EMA(X[, b]/X[, a], period) * X[, a];
dev <- X[, b] - Y;
D <- rollapply(dev, width=10, FUN=sd, align="r", fill=NA);
## T <- which(abs(dev) > 2*D);

position <- list(a=0, b=0, status=0, t=NA);
T <- rep(NA, n);
cash <- 1;
msg <- "";
for (t in min(which(!is.na(D))):n) {
    msg <- sprintf("%d", t);
    if (position$status == 0) {
        if (dev[t] < -2*D[t]) {
            position$status <- 1;
            position$t <- t;
            position$a <- cash/2 / X[t, a];
            position$b <- cash/2 / X[t, b];
            T[t] <- 1;
            msg <- sprintf("dev = %e, Long %s short %s cash %e status %d",
                           dev[t], names[b], names[a], cash, position$status);
        } else if (dev[t] > 2*D[t]){
            position$status <- -1;
            position$t <- t;
            position$a <- cash/2 / X[t, a];
            position$b <- cash/2 / X[t, b];
            T[t] <- 1;
            msg <- sprintf("dev = %e, Long %s short %s cash %e status %d",
                           dev[t], names[a], names[b], cash, position$status);
        }
    } else {
        if (position$status == 1 && dev[t] > 0) {
            position$status <- 0;
            cash <- -position$a * X[t, a] + position$b * X[t, b] + cash;
            T[t] <- 0;
            msg <- sprintf("Close. Cash %e status %d", cash, position$status);
        } else if (position$status == -1 && dev[t] < 0) {
            position$status <- 0;
            cash <- -position$b * X[t, b] + position$a * X[t, a] + cash;
            T[t] <- 0;
            msg <- sprintf("Close. Cash %e status %d", cash, position$status);
        }
    }
    print(msg);
}

plot(1:n, X[, b], col="#0000FF", type="l");
lines(1:n, Y, col="#00FF00");
I1 <- which(T > 0);
I2 <- which(T == 0);
points(I1, X[I1, b]);
points(I2, X[I2, b], lwd=2);
