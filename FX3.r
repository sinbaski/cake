rm(list=ls());
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

## E <- eigen(cov(ret[1:120, ]));
## R <- ret[121:n, ] %*% E$vectors[, 1];
## plot(1:(n-120), exp(cumsum(R)), type="l");

period <- 30;
w <- length(currencies);
K <- rep(NA, n);
Q <- rep(NA, n);

d <- floor(n/period);
M <- array(NA, dim=c(length(currencies), length(currencies), d));
wealth <- 1;
for (t in 1:(n-period+1)) {
    if (t %% period == 1) {
        C <- cov(ret[t:(t+period-1), ]);
        E <- eigen(C);
        M[,,ceiling(t/period)] <- E$vectors;
    }
    X = ret[t:(t+period-1), ] %*% E$vectors[, w];
    Q[t:(t+period-1)] = exp(cumsum(X))*wealth;
    wealth <- Q[t+period-1];
}

plot(1:n, Q, type="l");

colors <- terrain.colors(n=d);
for (t in (1+(0:9)*5)) {
    if (t == 1) {
        plot(1:length(currencies), M[,1,t], type="p", pch=15, cex=2, col=colors[t]);
    }
    points(1:length(currencies), M[,1,t], pch=15, cex=2, col=colors[t]);
}
