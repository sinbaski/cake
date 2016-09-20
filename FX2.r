rm(list=ls());
source("/home/lxb353/hdrive/work/r/libxxie.r");

currencies <- c(
    "AUD_SEK_Rates",
    "CAD_SEK_Rates",
    ## "CHF_SEK_Rates",

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
    ## "CHF",

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
    ##"SAR",
    "SGD",
    "USD"
    );

ret <- getAssetReturns("2010-01-04", "2016-04-01",
                       currencies, 1,
                       "rate", "localhost");
n <- dim(ret)[1];
p <- dim(ret)[2];

## E <- eigen(cov(ret));
## D <- eigen(cov(abs(ret)));
## F <- eigen(cov(ret^2));

m <- floor(n/2);
E <- eigen(cov(ret[1:m, ]));
Y <- ret %*% E$vectors;
X1 <- ret[1:m, 1];
model <- lm(X1 ~ Y[(1:m), 1:4]);
Z <- Y[(m+1):n, 1:4] %*% model$coefficients[2:5];
S1 <- cumsum(Z);
S2 <- cumsum(ret[(m+1):n, 1]);
M <- lm(S2 ~ S1);

plot((m+1):n, M$fitted.values, type="l", ylim=c(min(c(S1, S2)), max(c(S1, S2))));
lines((m+1):n, S2, col="#0000FF");

