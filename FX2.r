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

    ## "MAD_SEK_Rates",
    "MXN_SEK_Rates",
    "NOK_SEK_Rates",

    "NZD_SEK_Rates",
    ## "PLN_SEK_Rates",
    "SAR_SEK_Rates",

    "SGD_SEK_Rates",
    ## "THB_SEK_Rates",
    ## "TRY_SEK_Rates",

    "USD_SEK_Rates"
    );

ret <- getAssetReturns("2010-01-04", "2016-04-01",
                       currencies, 1,
                       "rate", "localhost");
n <- dim(ret)[1];
p <- dim(ret)[2];

w <- 360;
C <- cov(ret[(n-w+1):n, ]);
E <- eigen(C);

R <- ret[(n-w+1):n, ] %*% E$vectors;
E <- eigen(cov(R));

d <- 5;
## R <- result$loadings;
par(mfrow=c(2,5));
for (i in 1:(d-1)) {
    for (j in (i+1):d) {
        ccf(R[, i], R[, j], lag.max=20, type="correlation",
            main=sprintf("%d & %d, window=%d", i, j, w),
            ylim=c(-0.18, 0.18));
        grid();
    }
}
x11();

par(mfrow=c(2,2));
for (i in 1:p) {
    acf(R[, i]);
}
