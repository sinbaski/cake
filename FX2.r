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

w <- n;
E <- eigen(cov(ret));
D <- eigen(cov(abs(ret)));
F <- eigen(cov(ret^2));
## R <- (tail(ret, w) %*% E$vectors);
## E <- eigen(cov(R));
## plot(1:p, (E$values)/sum(E$values));
## grid();
## x11();
pdf("/tmp/FX_eigenvectors.pdf")
par(mfrow=c(3,6));
for (i in 1:p) {
    V <- E$vectors[, i];
    k <- which.max(abs(V));
    V <- V * sign(V[k]);
    plot(1:p, V, main=sprintf("eigenvector[%d]", i),
         xlab="i", ylab=expression(V[i]),
         ylim=c(-1, 1));

    V <- D$vectors[, i];
    k <- which.max(abs(V));
    V <- V * sign(V[k]);
    points(1:p, V, main=sprintf("eigenvector[%d]", i),
           col="#FF0000",
           ylim=c(-1, 1));

    V <- F$vectors[, i];
    k <- which.max(abs(V));
    V <- V * sign(V[k]);
    points(1:p, V, main=sprintf("eigenvector[%d]", i),
           col="#00FF00",
           ylim=c(-1, 1));
    
    grid();
}
dev.off();

## d <- 5;
## ## R <- result$loadings;
## par(mfrow=c(2,5));
## for (i in 1:(d-1)) {
##     for (j in (i+1):d) {
##         ccf(R[, i], R[, j], type="correlation",
##             lag.max=10,
##             main=sprintf("%d & %d, window=%d", i, j, w),
##             ylim=c(-0.18, 0.18));
##         grid();
##     }
## }


## x11();

## par(mfrow=c(2,2));
## for (i in 1:4) {
##     acf(R[, i]);
## }
