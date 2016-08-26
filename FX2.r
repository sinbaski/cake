rm(list=ls());
source("common/libxxie.r");

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

E <- eigen(cov(ret - mean(ret)));
D <- eigen(cov(abs(ret) - mean(abs(ret))));
F <- eigen(cov(ret^2 - mean(ret^2)));

## R <- (tail(ret, w) %*% E$vectors);
## E <- eigen(cov(R));
pdf("/tmp/FX_eigenvalues.pdf", width=14, height=14);
plot(1:p, (E$values)/sum(E$values),
     main=expression(lambda[(i)]/trace),
     ylim=c(0, 0.8),
     xlab=expression(i), ylab="", cex=2, pch=15);
points(1:p, (D$values)/sum(D$values), col="#FF0000", cex=2, pch=16);
points(1:p, (F$values)/sum(F$values), col="#00FF00", cex=2, pch=17);

## points(1:p, (E1$values)/sum(E1$values), col="#FF0000", cex=2, pch=15);
## points(1:p, (D1$values)/sum(D1$values), col="#00FF00", cex=2, pch=16);
## points(1:p, (F1$values)/sum(F1$values), col="#00FF00", cex=2, pch=17);

legend("topright",
       legend=c(expression(X - EX),
       expression(abs(X) - E * abs(X)), expression(X^2 - E * X^2)),
       col=c("#000000", "#FF0000", "#00FF00"),
       pch=c(15, 16, 17), cex=2);
grid();
dev.off();

pdf("/tmp/FX_eigenvectors.pdf", width=20, height=10);
par(mfrow=c(3,6));
for (i in 1:p) {
    V <- E$vectors[, i];
    k <- which.max(abs(V));
    V <- V * sign(V[k]);
    plot(1:p, V, main=sprintf("eigenvector[%d]", i),
         xlab="i", ylab=expression(V[i]),
         ylim=c(-1, 1), pch=15,
         xaxt="n");
    axis(side=1, at=1:p, labels=names, las=2);
    
    V <- D$vectors[, i];
    k <- which.max(abs(V));
    V <- V * sign(V[k]);
    points(1:p, V, main=sprintf("eigenvector[%d]", i),
           col="#FF0000", pch=16);

    V <- F$vectors[, i];
    k <- which.max(abs(V));
    V <- V * sign(V[k]);
    points(1:p, V, main=sprintf("eigenvector[%d]", i),
           col="#00FF00", pch=17);
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
