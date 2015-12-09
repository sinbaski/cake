rm(list=ls());
library(abind);
library(RMySQL);
source("libxxie.r");

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
    "PLN_SEK_Rates",
    "SAR_SEK_Rates",

    "SGD_SEK_Rates",
    "THB_SEK_Rates",
    "TRY_SEK_Rates",

    "USD_SEK_Rates"
    );

## currencies <- c(
## "AUD_SEK_Rates",
## "CAD_SEK_Rates",
## "CHF_SEK_Rates",
## "CNY_SEK_Rates",
## "DKK_SEK_Rates",
## "EUR_SEK_Rates",
## "GBP_SEK_Rates",
## "JPY_SEK_Rates",
## "NOK_SEK_Rates",
## "USD_SEK_Rates"
## );


p <- length(currencies);
prices <- list();

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
    dbname='avanza', host="localhost");
for (i in 1:length(currencies)) {
    results <- dbSendQuery(database, sprintf("select rate from %s
where day >= '2013-01-01' order by day;", currencies[i]));
    if (length(prices) == 0) {
        prices <- fetch(results, n=-1)[[1]];
    } else {
        prices <- cbind(prices, fetch(results, n=-1)[[1]]);
    }
    dbClearResult(results);
}
dbDisconnect(database);

ret <- diff(log(prices), lag=1);
## ret <- diff(log(prices[seq(1, dim(prices)[1], by=5),]), lag=1);
n <- dim(ret)[1];
C <- cov(ret);
E <- eigen(C);
eigen.ptfl <- ret %*% E$vectors;
## d <- 5;
## A <- rep(NA, n-d+1);
## for (i in d:n) {
##     A[i-d+1] <- sum(eigen.ptfl[(i-d+1):i]);
## }
pdf("least-variance-ptfl-acf.pdf")
acf(eigen.ptfl[, p], lag.max=50);
dev.off();

pdf("eigenvector_p_comp.pdf")
plot(1:p, E$vectors[, p], main="eigenvector of the smallest eigenvalue",
     xlab="i",
     ylab="Component i");
grid(nx=10);
dev.off();

for (i in 1:6) {
    pdf(sprintf("eigenvector_%d_comp.pdf", i));
    plot(1:p, E$vectors[, i],
         main=sprintf("eigenvector of %dth largest eigenvalue", i),
         xlab="i",
         ylab="Component i");
    grid(nx=10);
    dev.off();
}

pdf("Eigenvalues_of_FX_cov.pdf")
plot(1:p, E$values, main="Eigenvalues of the Covariance Matrix",
     xlab="i", ylab=expression(lambda[i]));
grid(nx=20);
dev.off();



A <- computeCovCorr(eigen.ptfl);
B <- computeCovCorr(ret);

opt <- eigen.ptfl[,p];
acf(diff(opt, lag=20, differences=2), lag.max=50);

model <- ar(eigen.ptfl[,p], order.max=1);



## max.lag <- 60;

## for (i in seq(1, h-1)) {
##     A <- lagged.cor[,,i];
##     B <- A * (A < r[1] | A > r[2]);
##     sink(sprintf("lagged_cor_%d.txt", i));
##     for (j in seq(1:dim(B)[1])) {
##         for (k in seq(1:dim(B)[1])) {
##             if (B[j,k] != 0) {
##                 cat(sprintf("%7.2f", B[j,k]));
##             } else {
##                 cat(sprintf("      0", B[j,k]));
##             }
##         }
##         for (k in 1:5) cat("\n");
##     }
##     sink();
## }

