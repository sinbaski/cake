rm(list=ls());
library(abind);
library(RMySQL);
library(ForeCA);
library(MTS);

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
    dbname='avanza', host="localhost");

## currencies <- c(
## "AUD_SEK_Rates",
## "CAD_SEK_Rates",
## "CHF_SEK_Rates",
## "CNY_SEK_Rates",
## "CZK_SEK_Rates",
## "DKK_SEK_Rates",
## "EUR_SEK_Rates",
## "GBP_SEK_Rates",
## "HKD_SEK_Rates",
## "HUF_SEK_Rates",
## "JPY_SEK_Rates",
## "KRW_SEK_Rates",
## "MAD_SEK_Rates",
## "MXN_SEK_Rates",
## "NOK_SEK_Rates",
## "NZD_SEK_Rates",
## "PLN_SEK_Rates",
## "SAR_SEK_Rates",
## "SGD_SEK_Rates",
## "THB_SEK_Rates",
## "TRY_SEK_Rates",
## "USD_SEK_Rates",
## );

currencies <- c(
"AUD_SEK_Rates",
"CAD_SEK_Rates",
"CHF_SEK_Rates",
"CNY_SEK_Rates",
"DKK_SEK_Rates",
"EUR_SEK_Rates",
"GBP_SEK_Rates",
"JPY_SEK_Rates",
"NOK_SEK_Rates",
"USD_SEK_Rates"
);


p <- length(currencies);
prices <- list();

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

ret <- diff(log(prices), lag=5);
n <- dim(ret)[1];
C <- cov(ret);
E <- eigen(C);
eigen.ptfl <- ret %*% E$vectors;

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

