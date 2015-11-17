rm(list=ls());
library(abind);
library(RMySQL);
library(dse);

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
    dbname='avanza', host="localhost");

tables <- c(
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
"NOK_SEK_Rates",
"NZD_SEK_Rates",
"PLN_SEK_Rates",
"SAR_SEK_Rates",
"SGD_SEK_Rates",
"THB_SEK_Rates",
"TRY_SEK_Rates",
"USD_SEK_Rates"
);


p <- length(tables);
ret <- list();
for (i in 1:p) {
    results <- dbSendQuery(database, sprintf("select rate from %s order by day;", tables[i]));
    A <- fetch(results, n=-1)[[1]];
    dbClearResult(results);
    A <- (A - mean(A))/sd(A)
    if (length(ret) == 0) {
        ret <- A;
    } else {
        ret <- cbind(ret, A);
    }
}
n <- dim(ret)[1];
E <- list();
for (i in 0:5) {
    A <- t(ret[1:(n-i)]) %*% ret[(1+i):n];
    F <- eigen(t(A) %*% A);
    
}

## days <- list();
## for (i in 1:p) {
##     results <- dbSendQuery(database, sprintf("select day from %s order by day;", tables[i]));
##     A <- fetch(results, n=-1)[[1]];
##     dbClearResult(results);
##     if (length(days) == 0) {
##         days <- A;
##     } else {
##         days <- intersect(days, A);
##     }
## }


