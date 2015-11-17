rm(list=ls());
library(abind);
library(RMySQL);
library(dse);

cor.conf.ntvl <- function (prob, N) {
    z <- qnorm(prob, mean=0, sd=1/sqrt(N-3));
    x <- exp(2*z);
    return ((x - 1)/(x + 1));
}
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
sigmas <- rep(NA, p);
for (i in 1:p) {
    results <- dbSendQuery(database, sprintf("select rate from %s order by day;", tables[i]));
    A <- fetch(results, n=-1)[[1]];
    dbClearResult(results);
    A <- diff(log(A));
    
    ##    A <- (A - mean(A))/sd(A)
    if (length(ret) == 0) {
        ret <- A;
    } else {
        ret <- cbind(ret, A);
    }
    sigmas[i] <- sd(A);
}
n <- dim(ret)[1];
dbDisconnect(database);

max.lag <- 60;
lagged.cov <- array(dim=c(p,p,1));
lagged.cor <- array(dim=c(p,p,1));
r <- cor.conf.ntvl(c(0.01, 0.99), n);
h <- 1;
while (h <= max.lag) {
    A <- matrix(ncol=p, nrow=p);
    for (i in 1:p) {
        for (j in 1:p) {
            A[i, j] <- cov(ret[1:(n-h), i], ret[(1+h):n, j]);
        }
    }
    B <- A / sigmas;
    if (min(B) > r[1] && max(B) < r[2]) {
        break;
    }
    if (h==1) {
        lagged.cov[,,1] <- A;
        lagged.cor[,,1] <- B;
    } else {
        lagged.cov <- abind(lagged.cov, A, along=3);
        lagged.cor <- abind(lagged.cor, B, along=3);
    }
    h <- h + 1;
}

ts <- TSdata(input=ret);

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


