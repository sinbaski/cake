rm(list=ls());
library(abind);
library(RMySQL);

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
ret <- diff(log(prices));
n <- dim(ret)[1];
C <- cov(ret);
E <- eigen(C);
eigen.ptfl <- ret %*% E$vectors;
v <- sqrt(E$values);
sigma.inv <- (v %*% t(v))^(-1);

cor.conf.ntvl <- function (prob, N) {
    z <- qnorm(prob, mean=0, sd=1/sqrt(N-3));
    x <- exp(2*z);
    return ((x - 1)/(x + 1));
}

max.lag <- 60;
lagged.cov <- array(dim=c(p,p,1));
lagged.cor <- array(dim=c(p,p,1));
r <- cor.conf.ntvl(c(0.01, 0.99), n);
h <- 1;
while (h <= max.lag) {
    A <- matrix(ncol=p, nrow=p);
    for (i in 1:p) {
        for (j in 1:p) {
            A[i, j] <- cov(eigen.ptfl[1:(n-h), i], eigen.ptfl[(1+h):n, j]);
        }
    }
    B <- A * sigma.inv;
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

for (i in seq(1, h-1)) {
    A <- lagged.cor[,,i];
    B <- A * (A < r[1] | A > r[2]);
    sink(sprintf("lagged_cor_%d.txt", i));
    for (j in seq(1:dim(B)[1])) {
        for (k in seq(1:dim(B)[1])) {
            if (B[j,k] != 0) {
                cat(sprintf("%7.2f", B[j,k]));
            } else {
                cat(sprintf("      0", B[j,k]));
            }
        }
        for (k in 1:5) cat("\n");
    }
    sink();
}
## max.lag <- h-1;
## results <- dbSendQuery(database, "select price from DKK_SEK_Rates where day >= '2014-10-01' order by day;" );
## prices <- fetch(results, n=-1)[[1]];
## dbClearResult(results);
dbDisconnect(database);
