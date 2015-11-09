rm(list=ls());
library(abind);
library(RMySQL);

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
    dbname='avanza', host="localhost");

currencies <- c(
"AUD_Prices",
"CHF_Prices",
"CNY_Prices",
"DKK_Prices",
"EUR_Prices",
"GBP_Prices",
"HKD_Prices",
"JPY_Prices",
"NOK_Prices",
"USD_Prices"
);

p <- length(currencies);
results <- dbSendQuery(database, "select count(*) from USD_Prices where day >= \"2013-01-01\";");
n <- fetch(results, n=-1)[[1]];
dbClearResult(results);

prices <- matrix(NA, ncol=length(currencies), nrow=n);
for (i in 1:length(currencies)) {
    results <- dbSendQuery(database, sprintf("select price from %s
where day >= '2013-01-01' order by day;", currencies[i]));
    prices[,i] <- fetch(results, n=-1)[[1]];
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
## max.lag <- h-1;
## results <- dbSendQuery(database, "select price from DKK_Prices where day >= '2014-10-01' order by day;" );
## prices <- fetch(results, n=-1)[[1]];
## dbClearResult(results);
dbDisconnect(database);
