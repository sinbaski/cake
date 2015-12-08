library(RMySQL);
library(abind);
source("innovations-algorithm.R");

###
# A matrix of return values derived from price data on common days
# of all stocks in the given tables.
### 
getAssetReturns <- function(day1, day2, tables) {
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
        dbname='avanza', host=Sys.getenv("PB"));
    days <- vector('character');
    for (i in 1:length(tables)) {
        results <- dbSendQuery(
            database,
            sprintf("select distinct day from %s where
day between '%s' and '%s' order by day;", tables[i], day1, day2));
        D = fetch(results, n=-1)[[1]];
        dbClearResult(results);
        if (length(days) > 0) {
            days <- intersect(days, D);
        } else {
            days <- D;
        }
    }
    n.days = length(days);
    R = matrix(nrow=n.days-1, ncol=length(tables));
    str = sprintf("'%s'", days[1]);
    for (i in 2 : n.days) {
        str = sprintf("%s, '%s'", str, days[i]);
    }

    for (i in 1:length(tables)) {
        results <- dbSendQuery(database, sprintf("select closing from
%s where day in (%s) order by day;", tables[i], str));
        prices <- fetch(results, n=-1)[[1]];
        R[,i] <- diff(log(prices));
    }
    dbDisconnect(database);
    return (data.frame(days[-1], R));
}

### Given a vector of observations, infer the innovations
inferInnovations <- function(X) {
    p <- length(X);
    acov <- acf(X, type="covariance", plot=FALSE)$acf;
    acov <- append(acov, rep(0, length(X) - length(acov) + 1));
    InnAlgo <- innovations.algorithm(acov);
    inno <- rep(NA, p);
    inno[1] <- X[1];
    for (i in 1:p-1) {
        inno[i+1] <- X[i+1] - sum(InnAlgo$thetas[[p]][1:i]*rev(inno[1:i]));
    }
    return(inno);
}

getInterpolatedReturns <- function(day1, day2, assetSet) {
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
        dbname='avanza', host="localhost");
    results = dbSendQuery(database, sprintf("select symbol from %s;",
        assetSet));
    tables <- fetch(results, n=-1)[[1]];
    dbClearResult(results);
    n.stocks <- length(tables);
    suffix <- "";
    if (assetSet == "SP500_components") {
        suffix <- "_US";
    }

    results <- dbSendQuery(
        database,
        sprintf("select count(*) from Calendar where weekday(day) <= 4
and day between '%s' and '%s';", day1, day2));
    n.records <- fetch(results, n=1)[1, 1];
    dbClearResult(results);

    to.include <- rep(TRUE, n.stocks);
    for (i in 1 : n.stocks) {
        tables[i] <- gsub("[.]", "_", tables[i]);
        tables[i] <- gsub("-", "_series_", tables[i]);
        tables[i] <- paste(tables[i], suffix, sep="");

        results <- dbSendQuery(
            database,
            sprintf("select count(*) from %s where
day between '%s' and '%s';", tables[i], day1, day2)
            );
        n.traded.days <- fetch(results)[1,1];
        dbClearResult(results);

        results <- dbSendQuery(
            database,
            sprintf("select min(day) from %s;",
                    tables[i])
            );
        d <- fetch(results)[1, 1];
        dbClearResult(results);

        if (n.traded.days < 1000 || d > day1) {
            to.include[i] <- FALSE;
            next;
        }

    }
    p <- sum(to.include);
    prices <- matrix(NA, nrow=n.records, ncol=p);

    results <- dbSendQuery(
        database,
        sprintf("select day from Calendar where weekday(day) <= 4
and day between '%s' and '%s';", day1, day2)
        );
    days <- fetch(results, n=-1)[[1]];
    dbClearResult(results);

    stocks.included <- which(to.include);
    for (i in 1:length(stocks.included)) {
        results <- dbSendQuery(
            database,
            sprintf("select day, closing from %s
where day between '%s' and '%s'",
                    tables[stocks.included[i]],
                    day1, day2)
            );
        A <- fetch(results, n=-1);
        dbClearResult(results);
        I <- days %in% A[[1]];
        prices[which(I), i] <- A[[2]];

        if (is.na(prices[1, i])) {
            ## get the last trading day before the period
            results <- dbSendQuery(
                database,
                sprintf("select max(day) from %s where day <= '%s';",
                        tables[stocks.included[i]], day1)
                );
            d <- fetch(results)[1, 1];
            dbClearResult(results);

            ## get the closing price of the last trading day
            results <- dbSendQuery(
                database,
                sprintf("select closing from %s where day = '%s';",
                        tables[stocks.included[i]], d)
                )
            last.price <- fetch(results)[1, 1];
            dbClearResult(results);

            ## fill in the price up to the first available
            j <- 1;
            while (is.na(prices[j, i])) {
                prices[j, i] <- last.price;
                I[j] <- TRUE;
                j <- j + 1;
            }
        }
        J <- which(!I);
        for (j in J) {
            prices[j, i] <- prices[j-1, i];
        }
    }
    dbDisconnect(database);
    R <- diff(log(prices));
}

estimateTailIndices <- function(ret) {
    if (class(ret) == "matrix") {
        tailIndices = matrix(NA, nrow=dim(ret)[2], ncol=2);
        for (i in 1:dim(ret)[2]) {
            X = ret[, i];
            a <- quantile(X, probs=0.03);
            tailIndices[i, 1] <- 1/mean(log(X[which(X < a)]/a));

            b <- quantile(X, probs=0.97);
            tailIndices[i, 2] <- 1/mean(log(X[which(X > b)]/b));
        }
    } else if (class(ret) == "numeric") {
        tailIndices = rep(NA, 2);
        a <- quantile(ret, probs=0.03);
        tailIndices[1] <- 1/mean(log(ret[which(ret < a)]/a));

        b <- quantile(ret, probs=0.97);
        tailIndices[2] <- 1/mean(log(ret[which(ret > b)]/b));
    }
    return(tailIndices);
}

cor.conf.ntvl <- function (prob, N) {
    z <- qnorm(prob, mean=0, sd=1/sqrt(N-3));
    x <- exp(2*z);
    return ((x - 1)/(x + 1));
}

computeCovCorr <- function(data, max.lag=60) {
    lagged.cov <- array(dim=c(p,p,1));
    lagged.cor <- array(dim=c(p,p,1));
    r <- cor.conf.ntvl(c(0.01, 0.99), n);
    h <- 1;
    sigma <- apply(data, 2, "sd");
    sigma.inv <- sigma %o% sigma;
    while (h <= max.lag) {
        A <- matrix(ncol=p, nrow=p);
        for (i in 1:p) {
            for (j in 1:p) {
                A[i, j] <- cov(data[1:(n-h), i], data[(1+h):n, j]);
            }
        }
        B <- A / sigma.inv;
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
    return (list(lagged.cov=lagged.cov, lagged.cor=lagged.cor));
}

largeComp <- function(data, level) {
    return(data * (abs(data) > level));
}
