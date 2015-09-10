library(RMySQL);
source("innovations-algorithm.r");

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

getInterpolatedReturns <- function(day1, day2, tables) {
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
        dbname='avanza', host=Sys.getenv("PB"));
    results <- dbSendQuery(
        database,
        sprintf(paste("select day from Calendar where weekday(day) <= 4",
                      "and day between '%s' and '%s';"), day1, day2)
    );
    days <- fetch(results, n=-1)[[1]];
    dbClearResult(results);
    p <- length(tables);
    n <- length(days);
    prices <- matrix(NA, nrow=n, ncol=p);
    for (i in 1:p) {
        results <- dbSendQuery(
            database,
            sprintf(paste("select day, closing from %s where",
                          "day between '%s' and '%s'"),
                    tables[i], day1, day2)
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
                        tables[i], day1)
            );
            d <- fetch(results)[1, 1];
            dbClearResult(results);

            ## get the closing price of the last trading day
            results <- dbSendQuery(
                database,
                sprintf("select closing from %s where day = '%s';",
                        tables[i], d)
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
    return(diff(log(prices)));
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
