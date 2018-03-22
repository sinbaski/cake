rm(list=ls());
graphics.off();
require(RMySQL);
require(abind);
source("./libeix.r");

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");
rs <- dbSendQuery(database, "select * from trade_log order by tm;");
data <- fetch(rs, n=-1);
dbClearResult(rs);
dbDisconnect(database);

days <- data$tm;
records <- as.matrix(data[, c(-1, -2)]);
cols <- seq(from=1, by=2, to=dim(records)[2]-1);
cash <- rep(NA, dim(records)[1]);
V <- rep(NA, dim(records)[1]);
V[1] <- 1;
cash[1] <- V[1] - sum(records[1, cols+1]);
for (k in 2:dim(records)[1]) {
    V[k] <- cash[k-1] +
        sum(records[k-1, cols+1] * (records[k, cols]/records[k-1, cols]));
    cash[k] <- V[k] - sum(records[k, cols+1]);
}
ret <-tail(V, n=-1)/head(V, n=-1) - 1;
sharpe <- mean(ret)/sd(ret);
V.max <- cummax(V);
DD <- 1 - V/V.max;
## turnover <- sum(abs(
##     tail(records[, cols+1], n=-1) - head(records[, cols+1], n=-1)
## ));
turnover <- sapply(2:dim(records)[1], FUN=function(k) {
    x <- sum(abs(records[k, cols+1] - records[k-1, cols+1]*records[k, cols]/records[k-1, cols]));
    x/V[k];
});

## database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
##                      dbname='market', host="localhost");
## stem <- "update trade_log";
## for (k in 2:dim(records)[1]) {
##     stmt <- paste(
##         stem,
##         sprintf("set worth=%e where tm=\"%s\";", V[k], days[k])
##     );
##     rs <- dbSendQuery(database, stmt);
##     dbClearResult(rs);
## }
## dbDisconnect(database);
