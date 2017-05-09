rm(list=ls())
require(RMySQL);
require(MTS);
source("common/libxxie.r")

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza', host="localhost");

end.date <- '2015-03-07';
len <- 25;
results <- dbSendQuery(database,
                       paste("select * from SP500 ",
                             "where day <= '", end.date, "' ",
                             "and day >= date_add('", end.date,
                             "', interval -", len, " week) ",
                             "order by day")
                       );
records <- fetch(results);
dbClearResult(results);

X <- apply(log(records[, -1]), MARGIN=2, FUN=diff);

results <- dbSendQuery(database,
                       paste("select * from DJIA ",
                             "where day <= '", end.date, "' ",
                             "and day >= date_add('", end.date,
                             "', interval -", len, " week) ",
                             "order by day")
                       );
index <- fetch(results);
dbClearResult(results);
Y <- apply(log(index[, -1]), MARGIN=2, FUN=diff);

results <- dbSendQuery(database,
                       paste("select * from AAPL_US ",
                             "where day <= '", end.date, "' ",
                             "and day >= date_add('", end.date,
                             "', interval -", len, " week) ",
                             "order by day")
                       );
records <- fetch(results);
dbClearResult(results);
Z <- apply(log(records[, -1]), MARGIN=2, FUN=diff);


## X <- cbind(log(records$high) - log(records$closing),
##            log(records$closing) - log(records$low));
## X <- cbind(X, log(records$volume));

pdf("~/cake/DJIA_HighLow_ccf.pdf")
ccf(Y[, 1], Y[, 2], lag.max=20, col="red", lwd=2,
    main=expression(ccf(bar(X), underline(X))),
    xaxt="n",
    xlab="DJIA",
    ylab="correlation"
    );
axis(side=1, at=seq(from=-20, to=20, by=5));
abline(v=-20:20, lty=3);
dev.off();

## X <- as.matrix(records[1:(n-1), -1]);
## model <- lm(R ~ V);
## C <- coef(model);
## predictions <- C[1] * P

## predictions <- as.matrix(records[n:(n+h-1), -1]) %*% C[-1] + C[1];
## cbind(records$closing[(n+1):(n+h)], predictions);

model <- VARMA(Y, p=2, q=1, include.mean=F);
M <- refVARMA(model, thres=qt(p=0.975, df=dim(Y)[1]-1));
N <- refVARMA(model);
dbDisconnect(database);

