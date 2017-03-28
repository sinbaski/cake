rm(list=ls())
source("common/libxxie.r")

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza', host="localhost");

end.date <- '2015-03-07';
len <- 25;
results <- dbSendQuery(database,
                       paste("select * from DO_US ",
                             "where day <= '", end.date, "' ",
                             "and day >= date_add('", end.date,
                             "', interval -", len, " week) ",
                             "order by day")
                       );
records <- fetch(results);
dbClearResult(results);

h <- 5;
num <- dim(records)[1];
n <- num - h;


P <- diff(log(records$closing));
V <- diff(log(records$volume));
X <- apply(log(records[, -1]), MARGIN=2, FUN=diff);

results <- dbSendQuery(database,
                       paste("select * from SP500 ",
                             "where day <= '", end.date, "' ",
                             "and day >= date_add('", end.date,
                             "', interval -", len, " week) ",
                             "order by day")
                       );
index <- fetch(results);
dbClearResult(results);

Y <- apply(log(index[, -1]), MARGIN=2, FUN=diff);
## X <- cbind(log(records$high) - log(records$closing),
##            log(records$closing) - log(records$low));
## X <- cbind(X, log(records$volume));

ccf(P, X[, 4], lag.max=20);
ccf(P, Y[, 4], lag.max=20);

## X <- as.matrix(records[1:(n-1), -1]);
model <- lm(P ~ V);
C <- coef(model);
predictions <- C[1] * P

predictions <- as.matrix(records[n:(n+h-1), -1]) %*% C[-1] + C[1];
cbind(records$closing[(n+1):(n+h)], predictions);

dbDisconnect(database);

