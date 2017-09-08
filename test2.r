rm(list=ls());
library(RMySQL);
library(MASS)
library(xts);

source("common/libxxie.r");

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza');

## stmt <- "select T2.day, T2.closing, (T2.closing - avg(T1.closing))/avg(T1.closing) as deviation from NDA_series_SEK_ST_SE as T2 join NDA_series_SEK_ST_SE as T1 on T2.day > T1.day and datediff(T2.day, T1.day) <= 60 where T2.day between '2014-04-01' and '2014-09-30' group by T2.day;";

stmt <- "select day, price, deviation from Deviations;";

results = dbSendQuery(
    database, stmt
);
D <- fetch(results, n=-1);
dbClearResult(results);

dbDisconnect(database);

X = xts(D$deviation, order.by=as.Date(D$day));
plot(X);

Y <- diff(D$deviation);
plot(xts(Y, order.by=as.Date(D$day[-1])))

## Model X as an Ornstein Uhlenbeck process for simplicity
## Use the sde package. Function dcOU gives the conditional density function of an OU process.
## Even better: Y has heavy-tails. Model X as an Levy process?



## Tdist <- fitdistr(Y, "t");
## Ndist <- fitdistr(Y, "normal");




