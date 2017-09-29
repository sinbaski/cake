rm(list=ls())
require(RMySQL);
require(MTS);
source("common/libxxie.r")

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza', host="localhost");

end.date <- '2015-03-07';
len <- 120;
results <- dbSendQuery(database,
                       paste("select * from DJIA order by day")
                       );
records <- fetch(results);
dbClearResult(results);

X <- apply(log(records[, -1]), MARGIN=2, FUN=diff);

t0 <- 1;
t1 <- t0 + len - 1;

model <- refVARMA(model=VARMA(X[t0:t1, ], p=1, q=1));
## VARMA(2, 2)
## aic=  -36.34471 
## bic=  -35.48524 
## VARMA(2, 1)
## aic=  -36.80728 
## bic=  -36.27301 
## VARMA(1, 2)
## aic=  -37.44692 
## bic=  -36.68036 
## VARMA(1, 1)
## aic=  -36.28971 
## bic=  -35.89482 

dbDisconnect(database);
