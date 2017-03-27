rm(list=ls())
source("common/libxxie.r")

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza', host="localhost");

end.date <- '2015-05-07';
results <- dbSendQuery(database,
                       paste("select * from APA_US ",
                             "where day <= '", end.date, "' ",
                             "and day >= date_add('", end.date,
                             "', interval -24 week)")
                       );
records <- fetch(results);
dbClearResult(results);

n <- dim(records)[1];
series <- records[, -1];
P <- diff(series[, 3]);
Q <- cbind(P, series[-n, ]);
cor(Q);

dbDisconnect(database);

