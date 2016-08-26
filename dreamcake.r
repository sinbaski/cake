rm(list=ls());
library("TTR");
library("quantmod");
source("~/hdrive/work/r/libxxie.r");

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza', host="localhost");
results <- dbSendQuery(database,
                       "select * from DAX where day between '2012-01-01' and '2015-05-27' order by day;");
data <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

##plot(1:length(data$closing), data$closing, type="l");
candleChart(data, type="candlesticks");
adx <- ADX(data[, c("high", "low", "closing")]);
lines(1:length(data$closing), adx[, "DIp"], col="#00FF00");
lines(1:length(data$closing), adx[, "DIn"], col="#FF0000");
