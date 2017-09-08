rm(list=ls());
graphics.off();
require(RMySQL);
require("xts");


database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza', host="localhost");

stmt <- paste('select T2.day, T2.closing, log(T2.closing) - log(avg(T1.closing)) as dev',
              'from XOM_US as T1 join XOM_US as T2 on datediff(T2.day, T1.day) between 1 and 60',
              'and T2.day > "2010-01-01"',
              'group by T2.day order by T2.day desc;');

results <- dbSendQuery(database, stmt);
series <- fetch(results, n=-1);
dbClearResult(results);

dev <- xts(series$dev, order.by=as.Date(series$day));
plot(dev);
abline(h=0, col='#FF0000');

dbdisconnect(database);


