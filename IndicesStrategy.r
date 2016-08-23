rm(list=ls());
library("gtrendsR");
library("zoo");
gconnect("xie.xiaolei@gmail.com", "Wangdan?");
series <- gtrends("debt", start_date=as.Date("2010-01-01"),
                  end_date=as.Date("2015-05-27"));
plot(series);

