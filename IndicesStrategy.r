rm(list=ls());
## library("gtrendsR");
## library("zoo");
## gconnect("xie.xiaolei@gmail.com", "Wangdan?");
## series <- gtrends("debt", start_date=as.Date("2010-01-01"),
##                   end_date=as.Date("2015-05-27"));
## plot(series);

dev.dist <- list(mu=NA, sigma=NA, last.updated=NA);
trader <- list(entry.price=NA, capital=10000, fee=100);

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='avanza', host=host);

Frankfurt <- function(name, day)
{
    look.back.period <- 40;
    dist.period <- 30 * 24;
    one.month <- 30;
    action <- 0;
    
    if (is.na(dev.dist$mu) ||
        as.numeric(as.Date(day) - as.Date(dev.dist$last.updated))
        >= one.month) {
        X <- getAssetPrices(as.Date(day) - dist.period, day, name,
                            1, "closing", "localhost");
        dev <- computeDeviations(X, look.back.period);
        dev.dist$mu <- 0;
        dev.dist$sigma <- sd(dev);
        dev.dist$last.updated <- day;
    }
    results <- dbSendQuery(
        database,
        sprintf("select closing from %s where day <= %s order by day desc limit %d",
                name, day, look.back.period)
    );
    X = rev(fetch(results, n=-1)[[1]]);
    days <- 1 : look.back.period;
    fit <- lm(X ~ days);

    c <- fit$fitted.values[look.back.period];
    a <- trader$fee;
    w <- trader$capital;
    threshold <- 1 - a / (a + w);

    if (X[look.back.period] <= threshold * c)
        action <- 1;
}



dbDisconnect(database);
