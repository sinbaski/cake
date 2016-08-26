rm(list=ls());
library("gtrendsR");
library("zoo");
source("common/libxxie.r");
## gconnect("xie.xiaolei@gmail.com", "Wangdan?");
## series <- gtrends("debt", start_date=as.Date("2010-01-01"),
##                   end_date=as.Date("2015-05-27"));
## plot(series);





## Trade stock indices with volume information
Frankfurt <- function(name, day)
{
##    prices <- getAssetPrices(as.Date(day) - 20, day, name, 1, "closing", "localhost");
    prices <- getAssetPrices("2010-01-03", "2015-05-27", name, 1, "closing", "localhost");
    n <- length(prices);

    dev <- computeDeviations(prices, 20);
    

    ## bandwidth <- 2*sd(prices[(n-20+1):n]);
    
    ## Incomplete strategy based on Bollinger bands
    ## Bollinger <- list(lower=0, upper=0, avg=0);
    ## Bollinger$avg = mean(prices[(n-20+1):n]);
    ## Bollinger$upper = Bollinger$mean + bandwidth;
    ## Bollinger$lower = Bollinger$mean - bandwidth;

    ## if (prices[n] > Bollinger$lower && prices[n] < Bollinger$upper)
    ##     return(0);
    ## if (prices[n] <= Bollinger$lower) {
        
    ## }
}
