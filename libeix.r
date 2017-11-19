erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1;

fit.OU <- function(ts)
{
    minus.log.lik <- function(theta, sig)
    {
        -sum(dcOU(x=tail(ts, n=-1), Dt=1, x0=head(ts, n=-1), theta=c(0, theta, sig), log=TRUE))
    }
    theta.init <- -log(mean(tail(ts, n=-1)/head(ts, n=-1)));
    sig.init <- sd(diff(R));
    params <- optim(par=c(theta.init, sig.init),
                    fn=function(arg) minus.log.lik(arg[1], arg[2]),
                    method="L-BFGS-B",
                    lower=c(theta.init/10, sig.init/10),
                    upper=c(theta.init*2, sig.init*2),
                    );
    return(params$par);
}




## compute.volatility <- function(symbol)
## {
##     database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
##                          dbname='market', host="localhost");

##     stmt <- sprintf(
##         paste(
##         "create table %s_vol (",
##         "tm date primary key,",
##         "realized double,",
##         "highlow double",
##         ");"
##         ), symbol
##     );
##     results <- dbSendQuery(database, stmt);    

##     stmt <- sprintf(
##         "select distinct(date(tm)) as t from %s_1min;", symbol
##     );
##     results <- dbSendQuery(database, stmt);
##     days <- fetch(results, n=-1)$t;
##     dbClearResult(results);

##     vol.realized <- rep(NA, length(days));
##     vol.range <- rep(NA, length(days));
##     for (t in 2:length(days)) {
##         stmt <- sprintf(
##             "select closing from %s_1min where date(tm) = \"%s\" order by tm;", symbol, days[t]
##         );
##         results <- dbSendQuery(database, stmt);
##         X <- fetch(results, n=-1);
##         dbClearResult(results);
##         ret <- diff(log(X$closing));
##         vol.realized[t] <- sqrt(sum(ret^2));

##         stmt <- sprintf(
##             "select max(high) as high, min(low) as low from %s_1min where date(tm) = \"%s\";", symbol, days[t]
##         );
##         results <- dbSendQuery(database, stmt);
##         X <- fetch(results, n=-1);
##         dbClearResult(results);
##         high <- X$high;
##         low <- X$low;

##         stmt <- sprintf(
##             paste(
##                 "select closing from %s_1min where tm = ",
##                 "(select tm from %s_1min where date(tm) < \"%s\" order by tm desc limit 1);"
##             ), symbol, symbol, days[t]
##         );
##         results <- dbSendQuery(database, stmt);
##         closing <- fetch(results, n=-1)$closing;
##         dbClearResult(results);

##         vol.range[t] <- log(max(high, closing)) - log(min(low, closing));
##     }
##     vol <- cbind(vol.realized[-1], vol.range[-1]);
##     write.table(vol, sprintf("/tmp/%s_vol.txt", symbol), sep=",",
##                 row.names=FALSE, col.names=FALSE);
##     stmt <- sprintf(
##         paste(
##             "load data infile '/tmp/%s_vol.txt' into table %s_vol",
##             "fields terminated by \",\";"
##         ), symbol, symbol
##     );
##     results <- dbSendQuery(database, stmt);
##     dbDisconnect(database);
##     return(list(realized=vol.realized, range=vol.range));
## }
