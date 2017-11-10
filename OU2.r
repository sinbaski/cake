rm(list=ls());
graphics.off();
require(RMySQL);
require("xts");
require("fGarch");
require("sde");
require("zoo");
require("TTR");

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");


stmt <- paste(
    "select ko_daily.tm as tm,", 
    "ko_daily.closing as ko,",
    "ko_daily.high - ko_daily.low ko_range,",
    "spy_daily.closing as spy,",
    "spy_daily.high - spy_daily.low as spy_range,",
    "xlp_daily.closing as xlp,",
    "xlp_daily.high - xlp_daily.low as xlp_range",
    "from ko_daily join spy_daily join xlp_daily",
    "on ko_daily.tm = spy_daily.tm and ko_daily.tm = xlp_daily.tm;"
);
results <- dbSendQuery(database, stmt);
X <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

n <- dim(X)[1];
look.back <- 60;
ma.t <- 10;
action <- {};
trades <- {};
prob.trig <- 0.05;
holding <- rep(0, 3);
cash <- rep(0, n);
wealth <- rep(0, n);
position <- 0;
for (t in (look.back + 1):(n-50)) {
## for (t in (look.back + 1):73) {
    model <- lm(ko~spy+xlp-1, data=X[(t-look.back):(t-1), c(2,4,6)]);
    Y <- residuals(model);
    EY <- EMA(Y, n=ma.t)
    F <- ecdf(Y - EY);
    value <- X[t, 2] - predict(model, newdata=X[t, c(4,6)], n.ahead=1);
    prob <- F(value);

    if ((prob > 1 - prob.trig && position %in% c(-1, 0)) || (prob > 0.45 && position == 1)) {
        ## Short the portfolio
        action <- rbind(action, c(t, -1));
        if (position %in% c(0, -1)) {## Not in a position or in a short position
            holding <- holding - c(1, -coef(model));
            cash[t] <- cash[t-1] + sum(c(1, -coef(model)) * X[t, c(2,4,6)]);
            position <- -1;
        } else if (position == 1) {## In a long position
            cash[t] <- cash[t-1] + sum(holding * X[t, c(2,4,6)]);
            holding <- rep(0, 3);
            position <- 0;
        }
    }  else if ((prob < prob.trig && position %in% c(0, 1)) || (prob < 0.55 && position == -1)) {
        ## Long the portfolio
        action <- rbind(action, c(t, 1));
        if (position %in% c(0, 1)) {
            ## Not in a position or in a long position
            holding <- holding + c(1, -coef(model));
            cash[t] <- cash[t-1] - sum(c(1, -coef(model)) * X[t, c(2,4,6)]);
            position <- 1;
        } else {
            ## In a short position
            cash[t] <- cash[t-1] - sum(holding * X[t, c(2,4,6)]);
            holding <- rep(0, 3);
            position <- 0;
        }
    }
    else {
        cash[t] <- cash[t-1];
    }
    wealth[t] <- cash[t] + sum(holding * X[t, c(2,4,6)]);
}


long.days <- which(action[, 2] == 1);
short.days <- which(action[, 2] == -1);
W <- xts(wealth[(look.back+1):t], order.by=as.Date(X$tm[(look.back+1):t]));
dW <- diff(wealth[(look.back+1):t]);
qqnorm(dW/sd(dW));

par(mfrow=c(4,1));
plot(W);
ko <- xts(X$ko[(look.back+1):t], order.by=as.Date(X$tm[(look.back+1):t]));
plot(ko);
spy <- xts(X$spy[(look.back+1):t], order.by=as.Date(X$tm[(look.back+1):t]));
plot(spy);
xlp <- xts(X$spy[(look.back+1):t], order.by=as.Date(X$tm[(look.back+1):t]));
plot(xlp);
## plot(1:t, wealth[1:t], type='l');
