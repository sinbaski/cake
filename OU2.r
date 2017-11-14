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


## stmt <- paste(
##     "select ko_daily.tm as tm,", 
##     "ko_daily.closing as ko,",
##     "ko_daily.high - ko_daily.low ko_range,",
##     "spy_daily.closing as spy,",
##     "spy_daily.high - spy_daily.low as spy_range,",
##     "xlp_daily.closing as xlp,",
##     "xlp_daily.high - xlp_daily.low as xlp_range",
##     "from ko_daily join spy_daily join xlp_daily",
##     "on ko_daily.tm = spy_daily.tm and ko_daily.tm = xlp_daily.tm;"
## );
stmt <- paste(
    "select ko_daily.tm as tm,",
    "ko_daily.closing as ko,",
    "pep_daily.closing as pep,",
    "spy_daily.closing as spy,",
    "xlp_daily.closing as xlp",
    "from ko_daily join pep_daily",
    "join spy_daily",
    "join xlp_daily",
    "on ko_daily.tm = pep_daily.tm",
    "and ko_daily.tm = spy_daily.tm",    
    "and ko_daily.tm = xlp_daily.tm"    
);

results <- dbSendQuery(database, stmt);
X <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

n <- dim(X)[1];
p <- dim(X)[2]-1;
look.back <- 60;
ma.t <- 10;
action <- {};
trades <- {};
prob.trig <- 0.05;
holding <- matrix(NA, nrow=n, ncol=p);
cash <- matrix(NA, nrow=n, ncol=p);
wealth <- matrix(NA, nrow=n, ncol=p);
position <- 0;
trading.allowed <- NA;
deviations <- rep(NA, n);
ratio.hold <- matrix(NA, nrow=n, ncol=p);
ratio.cal <- matrix(NA, nrow=n, ncol=p);
for (t in (look.back + 1):n) {
    ## for (t in (look.back + 1):73) {
    period <- (t-look.back):(t-1);
    prices <- X[period, ];
    ## model.vol.ko <- garchFit(~garch(1,1), data=ret[, 1], trace=FALSE);
    ## model.vol.spy <- garchFit(~garch(1,1), data=ret[, 2], trace=FALSE);
    ## model.vol.xlp <- garchFit(~garch(1,1), data=ret[, 3], trace=FALSE);
    ## sigma <- c(
    ##     tail(model.vol.ko@sigma.t, 1),
    ##     tail(model.vol.spy@sigma.t, 1),
    ##     tail(model.vol.xlp@sigma.t, 1)
    ## );
    ## vol.params <- matrix(
    ##     c(coef(model.vol.ko), coef(model.vol.spy), coef(model.vol.xlp)),
    ##     ncol=4, nrow=3, byrow=TRUE
    ## )[, 2:4];
    ## newdata <- matrix(c(rep(1, 3), tail(ret, n=1), sigma), nrow=3, ncol=3,
    ##                   byrow=FALSE);
    ## vol.t <- sqrt(apply(vol.params * newdata^2, MARGIN=1, FUN=sum));
    ## F.ko <- ecdf(model.vol.ko@sigma.t);
    ## F.spy <- ecdf(model.vol.spy@sigma.t);
    ## F.xlp <- ecdf(model.vol.xlp@sigma.t);
    
    ## trading.allowed <- F.ko(vol.t[1]) < vol.ub && F.spy(vol.t[2]) < vol.ub && F.xlp(vol.t[3]) < vol.ub;

    ## if (!trading.allowed && position == 0) {
    ##     cash[t] <- cash[t-1];
    ##     wealth[t] <- wealth[t-1];
    ##     next;
    ## }

    ## KO is leading
    ## if (mean(deviations[period]/prices) > 3.0e-3) {
    ##     holding <- holding + shares * (cash)
    ## }
    model <- lm(ko~pep+spy+xlp, data=prices);
    deviations[t] <- X$ko[t] - predict(model, newdata=X[t, ], n.ahead=1);
    shares <- c(1, -tail(coef(model), n=-1));
    ratio.cal[t, ] <- shares;

    ## value <- deviations[t];

    ## F <- ecdf(residuals(model));

    ## if (value > quantile(F, 1 - prob.trig)) {
    ## ## if ((prob > 1 - prob.trig && position %in% c(-1, 0)) || (prob > 0.45 && position == 1)) {
    ##     ## Short the portfolio
    ##     action <- rbind(action, c(t, -1));
    ##     cash[t] <- cash[t-1] + sum(shares * X[t, c(2,4,6)]);
    ##     holding <- holding - shares;        
    ## } else if (value < quantile(F, prob.trig)) {
    ##     ## Long the portfolio
    ##     action <- rbind(action, c(t, 1));
    ##     holding <- holding + shares;
    ##     cash[t] <- cash[t-1] - sum(shares * X[t, c(2,4,6)]);
    ## } else {
    ##     cash[t] <- cash[t-1];
    ## }
    ## wealth[t] <- cash[t] + sum(holding * X[t, c(2,4,6)]);
}
Y <- tail(deviations, n=-look.back);
dY <- diff(Y);
K <- length(dY);

cor.1 <- lapply(30:K, FUN=function(i) acf(dY[(i-29):i], lag.max=1)$acf[2]);
plot(1:length(cor.1), cor.1, type="l");
abline(h=c(-2/sqrt(30), 2/sqrt(30)), col="#FF0000");

plot(1:length(deviations), deviations, type="l");

par(mfrow=c(3, 1));
plot((look.back+1):n, ratio.cal[(look.back+1):n, 2], type="l");
plot((look.back+1):n, ratio.cal[(look.back+1):n, 3], type="b");
plot((look.back+1):n, ratio.cal[(look.back+1):n, 4], type="b");

C <- ratio.cal[(look.back+1):n, ];
dC <- apply(C, MARGIN=2, FUN=diff);
dC.rel <- dC[, 2]/head(C[, 2], n=-1);
I <- which(abs(dC.rel) < 1.0e-1) + look.back + 1;
J <- setdiff(1:n, I);
Z <- rep(NA, n);
Z[I] <- deviations[I];
Z[J] <- 0;

par(mfrow=c(2, 1));
plot(1:n, Z, "l");
points(J, rep(0, length(J)), col="#FF0000");
plot(1:n, ratio.cal[, 2], type="b");
points(J, rep(0, length(J)), col="#FF0000");
abline(h=seq(from=-0.3, to=0.4, by=0.1), col="#00FF00");

plot(1:dim(dC)[1], dC.rel,
     ## ylim=c(-1, 1),
     type="b");

F <- ecdf(dC.rel);
den <- density(dC.rel);
plot(den$x, den$y, type="l");



J <-900:1500;
plot(J, wealth[J], type="l");

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
