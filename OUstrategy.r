rm(list=ls());
graphics.off();
require(RMySQL);
require("xts");
require("fGarch");
require("sde");


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

dX <-as.data.frame(apply(X[,c(2,4,6)], MARGIN=2, FUN=diff));

## n1 <- dim(X)[1] %/% 3;
## n2 <- dim(X)[1] - n1;
## model <- lm(ko ~ spy + xlp, data=X[1:n1,]);
## Y <- tail(X$ko, n=-n1) - as.matrix(X[(n1+1):n, 1:2]) %*% coef(model)[-1] - coef(model)[1];

n <- dim(dX)[1];
look.back <- 252;
L <- 125;
rebase <- seq(from=look.back+1, to=n, by=L);
C <- matrix(NA, nrow=length(rebase), ncol=2);
Y <- rep(NA, n-look.back);
idx <- 1;
for (i in 1:length(rebase)) {
    I <- (rebase[i]-look.back):(rebase[i]-1);
    # dZ <- dX[I, ]/X[I+1, c(3,5,7)]
    model <- lm(ko~spy+xlp-1, data=dX[I, ]);
    C[i, ] <- coef(model);
    ## R <- dX[rebase[i]:(rebase[i] + L - 1), 1] - as.matrix(dX[rebase[i]:(rebase[i] + L - 1), c(2,3)]) %*% C[i, ];
    Y[idx:(idx+L-1)] <- X[rebase[i]:(rebase[i]+L-1), 2] - as.matrix(X[rebase[i]:(rebase[i]+L-1), c(4,6)]) %*% coef(model);
    idx <- idx + L;
    
    ## fit an Ornstein-Uhlenbeck process to the residuals
    ## lik.fun <- function(mu, theta, sigma) {
    ##     -sum(dcOU(x=tail(Y, n=-1),
    ##               Dt=1,
    ##               x0=head(Y, n=-1),
    ##               theta=c(theta*mu, theta, sigma),
    ##               log=TRUE
    ##               ));
    ## };
    ## sigma <- sd(R);
    ## fit <- optim(par=c(0.001, sigma),
    ##              fn=function(param) lik.fun(1, param[1], param[2])
    ##              ## method="L-BFGS-B"
    ##              );


    ## C.cov[,,i] <- cov(Z);
    ## C.cor[,,i] <- cor(Z);

    ## model <- lm(ko ~ spy + xlp);
    ## R <- diff(log(X[i:(i + L - 1), 1])) -
    ##     cbind(diff(log(X[i:(i+L-1), 2])),
    ##           diff(log(X[i:(i+L-1), 3]))) %*% coef(model)[-1] - coef(model)[1];
    ## Y[idx : (idx + L - 1)] = c(1, exp(cumsum(R)));
    ## idx <- idx + L;
    
    ## OU process of the prices
    ## ko <- X$ko[I];
    ## spy <- X$spy[I];
    ## xlp <- X$xlp[I];
    ## model <- lm(ko ~ spy + xlp);
    ## C[, idx] <- coef(model);

    ## J <- i:(i+L-1);
    ## Y[J-look.back] <- X$ko[J] - as.matrix(X[J, 1:2]) %*% C[-1, idx] - C[1, idx];

    ## idx <- idx + 1;


    ## beta <- coef(model)[-1];
    ## intercept <- coef(model)[1];
    ## R <- residuals(model);

    
    ## Y <- X$ko[J] - cbind(X$spy[J], X$xlp[J]) %*% beta;
    ## ptfl[J] <- Y;
}
plot(1:length(Y), Y, t="l");
abline(v=seq(from=20, to=length(Y), by=20), col="#0000FF");
abline(h=0, col="#FF0000");
plot(1:dim(C)[1], C[, 1], type='l');

plot(1:dim)
par(mfrow=c(2,3));
plot(1:dim(C.cov)[3], C.cov[1,1,], type="l");
plot(1:dim(C.cov)[3], C.cov[2,2,], type="l");
plot(1:dim(C.cov)[3], C.cov[3,3,], type="l");
plot(1:dim(C.cov)[3], C.cov[1,2,], type="l");
plot(1:dim(C.cov)[3], C.cov[1,3,], type="l");
plot(1:dim(C.cov)[3], C.cov[2,3,], type="l");

## plot((L+1):length(X$ko), tail(X$ko, n=-L), type="l");
## lines(1:length(ptfl), ptfl, col="#0000FF");
plot(1:length(ptfl), ptfl, col="#0000FF");

## series <- xts(ptfl[2:(n+1)], order.by=as.Date(X$tm));
## plot(series);
plot(ptfl, type="l",
     ylab="portfolio price",
     xlab="time"
     );
rebase <- (1:periods)*L;
## abline(v=rebase, col='#00FF00');
abline(h=1, col='#FF0000')

abline(h = -1:5, v = -2:3, col = "red", lty = 3)
## W <- rep(NA, 2);
## W[2] <- 1/(1 - coef(model)[1]);
## W[1] <- 1 - W[2];

## eps <- residuals(model);
## Y <- exp(cumsum(eps));
## series <- xts(Y, order.by=as.Date(X$tm));
## plot(series);
## abline(h=0, col='#FF0000');



## Y.ma <- rollmean(Y, )
    



## dev <- xts(series$dev, order.by=as.Date(series$day));
## plot(dev);
## abline(h=0, col='#FF0000');




