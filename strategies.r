rm(list=ls());
graphics.off();
require(RMySQL);
require(fGarch);
require(xts);
source("~/kkasi/r/libxxie.r");
source("~/cake/libeix.r");

sl291217 <- function(S, lookback, exposure=0.5)
{
    market <- rep("unknown", length(S));
    holding <- matrix(NA, ncol=2, nrow=length(S));
    ## Number of shares
    holding[1:(lookback-1), 1] <- 0;
    ## cash amount
    holding[1:(lookback-1), 2] <- 1;
    for (t in (lookback):length(S)) {
        interval <- (t - lookback + 1):t;
        wealth <- holding[t-1, 1] * S[t] + holding[t-1, 2];
        stopifnot(wealth > 0);
        mkt <- categorise.mkt(S[interval], lookback, 0.45);

        if (mkt$category == "up" && market[t-1] != "up") {
            holding[t, 1] <- wealth * exposure / S[t];
        } else if (mkt$category == "up" && market[t-1] == "up") {
            holding[t, 1] <- holding[t-1];
        } else if (mkt$category == "OU") {
            r <- mkt$par[1] + (log(S[t]) - mkt$par[1]) * exp(-mkt$par[2]) - log(S[t]);
            r.sd <- mkt$par[3]/sqrt(2)/sqrt(mkt$par[2]) * sqrt(1 - exp(-2*mkt$par[2]));
            if (r/r.sd > 1) {
                holding[t, 1] <- wealth * exposure / S[t];
            } else if (r/r.sd < -1) {
                holding[t, 1] <- -wealth * exposure / S[t];
            } else {
                holding[t, 1] <- 0;
            }
        } else { ## market "unknown" or "down"
            holding[t, 1] <- 0;
        }
        holding[t, 2] <- holding[t-1, 2] - (holding[t, 1] - holding[t-1, 1]) * S[t];
        market[t] <- mkt$category;
    }
    return(holding);
}

sl281217 <- function(S, lookback, exposure=0.5)
{
    holding <- matrix(NA, ncol=2, nrow=length(S));
    ## Number of shares
    holding[1:(lookback-1), 1] <- 0;
    ## cash amount
    holding[1:(lookback-1), 2] <- 1;
    drift <- rep(NA, length(S));
    N <- 252;
    for (t in lookback:length(S)) {
        interval <- (t - lookback + 1):t;
        bs <- fit.BS(S[interval]);
        drift[t] <- bs$par[1];
        if (t < N + lookback - 1) {
            holding[t, ] <- holding[t-1, ];
            next;
        }
        wealth <- holding[t-1, 1] * S[t] + holding[t-1, 2];
        stopifnot(wealth > 0);
        
        emp <- ecdf(drift[(t-N+1):t]);
        qf <- function(x) quantile(emp, x);
        z <- drift[t];
        if (z > qf(0.7) && z > 0) {
            holding[t, 1] <- wealth * exposure / S[t];
        } else if (z < qf(0.3) && z < 0) {
            holding[t, 1] <- 0;
        } else {
            holding[t, 1] <- holding[t-1, 1];
        }
        holding[t, 2] <- holding[t-1, 2] - (holding[t, 1] - holding[t-1, 1]) * S[t];
    }
    return(holding);
}

sm281217 <- function(S, lookback, exposure=0.7)
{
    holding <- matrix(NA, ncol=2, nrow=length(S));
    ## Number of shares
    holding[1:(lookback-1), 1] <- 0;
    ## cash amount
    ## holding[1:(lookback-1), 2] <- exp(cumsum(c(0, diff(log(spy[1:(lookback-1)])))));
    holding[1:(lookback-1), 2] <- 1;
    for (t in (lookback):length(S)) {
        interval <- (t - lookback + 1):t;
        wealth <- holding[t-1, 1] * S[t] + holding[t-1, 2];
        stopifnot(wealth > 0);
        
        bs <- fit.BS(S[interval]);
        ## expected <- S[t - lookback + 1] * exp((bs$par[1] - bs$par[2]^2/2)*(1:(lookback-1)) + bs$par[2]^2/2);
        expected <- S[t - lookback + 1] * exp(bs$par[1] * (1:(lookback-1)));
        res <- tail(S[interval], n=-1) - expected;
        dist.res <- fit.dist(res);
        if (dist.res$bic == Inf) {
            stop("Distribution fitting failed.");
            holding[t, ] <- holding[t-1, ];
            next;
        }
        z <- tail(res, n=1);
        ql <- dist.res$fun.q(0.1);
        qm <- dist.res$fun.q(0.5);
        qh <- dist.res$fun.q(0.9);
        if (z < ql) {
            holding[t, 1] <- wealth * exposure / S[t];
        } else if (z > qh) {
            holding[t, 1] <- -wealth * exposure / S[t];
        } else if (z <= qm && holding[t-1, 1] < 0 || z >= qm && holding[t-1, 1] > 0 ) {
            holding[t, 1] <- 0;
        } else {
            holding[t, 1] <- holding[t-1, 1];
        }
        ## holding[t, 2] <- holding[t-1, 2] * (spy[t]/spy[t-1])- (holding[t, 1] - holding[t-1, 1]) * S[t];
        holding[t, 2] <- holding[t-1, 2] - (holding[t, 1] - holding[t-1, 1]) * S[t];
    }
    return(holding);
}

cm291217 <- function(S, T1, T2, exposure=0.7)
{
    holding <- matrix(NA, ncol=2, nrow=length(S));
    ## Number of shares
    holding[1:(T1 + T2 - 1), 1] <- 0;
    ## cash amount
    ## holding[1:(lookback-1), 2] <- exp(cumsum(c(0, diff(log(spy[1:(lookback-1)])))));
    holding[1:(T1 + T2 - 1), 2] <- 1;
    for (t in (lookback):length(S)) {
        interval <- (t - lookback + 1):t;
        wealth <- holding[t-1, 1] * S[t] + holding[t-1, 2];
        stopifnot(wealth > 0);
        
        bs <- fit.BS(S[interval]);
        expected <- S[t - lookback + 1] * exp((bs$par[1] - bs$par[2]^2/2)*(1:(lookback-1)) + bs$par[2]^2/2);
        res <- tail(S[interval], n=-1) - expected;
        dist.res <- fit.dist(res);
        if (dist.res$bic == Inf) {
            stop("Distribution fitting failed.");
            holding[t, ] <- holding[t-1, ];
            next;
        }
        z <- tail(res, n=1);
        ql <- dist.res$fun.q(0.1);
        qm <- dist.res$fun.q(0.5);
        qh <- dist.res$fun.q(0.9);
        if (z < ql) {
            holding[t, 1] <- wealth * exposure / S[t];
        } else if (z > qh) {
            holding[t, 1] <- -wealth * exposure / S[t];
        } else if (z <= qm && holding[t-1, 1] < 0 || z >= qm && holding[t-1, 1] > 0 ) {
            holding[t, 1] <- 0;
        } else {
            holding[t, 1] <- holding[t-1, 1];
        }
        ## holding[t, 2] <- holding[t-1, 2] * (spy[t]/spy[t-1])- (holding[t, 1] - holding[t-1, 1]) * S[t];
        holding[t, 2] <- holding[t-1, 2] - (holding[t, 1] - holding[t-1, 1]) * S[t];
    }
    return(holding);
}

## mixer <- function(S, lookback, strats=list(sl281217, sm281217), alloc=c(0.5, 0.5))
## {
##     V <- matrix(NA, nrow=length(S), ncol=length(strats));
##     for (i in 1:length(strats)) {
##         holding <- strats[[i]](S, lookback);
##         V[, i] <- holding[, 1] * S + holding[, 2];
##     }
##     return(V %*% alloc);
## }



database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");

stmt <- paste(
    "select tm, low, high, closing from pep_daily;"
);
results <- dbSendQuery(database, stmt);
data <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

S <- data$closing;
n <- dim(data)[1];
lookback <- 20;

holding <- sl281217(S, 30, 0.7);
V1 <- holding[, 1] * S + holding[, 2];

holding <- sm281217(S, 30, 0.7);
V2 <- holding[, 1] * S + holding[, 2];

holding <- sl291217(S, lookback, 0.7);
V3 <- holding[, 1] * S + holding[, 2];

m <- 1;
n <- dim(data)[1];
## n <- 851;
step <- 50;
## I.long <- intersect(which(holding[, 1] > 0 & market == "up"), m:n);
## I.short <- intersect(which(holding[, 1] < 0 & market == "down"), m:n);
## I.ouup <- intersect(which(holding[, 1] > 0 & market == "OU"), m:n);
## I.oudown <- intersect(which(holding[, 1] < 0 & market == "OU"), m:n);
I.long <- intersect(which(holding[, 1] > 0), m:n);
I.short <- intersect(which(holding[, 1] < 0), m:n);

graphics.off();
par(mfrow=c(2, 1));
grid.line <- seq(from=m, to=n, by=step);
## grid.line <- seq(from=1, to=n, by=1);
plot(x=m:n, S[m:n], type="l", ylim=c(min(data$low[m:n]), max(data$high[m:n])), xaxt="n");
## lines(x=m:n, data$low[m:n], col="#FF0000");
## lines(x=m:n, data$high[m:n], col="#00FF00");
points(I.long, S[I.long], pch=".", cex=2, col="#00FF00");
points(I.short, S[I.short], pch=".", cex=2, col="#FF0000");
## points(I.ouup, S[I.ouup], pch=0, col="#0000FF");
## points(I.oudown, S[I.oudown], pch=15, col="#0000FF");
abline(v=grid.line, lty=2);
axis(side=1, at=grid.line, labels=grid.line, las=2);

plot(x=m:n, V2[m:n], type="l", xaxt="n");
abline(v=grid.line, lty=2);
abline(h=1, lty=2, col="#0000FF");
axis(side=1, at=grid.line, labels=grid.line, las=2);
 
