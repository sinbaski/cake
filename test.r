rm(list=ls());
graphics.off();
require(RMySQL);
require(fGarch);
require(xts);
source("~/kkasi/r/libxxie.r");
source("~/cake/libeix.r");

factor.algo <- function(t, params, holding)
{
    ## params$stocks: a list of column indices of S, indicating the stocks to trade
    ## holding must have the length as params    
    p <- length(holding);
    invested <- holding * prices[t, params$stocks];
    wealth <- sum(invested) + holding[p+1];
    stopifnot(wealth > 0);
    ret <- apply(log(prices[(t-params$lookback+1):t, params$stocks]), MARGIN=2, FUN=diff);
    C <- cov(ret);
    E <- eigen(C);
    scheme <- E$vectors;
    ## s <- apply(E$vectors, FUN=sum, MARGIN=2);
    ## scheme <- scheme %*% diag(1/s);
    net <- apply(scheme, MARGIN=2, FUN=sum);
    Y <- ret %*% scheme;
    mu <- apply(Y, MARGIN=2, FUN=mean);
    sharpe <- mu/sqrt(E$values);
    H <- matrix(NA, nrow=p, ncol=p);
    projections <- matrix(NA, nrow=p, ncol=p);
    for (i in 1:p) {
        projections[, i] <- proj.vec(invested, scheme[, i]);
    }
    exposed <- apply(abs(scheme), MARGIN=2, FUN=sum);
    I <- which(abs(sharpe) > params$sharpe.min);
    total <- sum(abs(sharpe[I]));
    for (i in 1:p) {
        score <- abs(sharpe[i]);
        if (score > params$sharpe.min) {
            scale <- wealth * params$exposure * score/total / exposed[i];
            shares <- as.matrix(scheme[, i]/prices[t, ]);
            H[i, ] <- scale * sign(mu[i]) * shares;
        } else if (sum(abs(projections[, i])) > 0 && mu[i] != 0) {
            inner <- sum(projections[, i] * scheme[, i]);
            if (sign(inner) == sign(mu[i])) {
                H[i, ] <- as.matrix(projections[, i]/prices[t, ]);
            } else {
                H[i, ] <- rep(0, p);                    
            }
        } else {
            H[i, ] <- rep(0, p);
        }
    }
    shares <- rep(NA, p+1);
    shares[1:p] <- apply(H, MARGIN=2, FUN=sum);
    shares[p+1] <- holding[p+1] - sum((shares[1:p] - hold[1:p]) * prices[t, ]);
    holding <- shares;
    return(holding);
}

make.strat <- function(fun, param, S)
{
    list()
}

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");

stmt <- paste(
"select spy_daily.tm as tm, spy_daily.closing as spy, pep_daily.closing as pep, ko_daily.closing as ko,",
"xlp_daily.closing as xlp from",
"spy_daily join pep_daily join ko_daily join xlp_daily on",
"spy_daily.tm = pep_daily.tm and pep_daily.tm = ko_daily.tm and ko_daily.tm = xlp_daily.tm;"
);
results <- dbSendQuery(database, stmt);
data <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);
prices <- data[, -1];
p <- dim(prices)[2];

log.mutation <- function(x, sig)
{
    stopifnot(x > 0);
    exp(log(x) + rnorm(n=1, mean=0, sd=sig));
}

strats <- vector("list", length=1000);



for (i in 1:length(strats)) {
    stocks <- 1:dim(prices)[2];
    lookback <- runif(1, min=15, max=25);
    sharpe.min <- runif(1, min=0.3, max=0.6);
    exposure <- runif(1, min=0.5, max=0.9);
    params <- list(stocks=stocks, lookback=round(lookback),
                   sharpe.min=sharpe.min, exposure=exposure
                   );
    holding <- c(rep(0, dim(prices)[2]), 1);
    strats[i] <- list(fun=factor.algo, params=params, holding=holding);
}

t0 <- 25;
for (t in t0:dim(prices)[1]) {
    holding <- matrix(NA, nrow=length(strats), ncol=p+1);
    for (i in 1:length(strats)) {
        holding[i, ] <- strats[i]$fun(t, params, strats[i]$holding);
    }
    if (t == t0) {
        for (i in 1:length(strats)) {
            strats[i]$holding <- holding[i, ];
        }
        next;
    }
    wealth.2 <- apply(holding[, -1] %*% t(prices[t, ]), MARGIN=1, FUN=sum) + holding[, p+1];
    wealth.1 <- apply(strats$holding[, -1] %*% t(prices[t-1, ]), MARGIN=1, FUN=sum) + strats$holding[, p+1];
    scores <- wealth.2/wealth.1 - 1;
    ## may be too costly to compute
    ## res <- uniroot(f=function(x) mean(exp(x * scores)) - 1, interval=c(0.1, 5), tol=0.01);
    winner <- rdiscrete(length(strats), probs=scores, values=1:length(strats));
    loser <- setdiff(1:length(strats), winner);
    ## How do we re-distribute the assets of the losers?
    strats <- strats[winner];

    ## Now mutate the parameters
    for (i in 1:length(strats)) {
        strats[i]$params$lookback <- round(log.mutation(strats[i]$params$lookback, 0.1));
        strats[i]$params$exposure <- round(log.mutation(strats[i]$params$exposure, 0.1));
        strats[i]$params$sharpe.min <- round(log.mutation(strats[i]$params$exposure, 0.1));
    }
    
    
}







