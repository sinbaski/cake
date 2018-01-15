rm(list=ls());
graphics.off();
require(RMySQL);
source("~/kkasi/r/libxxie.r");
source("~/cake/libeix.r");

factor.algo <- function(t, params, holding)
{
    ## params$stocks: a list of column indices of S, indicating the stocks to trade
    ## holding must have the length as params    
    p <- dim(prices)[2];
    invested <- holding * prices[t, params$stocks];
    wealth <- sum(invested) + holding[p+1];
    stopifnot(wealth > 0);
    ret <- apply(log(prices[(t-params$lookback+1):t, params$stocks]), MARGIN=2, FUN=diff);
    C <- cov(ret);
    E <- eigen(C);
    scheme <- E$vectors;
    Y <- ret %*% scheme;
    result <- matrix(NA, nrow=p, ncol=2);
    for (i in 1:p) {
        tt <- t.test(Y[, i]);
        result[i, 1] <- tt$estimate;
        result[i, 2] <- tt$p.value;
    }
    H <- matrix(NA, nrow=p, ncol=p);
    projections <- matrix(NA, nrow=p, ncol=p);
    for (i in 1:p) {
        projections[, i] <- proj.vec(invested, scheme[, i]);
    }
    exposed <- apply(abs(scheme), MARGIN=2, FUN=sum);
    I <- which(result[, 2] < params$confidence);
    total <- sum(abs(result[I, 1]));
    for (i in 1:p) {
        score <- abs(result[i, 1]);
        if (result[i, 2] < params$confidence) {
            scale <- wealth * params$exposure * score/total / exposed[i];
            shares <- as.matrix(scheme[, i]/prices[t, ]);
            H[i, ] <- scale * sign(result[i, 1]) * shares;
        } else if (sum(abs(projections[, i])) > 0 && score != 0) {
            inner <- sum(projections[, i] * scheme[, i]);
            if (sign(inner) == sign(result[i, 1])) {
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
    shares[p+1] <- holding[p+1] - sum((shares[1:p] - holding[1:p]) * prices[t, ]);
    holding <- shares;
    return(holding);
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

## load(file="./strategies_population.RData");


strats <- vector("list", length=4000);
for (i in 1:length(strats)) {
    stocks <- 1:p;
    lookback <- runif(1, min=15, max=25);
    confidence <- runif(1, min=0.05, max=0.3);
    exposure <- runif(1, min=0.5, max=0.9);
    params <- list(stocks=stocks, lookback=round(lookback),
                   confidence=confidence, exposure=exposure
                   );
    holding <- c(rep(0, p), 1);
    strats[[i]] <- list(fun=factor.algo, params=params, holding=holding);
}

sys.holding <- matrix(NA, nrow=dim(prices)[1], ncol=dim(prices)[2]+1);

t0 <- 25;
N <- 10;
require(parallel);
## for (t in t0:dim(prices)[1]) {
for (t in t0:80) {
    holding <- matrix(NA, nrow=length(strats), ncol=p+1);
    for (i in 1:length(strats)) {
        holding[i, ] <- strats[[i]]$fun(t, strats[[i]]$params, strats[[i]]$holding);
    }
    wealths <- apply(holding[, 1:p] %*% t(prices[t, ]), MARGIN=1, FUN=sum) + holding[, p+1];
    if (t - t0 <= 2*N || (t - t0) %% N != 0) {
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- holding[i, ];
        }
        sys.holding[t, ] <- apply(holding, MARGIN=2, FUN=sum);
        next;
    }
    stopifnot(min(wealths) > 0);

    ## who survive to the next period?
    mu.w <- mean(wealths);
    scores <- 10^((wealths/mu.w - 1)/0.01);
    scores <- scores/sum(scores);
    winners <- sample(1:length(strats), size=length(strats),
                      prob=scores, replace=TRUE);
    worth <- 0;
    for (i in winners) {
        worth <- worth + sum(holding[i, 1:p] * prices[t, ]) + holding[i, p+1];
    }
    k <- sum(wealths)/worth;
    holding[winners, ] <- k * holding[winners, ];
    holding <- holding[winners, ];
    strats <- strats[winners];
    sys.holding[t, ] <- apply(holding, MARGIN=2, FUN=sum);

    ## Survivors mutate
    values <- apply(prices[(t-N):t, ] * sys.holding[(t-N):t, 1:p],
                    MARGIN=1, FUN=sum) + sys.holding[(t-N):t, p+1];
    ret <- diff(log(values));
    sig <- 0.075 * exp(-mean(ret));
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- holding[i, ];
        l <- round(log.mutation(strats[[i]]$params$lookback, sig));
        l <- min(t, max(l, 5));
        strats[[i]]$params$lookback <- l;
        strats[[i]]$params$exposure <- log.mutation(strats[[i]]$params$exposure, sig);
        strats[[i]]$params$confidence <- log.mutation(strats[[i]]$params$confidence, sig);
    }

}
w <- apply(holding[, 1:p] %*% t(prices[t, ]), MARGIN=1, FUN=sum) + holding[, p+1];

strat.lookback <- rep(NA, length(strats));
for (i in 1:length(strats)) {
    strat.lookback[i] <- strats[[i]]$param$lookback;
}


