rm(list=ls());
graphics.off();
require(RMySQL);
source("~/cake/libeix.r");

factor.algo <- function(tm, params, holding)
{
    ## params$stocks: a list of column indices of S, indicating the stocks to trade
    ## holding must have the length as params    
    p <- dim(prices)[2];
    invested <- holding[1:p] * prices[tm, params$stocks];
    wealth <- sum(invested) + holding[p+1];
    stopifnot(wealth > 0);
    ret <- apply(prices[(tm-params$T1+1):tm, params$stocks],
                 MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
    C <- cov(ret);
    E <- eigen(C);
    scheme <- E$vectors;
    ## £ exposed for every £ invested, i.e. price for each unit of the portfolio
    leverage <- apply(abs(scheme), MARGIN=2, FUN=sum);
    Y <- ret %*% scheme;
    result <- matrix(NA, nrow=p, ncol=2);
    for (i in 1:p) {
        result[i, ] <- tryCatch (
            expr={
                tt <- t.test(Y[, i]);
                c(tt$estimate, tt$p.value);
            }, error = function(e) {
                c(0, 1);
            }
        );
    }
    ## numerical abnormality
    I <- which(E$values > 0);
    E$values[setdiff(1:p, I)] <- min(E$values[I]);
    sharpe <- result[, 1]/sqrt(E$values);
    H <- matrix(NA, nrow=p, ncol=p);
    projections <- matrix(NA, nrow=p, ncol=p);
    for (i in 1:p) {
        projections[, i] <- proj.vec(invested, scheme[, i]);
    }
    I <- which(result[, 2] < params$confidence);
    J <- setdiff(1:p, I);
    dev <- rep(NA, p);
    for (i in J) {
        S <- rep(NA, dim(Y)[1]+1);
        S[1] <- 1;
        for (j in 1:dim(Y)[1]) {
            S[j+1] <- S[j] * (1 + Y[j, i]);
        }
        bs <- fit.BS(S);
        res <- S[-1] - exp(bs$par[1] * (1:dim(Y)[1]));
        ## Sharpe
        sharpe[i] <- z/gaussian$mu - 1;
        sharpe[i] <- sharpe[i]/(gaussian$sig/abs(gaussian$mu));
        
        ## probability of being lower
        dev[i] <- gaussian$fun.p(z);
    }
    K <- which(abs(sharpe) > params$sharpe.min);
    total <- sum(abs(sharpe[K]));
    for (i in intersect(I, K)) {
        ## £ to invest in eigen portfolio i
        size <- wealth * params$exposure * abs(sharpe[i]) / total;
        ## Number of shares in each stock for each unit of the eigen portfolio
        shares <- scheme[, i]/prices[tm, ];
        ## Number of shares to buy/sell for eigen portfolio i
        H[i, ] <- sign(result[i, 1]) * size / leverage[i] * shares;
    }
    probs <- params$probs;
    for (i in intersect(J, K)) {
        if (dev[i] < probs[1]) {
            size <- wealth * params$exposure * abs(sharpe[i]) / total;
            shares <- scheme[, i]/prices[tm, ];
            H[i, ] <- size / leverage[i] * shares;
        } else if (dev[i] > probs[4]) {
            size <- wealth * params$exposure * abs(sharpe[i]) / total;
            shares <- scheme[, i]/prices[tm, ];
            H[i, ] <- -size / leverage[i] * shares;
        } else {
            inner <- sum(projections[, i] * scheme[, i]);
            if (dev[i] >= probs[2] && inner > 0 || dev[i] <= probs[3] && inner < 0) {
                H[i, ] <- 0;
            } else {
                H[i, ] <- projections[, i]/prices[tm, ];
            }
        }
    }
    shares <- rep(NA, p+1);
    shares[1:p] <- apply(H, MARGIN=2, FUN=sum);
    shares[p+1] <- holding[p+1] - sum((shares[1:p] - holding[1:p]) * prices[tm, ]);
    holding <- shares;
    return(holding);
}

factor.algo.2 <- function(tm, params, holding)
{
    ## params$stocks: a list of column indices of S, indicating the stocks to trade
    ## holding must have the length as params    
    p <- dim(prices)[2];
    invested <- holding[1:p] * prices[tm, params$stocks];
    wealth <- sum(invested) + holding[p+1];
    stopifnot(wealth > 0);
    ret <- apply(prices[(tm-params$T1+1):tm, params$stocks],
                 MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
    C <- cov(ret);
    E <- eigen(C);
    scheme <- E$vectors;
    ## £ exposed for every £ invested, i.e. price for each unit of the portfolio
    leverage <- apply(abs(scheme), MARGIN=2, FUN=sum);
    Y <- ret %*% scheme;
    sharpe <- rep(NA, p);
    H <- matrix(NA, nrow=p, ncol=p);
    for (i in 1:p) {
        n <- params$T1;
        S <- rep(NA, n);
        S[1] <- 1;
        for (j in 1:(n-1)) {
            S[j+1] <- S[j] * (1 + Y[j, i]);
        }
        bs <- fit.BS(S);
        if (!bs$fitted) {
            sharpe[i] <- 0;
            next;
        }
        ## If T2 = 1, we have a pure momentum strategy
        sharpe[i] <- bs$E.proc(S[n - params$T2 + 1], params$T2)/S[n] - 1;
        sig <- bs$sd.proc(S[n - params$T2 + 1], params$T2)/S[n];
        sharpe[i] <- sharpe[i]/sig;
    }
    attractive <- which(abs(sharpe) > params$sharpe.min);
    normalizer <- sum(abs(sharpe[attractive]));

    ordinary <- setdiff(1:p, attractive);
    
    for (i in attractive) {
        ## £ to invest in eigen portfolio i
        size <- wealth * params$exposure * abs(sharpe[i]) / normalizer;
        ## Number of shares in each stock for each unit of the eigen portfolio
        shares <- scheme[, i]/prices[tm, ];
        ## Number of shares to buy/sell for eigen portfolio i
        H[i, ] <- sign(sharpe[i]) * size / leverage[i] * shares;
    }
    for (i in ordinary) {
        ## H[i, ] <- rep(0, p);
        proj.vec(invested, scheme[, i]);
    }
    shares <- rep(NA, p+1);
    shares[1:p] <- apply(H, MARGIN=2, FUN=sum);
    shares[p+1] <- holding[p+1] - sum((shares[1:p] - holding[1:p]) * prices[tm, ]);
    holding <- shares;
    return(holding);
}

symbols <- c(
    ## country ETF    
    "spy",
    "ewg",
    "ewj",
    "ewl",
    "ewn",
    "ewp",
    "ewq",
    "ewu",
    ## FX
    "fxb",
    "fxc",
    "fxe",
    "fxy",
    "uup",
    ## gold
    "iau",
    ## industrial base metal broad
    "dbb",
    ## copper
    "jjc"
);

stmt <- "select spy_daily.tm as tm,"
for (i in 1:length(symbols)) {
    stmt <- paste(
        stmt, sprintf("%1$s_daily.closing as %1$s,", symbols[i])
    );
}
stmt <- paste(substr(stmt, 1, nchar(stmt)-2), "from");
for (i in 1:length(symbols)) {
    stmt <- paste(
        stmt, sprintf("%s_daily join", symbols[i])
    );
}
stmt <- paste(substr(stmt, 1, nchar(stmt) - 5), "on");
for (i in 2:length(symbols)) {
    stmt <- paste(
        stmt, sprintf("spy_daily.tm = %s_daily.tm and", symbols[i])
    );
}
stmt <- paste(substr(stmt, 1, nchar(stmt) - 4),
              "order by spy_daily.tm;");

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="192.168.154.1");

results <- dbSendQuery(database, stmt);
data <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);
prices <- as.matrix(data[, -1]);
p <- dim(prices)[2];

log.mutation <- function(x, sig)
{
    stopifnot(min(x) > 0);
    exp(log(x) + rnorm(n=length(x), mean=0, sd=sig));
}

strats <- vector("list", length=4000);
for (i in 1:length(strats)) {
    stocks <- 1:p;
    T1 <- runif(1, min=15, max=25);
    T2 <- runif(1, min=1, max=T1);
    confidence <- runif(1, min=0.05, max=0.3);
    exposure <- runif(1, min=0.5, max=0.9);
    sharpe.min <- runif(1, min=0.3, max=1);
    ## probs <- rep(NA, 4);
    ## probs[1] <- runif(1, min=0.1, max=0.25);
    ## probs[2] <- runif(1, min=0.45, max=0.50);
    ## probs[3] <- runif(1, min=0.50, max=0.55);
    ## probs[4] <- runif(1, min=0.75, max=0.90);
    params <- list(stocks=stocks,
                   T1=round(T1),
                   T2=round(T2),
                   ## probs=probs,
                   sharpe.min=sharpe.min,
                   confidence=confidence,
                   exposure=exposure
                   );
    holding <- c(rep(0, p), 1);
    strats[[i]] <- list(fun=factor.algo.2, params=params, holding=holding);
}

sys.holding <- matrix(NA, nrow=dim(prices)[1], ncol=dim(prices)[2]+1);

t0 <- 25;
N <- 10;
V <- rep(NA, dim(prices)[1]);
V.max <- rep(length(strats), dim(prices)[1]);

require(foreach);
require(doMC);
require(sde);
registerDoMC(detectCores());
cl <- makeCluster(detectCores());
for (tm in 1201:1400) {
    cat(sprintf("At time %d\n", tm));
    ## holding <- matrix(NA, nrow=length(strats), ncol=p+1);
    ## for (i in 1:length(strats)) {
    ##     holding[i, ] <- strats[[i]]$fun(tm, strats[[i]]$params, strats[[i]]$holding);
    ## }
    holding <- foreach(i=1:length(strats), .combine=rbind) %dopar% {
        strats[[i]]$fun(tm, strats[[i]]$params, strats[[i]]$holding);
    }
    sys.holding[tm, ] <- parApply(cl, holding, MARGIN=2, FUN=sum);
    V[tm] <- sum(sys.holding[tm, 1:p] * prices[tm, ]) + sys.holding[tm, p+1];
    V.max[tm] = if (V[tm] > V.max[tm-1]) V[tm] else V.max[tm-1];
    stopifnot(1 - V[tm]/V.max[tm] < 0.1);

    if (tm - t0 <= 2*N || (tm - t0) %% N != 0) {
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- holding[i, ];
        }
        next;
    }
    
    wealths <- holding[, 1:p] %*% prices[tm, ] + holding[, p+1];
    stopifnot(min(wealths) > 0);

    ## who survive to the next period?
    mu.w <- mean(wealths);
    scores <- 4^((wealths/mu.w - 1)/0.01);
    scores <- scores/sum(scores);
    winners <- sample(1:length(strats), size=length(strats),
                      prob=scores, replace=TRUE);
    worth <- 0;
    for (i in winners) {
        worth <- worth + sum(holding[i, 1:p] * prices[tm, ]) + holding[i, p+1];
    }
    k <- sum(wealths)/worth;
    holding[winners, ] <- k * holding[winners, ];
    holding <- holding[winners, ];
    strats <- strats[winners];
    sys.holding[tm, ] <- parApply(cl, holding, MARGIN=2, FUN=sum);

    ## Survivors mutate
    sig <- 0.05 * exp(1 - V[tm]/V[tm - N]);
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- holding[i, ];
        l <- round(log.mutation(strats[[i]]$params$T1, sig));
        l <- min(tm, max(l, 5));
        strats[[i]]$params$T1 <- l;
        strats[[i]]$params$T2 <- min(max(1, round(log.mutation(strats[[i]]$params$T2, sig))), l);
        strats[[i]]$params$exposure <- min(log.mutation(strats[[i]]$params$exposure, sig), 1);
        strats[[i]]$params$confidence <- log.mutation(strats[[i]]$params$confidence, sig);
        strats[[i]]$params$sharpe.min <- log.mutation(strats[[i]]$params$sharpe.min, sig);
        ## probs <- sort(log.mutation(strats[[i]]$params$probs, sig));
        ## probs[probs > 1] = 1;
        ## strats[[i]]$params$probs <- probs;
    }
}
stopCluster(cl);
W <- holding[, 1:p] %*% prices[tm, ] + holding[, p+1];
DD <- 1 - V[t0:tm]/V.max[t0:tm];
days <- data[, 1];

T1 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$T1);
T2 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$T2);
exposure <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$exposure);
confidence <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$confidence); 
## probs <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$probs);

save(data, V, W, t0, tm, days, T1, T2,
     exposure, confidence,
     file="evolution.RData");
