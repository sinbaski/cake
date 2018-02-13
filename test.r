rm(list=ls());
graphics.off();
require(RMySQL);
require(forecast);
source("~/cake/libeix.r");

## factor.algo <- function(tm, params, holding)
## {
##     ## params$stocks: a list of column indices of S, indicating the stocks to trade
##     ## holding must have the length as params    
##     p <- dim(prices)[2];
##     invested <- holding[1:p] * prices[tm, params$stocks];
##     wealth <- sum(invested) + holding[p+1];
##     stopifnot(wealth > 0);
##     ret <- apply(prices[(tm-params$T1+1):tm, params$stocks],
##                  MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
##     C <- cov(ret);
##     E <- eigen(C);
##     scheme <- E$vectors;
##     ## £ exposed for every £ invested, i.e. price for each unit of the portfolio
##     leverage <- apply(abs(scheme), MARGIN=2, FUN=sum);
##     Y <- ret %*% scheme;
##     result <- matrix(NA, nrow=p, ncol=2);
##     for (i in 1:p) {
##         result[i, ] <- tryCatch (
##             expr={
##                 tt <- t.test(Y[, i]);
##                 c(tt$estimate, tt$p.value);
##             }, error = function(e) {
##                 c(0, 1);
##             }
##         );
##     }
##     ## numerical abnormality
##     I <- which(E$values > 0);
##     E$values[setdiff(1:p, I)] <- min(E$values[I]);
##     sharpe <- result[, 1]/sqrt(E$values);
##     H <- matrix(NA, nrow=p, ncol=p);
##     projections <- matrix(NA, nrow=p, ncol=p);
##     for (i in 1:p) {
##         projections[, i] <- proj.vec(invested, scheme[, i]);
##     }
##     I <- which(result[, 2] < params$confidence);
##     J <- setdiff(1:p, I);
##     dev <- rep(NA, p);
##     for (i in J) {
##         S <- rep(NA, dim(Y)[1]+1);
##         S[1] <- 1;
##         for (j in 1:dim(Y)[1]) {
##             S[j+1] <- S[j] * (1 + Y[j, i]);
##         }
##         bs <- fit.BS(S);
##         res <- S[-1] - exp(bs$par[1] * (1:dim(Y)[1]));
##         ## Sharpe
##         sharpe[i] <- z/gaussian$mu - 1;
##         sharpe[i] <- sharpe[i]/(gaussian$sig/abs(gaussian$mu));
        
##         ## probability of being lower
##         dev[i] <- gaussian$fun.p(z);
##     }
##     K <- which(abs(sharpe) > params$sharpe.min);
##     total <- sum(abs(sharpe[K]));
##     for (i in intersect(I, K)) {
##         ## £ to invest in eigen portfolio i
##         size <- wealth * params$exposure * abs(sharpe[i]) / total;
##         ## Number of shares in each stock for each unit of the eigen portfolio
##         shares <- scheme[, i]/prices[tm, ];
##         ## Number of shares to buy/sell for eigen portfolio i
##         H[i, ] <- sign(result[i, 1]) * size / leverage[i] * shares;
##     }
##     probs <- params$probs;
##     for (i in intersect(J, K)) {
##         if (dev[i] < probs[1]) {
##             size <- wealth * params$exposure * abs(sharpe[i]) / total;
##             shares <- scheme[, i]/prices[tm, ];
##             H[i, ] <- size / leverage[i] * shares;
##         } else if (dev[i] > probs[4]) {
##             size <- wealth * params$exposure * abs(sharpe[i]) / total;
##             shares <- scheme[, i]/prices[tm, ];
##             H[i, ] <- -size / leverage[i] * shares;
##         } else {
##             inner <- sum(projections[, i] * scheme[, i]);
##             if (dev[i] >= probs[2] && inner > 0 || dev[i] <= probs[3] && inner < 0) {
##                 H[i, ] <- 0;
##             } else {
##                 H[i, ] <- projections[, i]/prices[tm, ];
##             }
##         }
##     }
##     shares <- rep(NA, p+1);
##     shares[1:p] <- apply(H, MARGIN=2, FUN=sum);
##     shares[p+1] <- holding[p+1] - sum((shares[1:p] - holding[1:p]) * prices[tm, ]);
##     holding <- shares;
##     return(holding);
## }

algo.arma <- function(tm, params, Y, E, alloc)
{
    p <- dim(prices)[2];
    ## £ exposed for every £ invested, i.e. price for each unit of the portfolio
    leverage <- apply(abs(E$vectors), MARGIN=2, FUN=sum);
    ## More diverse allocation schemes score higher.
    scheme.score <- leverage/p;

    sharpe <- rep(NA, p);
    H <- matrix(NA, nrow=p, ncol=p);
    flag <- FALSE;
    for (i in 1:p) {
        ## mu <- mean(Y[, i]);
        ## sig <- if (E$values[i] > 0) sqrt(E$values[i]) else NaN;
        ## if (is.nan(sig) || abs(mu)/sig > 2) {
        ##     sharpe[i] <- sign(mu) * 2;
        ##     flag <- TRUE;
        ##     next;
        ## }
        model <- fit.arma(Y[, i]);
        prediction <- predict(model, n.ahead=1);
        if (length(model$coef) > 0) {
            sharpe[i] <- prediction$pred[1]/prediction$se[1];
            next;
        }
        n <- dim(Y)[1] + 1;
        S <- rep(NA, n);
        S[1] <- 1;
        for (j in 1:(n-1)) {
            S[j+1] <- S[j] * (1 + Y[j, i]);
        }
        sharpe[i] <- (mean(head(S, n=-1))/S[n] - 1)/prediction$se[1];
    }
    if (!flag) return(rep(0, p));
    
    scores <- abs(sharpe) * scheme.score;
    normalizer <- sum(scores);
    ## £ to invest in eigen portfolio i
    to.expose <- scores / normalizer;
    for (i in 1:p) {
        H[i, ] <- sign(sharpe[i]) * (to.expose[i]/leverage[i]) * E$vectors[, i] / prices[tm, ];
    }
    shares <- apply(H, MARGIN=2, FUN=sum);
    k <- sum(abs(shares) * prices[tm, ]);
    shares <- shares * alloc/k;
    return(shares);
}

## algo.bs <- function(tm, params, Y, E, alloc)
## {
##     p <- dim(prices)[2];
##     ## £ exposed for every £ invested, i.e. price for each unit of the portfolio
##     leverage <- apply(abs(E$vectors), MARGIN=2, FUN=sum);
##     ## More diverse allocation schemes score higher.
##     scheme.score <- leverage/p;

##     sharpe <- rep(NA, p);
##     H <- matrix(NA, nrow=p, ncol=p);
##     flag <- FALSE;
##     for (i in 1:p) {
##         sig <- if (E$values[i] > 0) sqrt(E$values[i]) else NaN;
##         n <- params$T1;
##         S <- rep(NA, n);
##         S[1] <- 1;
##         for (j in 1:(n-1)) {
##             S[j+1] <- S[j] * (1 + Y[j, i]);
##         }
##         bs <- fit.BS(S);
##         if (!bs$fitted) {
##             sharpe[i] <- 0;
##             next;
##         }
##         ## If T2 = 1, we have a pure momentum strategy
##         sharpe[i] <- bs$E.proc(S[n - params$T2 + 1], params$T2)/S[n] - 1;
##         ## sig <- bs$sd.proc(S[n - params$T2 + 1], params$T2)/S[n];
##         sharpe[i] <- sharpe[i]/sig;
##         flag <- TRUE;
##     }
##     if (!flag) return(rep(0, p));

##     scores <- abs(sharpe) * scheme.score;
##     normalizer <- sum(scores);
##     ## £ to invest in eigen portfolio i
##     to.expose <- scores / normalizer;
##     for (i in 1:p) {
##         H[i, ] <- sign(sharpe[i]) * (to.expose[i]/leverage[i]) * E$vectors[, i] / prices[tm, ];
##     }
##     shares <- apply(H, MARGIN=2, FUN=sum);
##     k <- sum(abs(shares) * prices[tm, ]);
##     shares <- shares * alloc /k;
##     return(shares);
## }

factor.algo.2 <- function(tm, params, holding)
{
    p <- dim(prices)[2];
    invested <- holding[1:p] * prices[tm, 1:p];
    wealth <- sum(invested) + holding[p+1];
    stopifnot(wealth > 0);
    ret <- apply(prices[(tm-params$T1+1):tm, 1:p],
                 MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
    C <- cov(ret);
    E <- eigen(C);
    Y <- ret %*% E$vectors;

    H <- matrix(NA, nrow=2, ncol=p);

    s <- params$alloc + 1;
    ## H[1, ] <- algo.bs(tm, params, Y, E, params$alloc/s);
    H[1, ] <- 0;
    H[2, ] <- algo.arma(tm, params, Y, E, 1/s);

    shares <- apply(H, MARGIN=2, FUN=sum);
    shares <- shares/sum(abs(shares) * prices[tm, ]) * wealth * params$exposure;
    
    holding[p+1] <- holding[p+1] - sum((shares - holding[1:p]) * prices[tm, ]);
    holding[1:p] <- shares;
    return(holding);
}

gen.strat <- function(holding)
{
    T1 <- runif(1, min=15, max=25);
    T2 <- runif(1, min=1, max=T1);
    exposure <- runif(1, min=0.5, max=0.9);
    confidence <- runif(1, min=0.05, max=0.4);
    alloc <- runif(1, min=0.5, max=1.5);
    params <- list(T1=round(T1),
                   T2=round(T2),
                   confidence=confidence,
                   alloc=alloc,
                   exposure=exposure
                   );
    return(list(fun=factor.algo.2, params=params, holding=holding));
}

symbols <- c(
    "spy",
    "aal",
    "aapl",
    "acn",
    "adbe",
    "adm",
    "aes",
    "amd",
    "amp",
    "amzn",
    "ba",
    "blk",
    "bsx",
    "bxp",
    "c",
    "cbg",
    "cbs",
    "celg",
    "clx",
    "cnp",
    "cog",
    "cvx",
    "d",
    "dbb",
    "dgx",
    "duk",
    "dvn",
    "ewg",
    "ewj",
    "ewl",
    "ewn",
    "ewp",
    "ewq",
    "ewu",
    "f",
    "fox",
    "fxb",
    "fxc",
    "fxe",
    "fxy",
    "gd",
    "goog",
    "iau",
    "ibm",
    "jjc",
    "jpm",
    "ko",
    "luv",
    "nke",
    "pep",
    "pg",
    "pru",
    "qcom",
    "rcl",
    "rht",
    "sbac",
    "see",
    "slv",
    "syk",
    "syy",
    "t",
    "tsn",
    "txn",
    "udr",
    "ung",
    "unp",
    "ups",
    "uso",
    "utx",
    "uup",
    "var",
    "vfc",
    "wat",
    "whr",
    "wm",
    "wmt",
    "xec",
    "xlp",
    "xrx",
    "yum"
);

p <- length(symbols);

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="192.168.154.1");

days <- matrix(NA, ncol=2, nrow=p);
for (i in 1:length(symbols)) {
    rs <- dbSendQuery(database, sprintf("select min(tm), max(tm) from %s_daily;", symbols[i]));
    days[i, ] <- as.matrix(fetch(rs, n=-1));
}


## stmt <- "select spy_daily.tm as tm,"
## for (i in 1:length(symbols)) {
##     stmt <- paste(
##         stmt, sprintf("%1$s_daily.closing as %1$s,", symbols[i])
##     );
## }
## stmt <- paste(substr(stmt, 1, nchar(stmt)-1), "from");
## for (i in 1:length(symbols)) {
##     stmt <- paste(
##         stmt, sprintf("%s_daily join", symbols[i])
##     );
## }
## stmt <- paste(substr(stmt, 1, nchar(stmt) - 5), "on");
## for (i in 2:length(symbols)) {
##     stmt <- paste(
##         stmt, sprintf("spy_daily.tm = %s_daily.tm and", symbols[i])
##     );
## }
## stmt <- paste(substr(stmt, 1, nchar(stmt) - 4),
##               "order by spy_daily.tm;");

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
    holding <- c(rep(0, p), 1);
    strats[[i]] <- gen.strat(holding);
}

sys.holding <- matrix(NA, nrow=dim(prices)[1], ncol=dim(prices)[2]+1);

t0 <- 25;
V <- rep(NA, dim(prices)[1]);
V.max <- rep(length(strats), dim(prices)[1]);

require(foreach);
require(doMC);
require(sde);
registerDoMC(detectCores());
wealths <- matrix(1, ncol=2, nrow=length(strats));

cl <- makeCluster(detectCores());
t1 <- t0;
## for (tm in 233:260) {
for (tm in 101:500) {
    cat(sprintf("At time %d\n", tm));
    ## holding <- matrix(NA, nrow=length(strats), ncol=1+p);
    ## for (i in 1:length(strats)) {
    ##     holding[i, ] <- strats[[i]]$fun(tm, strats[[i]]$params, strats[[i]]$holding);
    ## }

    holding <- foreach(i=1:length(strats), .combine=rbind) %dopar% {
        strats[[i]]$fun(tm, strats[[i]]$params, strats[[i]]$holding);
    }
    sys.holding[tm, ] <- parApply(cl, holding, MARGIN=2, FUN=sum);
    V[tm] <- sum(sys.holding[tm, 1:p] * prices[tm, ]) + sys.holding[tm, p+1];
    ## V.max[tm] = if (V[tm] > V.max[tm-1]) V[tm] else V.max[tm-1];
    ## DD <- 1 - V[tm]/V.max[tm];
    if (tm - t1 > 5) {
        ret.spy <- sapply((t1+1):tm, FUN=function(k) prices[k, 1]/prices[k-1, 1] - 1);
        ret.me <- sapply((t1+1):tm, FUN=function(k) V[k]/V[k-1] - 1);
        F <- ecdf(ret.spy - ret.me);
        prob <- max(1 - F(0), 1/20);
    } else {
        prob <- 0.1;
    }
    flag <- sample(x=c(TRUE, FALSE), size=1, replace=TRUE, prob=c(prob, 1- prob));
    if (!flag) {
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- holding[i, ];
        }
        next;
    }

    ## who survive to the next period?
    wealths[, 2] <- holding[, 1:p] %*% prices[tm, ] + holding[, p + 1];
    R <- wealths[, 2]/wealths[, 1] - 1;
    mu <- mean(R);

    scores <- 4^((R - mu)/0.01);
    scores <- scores/sum(scores);
    winners <- sample(1:length(strats), size=length(strats),
                      prob=scores, replace=TRUE);
    worth <- 0;
    for (i in winners) {
        worth <- worth + sum(holding[i, 1:p] * prices[tm, ]) + holding[i, p+1];
    }
    k <- sum(wealths[, 2])/worth;
    holding <- holding[winners, ] * k;
    strats <- strats[winners];
    sys.holding[tm, ] <- parApply(cl, holding, MARGIN=2, FUN=sum);
    wealths[, 1] <- holding[, 1:p] %*% prices[tm, ] + holding[, p + 1];

    ## Survivors mutate
    sig <- 0.05 * exp(1 - V[tm]/V[t1]);
    t1 <- tm;
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- holding[i, ];
        l <- round(log.mutation(strats[[i]]$params$T1, sig));
        l <- min(tm, max(l, 5));
        strats[[i]]$params$T1 <- l;
        strats[[i]]$params$T2 <- min(max(1, round(log.mutation(strats[[i]]$params$T2, sig))), l);
        strats[[i]]$params$exposure <- min(log.mutation(strats[[i]]$params$exposure, sig), 1);
        strats[[i]]$params$confidence <- min(0.4, log.mutation(strats[[i]]$params$confidence, sig));
        strats[[i]]$params$alloc <- log.mutation(strats[[i]]$params$alloc, sig);
    }
}
stopCluster(cl);

W <- holding[, 1:p] %*% prices[tm, ] + holding[, p+1];
DD <- 1 - V[101:300]/V.max[101:300];
days <- data[, 1];

T1 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$T1);
T2 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$T2);
T <- cbind(T1, T2);
exposure <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$exposure);
alloc <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$alloc);
confidence <- sapply(1:length(strats), FUN=function(i) strats[[i]]$param$confidence);

idx <- which.max(W);
strats[[idx]]$params

## save(data, V, W, t0, t1, tm, days, holding,
##      T1, T2, alloc, exposure, confidence,
##      file="evolution.RData");

save(data, V, W, t0, t1, tm, days, holding,
     T1, T2, alloc, exposure, confidence,
     file="evolution600.RData");

## load(file="evolution.RData");
for (i in 1:length(strats)) {
    strats[[i]]$params$T1 <- T1[i];
    strats[[i]]$params$T2 <- T2[i];
    strats[[i]]$params$exposure <- exposure[i];
    strats[[i]]$params$alloc <- alloc[i];
    strats[[i]]$params$confidence <- confidence[i];
    strats[[i]]$holding <- c(rep(0, p), 1);
}
