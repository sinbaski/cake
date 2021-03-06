rm(list=ls());
graphics.off();
require(RMySQL);
source("./libeix.r");
set.seed(0);

algo.arma <- function(params, exposure.max, Y, E)
{
    p <- dim(prices)[2];
    K <- 1;
    while(K <= p && E$values[K] > 1.0e-6) K <- K + 1;
    K <- K - 1;
    ## mean, sd, GDA score, expected loss when we are in loss for each £ exposed.
    metrics <- matrix(NA, nrow=K, ncol=4);
    SR <- function(mu, sig) {
        I <- integrate(
            f=function(x) x * dnorm(x, mean=mu, sd=sig),
            lower=if (mu > 0) -Inf else 0,
            upper=if (mu > 0) 0 else Inf,
            rel.tol=1.0e-2
        );
        score <- mu + params$gda * I$value;
        if (mu > 0) {
            es <- abs(I$value)/pnorm(0, mean=mu, sd=sig);
        } else {
            es <- abs(I$value)/(1 - pnorm(0, mean=mu, sd=sig));
        }
        return(c(score, es));
    }
    for (i in 1:K) {
        model <- fit.arma(Y[, i], order.max=c(2,2));
        prediction <- predict(model, n.ahead=1);
        if (length(model$coef) > 0) {
            mu <- prediction$pred[1];
            sig <- prediction$se[1];
            metrics[i, 1:2] <- c(mu, sig);
            metrics[i, 3:4] <- SR(mu, sig);
        } else {
            ## No gain expected. We don't gamble.
            metrics[i, 1] <- 0;
            metrics[i, 2] <- prediction$se[1];
            metrics[i, 3:4] <- NA;
        }
    }
    if (sum(!is.na(metrics[, 3])) == 0) {
        return(rep(0, p));
    }
    i <- which.max(abs(metrics[, 3]));
    exposure <- min(params$loss.tol/metrics[i, 4], exposure.max/sum(abs(E$vectors[, i])));
    shares <- rep(0, p);
    if (abs(metrics[i, 3]) > params$score.min) {
        shares <- sign(metrics[i, 1]) * exposure * E$vectors[, i]/tail(prices, n=1);
    }
    return(shares);
}

factor.algo.2 <- function(params, holding, exposure.max)
{
    p <- dim(prices)[2];
    invested <- holding[1:p] * tail(prices, n=1);
    wealth <- sum(invested) + holding[p+1];
    stopifnot(wealth > 0);
    ret <- apply(tail(prices, n=params$T1), MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
    C <- cov(ret);
    E <- eigen(C);
    Y <- ret %*% E$vectors;
    shares <- algo.arma(params, exposure.max, Y, E);
    shares <- shares * wealth;

    holding[p+1] <- holding[p+1] - sum((shares - holding[1:p]) * tail(prices, n=1));
    holding[1:p] <- shares;
    return(holding);
}

trade <- function()
{
    p <- dim(prices)[2];
    export <- c(
        "prices", "strats", "exposure.max",
        "factor.algo.2", "algo.arma", "fit.arma"
    );
    holding <- foreach(i=1:length(strats), .combine=rbind, .export=export) %dopar% {
            params <- strats[[i]]$params;
            holding <- strats[[i]]$holding;
            factor.algo.2(strats[[i]]$params, strats[[i]]$holding, exposure.max);
    }
    H <- apply(holding, MARGIN=2, FUN=mean);
    L <- which(H[1:p] > 0);
    S <- which(H[1:p] < 0);
    long <- if (length(L) > 0) sum(tail(prices, n=1)[L] * H[L]) else 0;
    short <- if (length(S) > 0) -sum(tail(prices, n=1)[S] * H[S]) else 0;
    cat(sprintf("    long %.3f, short %.3f, cash %.3f\n", long, short, H[p+1]));
    return(holding=holding);
}

gen.strat <- function(T1.max, holding)
{
    T1 <- runif(1, min=15, max=T1.max);
    loss.tol <- runif(1, min=1.0e-3, max=5.0e-3);
    gda <- runif(1, min=0.05, max=1);
    score.min <- runif(1, min=1.0e-3, max=1.0e-2);
    params <- list(T1=round(T1),
                   gda=gda,
                   score.min=score.min,
                   loss.tol=loss.tol
                   );
    return(list(params=params, holding=holding));
}

log.mutation <- function(x, sig)
{
    stopifnot(min(x) > 0);
    x * exp(rnorm(n=length(x), mean=0, sd=sig));
}

save.conf <- function(strats, R)
{
    T1 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1);
    GDA <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$gda);
    score.min <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$score.min);
    loss.tol <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$loss.tol);
    save(T1, GDA, score.min, loss.tol, R, file="StratsReturns.RData");
}

get.data <- function(assets, day1, day2)
{
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                         dbname='market', host="localhost");
    included <- 1;
    n <- NA;
    for (i in 1:length(assets)) {
        rs <- dbSendQuery(
            database,
            sprintf(paste("select closing from %s_daily ",
                          "where tm between '%s' and '%s'",
                          "order by tm;"),
                    assets[i], day1, day2)
        );
        data <- fetch(rs, n=-1);
        if (i == 1) {
            prices <- data$closing;
            n <- dim(data)[1];
        } else {
            if (dim(data)[1] == n) {
                prices <- cbind(prices, data$closing);
                included <- c(included, i);
            } else {
                cat(sprintf("%s excluded.\n", assets[i]));
            }
        }
        dbClearResult(rs);
    }
    dbDisconnect(database);
    return(list(prices=prices, included=included));
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
    "mmm",
    "nke",
    "nvda",
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
    "yum",

    "jo",   ## coffee
    "dba",  ## agriculture fund
    "weat", ## wheat
    "hyg",  ## high yield cooporate bond
    "jnk",  ## high yield cooporate bond
    "lqd",  ## cooporate bond
    "vcsh", ## cooporate bond

    "vxx",  ## SP500 short term volatility
    "vixy", ## SP500 short term volatility
    "vxz",  ## SP500 mid term volatility
    "viix"  ## SP500 short term volatility
);

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");
rs <- dbSendQuery(database, "select tm from spy_daily where tm between '2011-09-19' and '2018-01-18';");
days <- fetch(rs, n=-1)[[1]];
dbClearResult(rs);
dbDisconnect(database);

## sys.holding <- matrix(NA, nrow=dim(prices)[1], ncol=dim(prices)[2]+1);


V <- rep(NA, length(days));
V.max <- rep(1, length(days));
DD <- rep(NA, length(days));

require(foreach);
require(doParallel);
## require(doMC);
cl <- makeCluster(7);
registerDoParallel(cl);


t0 <- 60;
t1 <- t0;
exposure.max <- 1.5;
strats <- vector("list", length=500);
for (i in 1:length(strats)) {
    holding <- c(rep(0, p), 1);
    strats[[i]] <- gen.strat(t0, holding);
}
wealths <- rep(1, length(strats));
period <- 20;
in.drawdown <- FALSE;
## data.old <- list(prices={}, included=1:82);
included <- 1:length(symbols);
## wealths <- matrix(1, nrow=length(strats), ncol=2);
for (tm in 670:1000) {
    ## update prices
    T <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1);
    T.max <- max(T);
    data.new <- get.data(symbols, days[(tm-T.max+1)], days[tm]);
    J <- 1:dim(data.new$prices)[2];
    if (!setequal(data.new$included, included)) {
        J <- which(data.new$included %in% included);
        stopifnot(length(J) == length(included));
    }
    prices <- data.new$prices;
    p <- dim(prices)[2];
    W <- sapply(1:length(strats), FUN=function(i) {
        H <- strats[[i]]$holding;
        sum(head(H, n=-1) * tail(prices, n=1)[J]) + tail(H, n=1);
    });

    included <- data.new$included;
    
    V[tm] <- mean(W);
    V.max[tm] = if (V[tm] > V.max[tm-1]) V[tm] else V.max[tm-1];
    DD[tm] <- 1 - V[tm]/V.max[tm];
    in.drawdown <- DD[tm] >= 0.015;

    if (tm == t1) {
        wealths <- W;
    } else {
        wealths <- rbind(wealths, W);
    }

    action <- "go";
    cat("\n", sprintf("On day %d, %s, DD=%.3f, value = %.3f\n", tm, days[tm], DD[tm], V[tm]));
    if (tm - t1 >= 12) {
        T <- t.test(sapply((t1+1):tm, FUN=function(i) V[i]/V[i-1] - 1), alternative="less");
        if (T$p.value < 0.2 && T$estimate < 0 || in.drawdown || (tm - t1) %% 20 == 0) {
            action <- "evaluate";
        } else {
            action <- "go";
        }
        cat(sprintf("    t-test: p-value=%.3f, estimate=%.3f\n", T$p.value, T$estimate));
    } else {
        action <- "go";
    }
    if (action == "go") {
        cat(sprintf("    continue\n"));
        holding <- trade();
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- holding[i, ];
        }
        next;
    }
    ## Compute statistics since the last selection or regeneration
    cat(sprintf("    evaluate.\n"));
    
    R <- apply(wealths, MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
    ## sharpe may be NaN when the wealths over the entire period have not changed.
    sharpe <- sapply(1:dim(R)[2], FUN=function(i) mean(R[, i])/sd(R[, i]));
    sharpe[is.nan(sharpe)] <- 0;
    ret <- sapply(1:dim(wealths)[2], FUN=function(i) tail(wealths[, i], n=1)/head(wealths[, i], n=1) - 1);
    
    cat("    returns: ", c(min(ret), mean(ret), max(ret)), "\n");
    cat("    Sharpe: ", c(min(sharpe), mean(sharpe), max(sharpe)), "\n");
    ## Bad performance. Search the entire configuration space
    idx <- which.max(sharpe);
    cat("    Best:  ", paste(strats[[idx]]$params), "\n");
    idx <- which.min(sharpe);
    cat("    Worst: ", paste(strats[[idx]]$params), "\n");
    cat(sprintf("    SPY:   %.3f\n", tail(prices, n=1)[1]/head(prices,n=1)[1] - 1));

    if (max(sharpe) < 1.0) {
        cat(sprintf("    regenerate.\n"));
        for (i in 1:length(strats)) {
            holding <- c(rep(0, p), V[tm]);
            strats[[i]] <- gen.strat(120, holding);
        }
        t1 <- tm;
        holding <- trade();
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- holding[i, ];
        }
        next;
    }

    ## who survive to the next period?
    cat(sprintf("    select.\n"));
    ## fitness <- exp(sharpe - mean(sharpe));
    fitness <- exp((sharpe - mean(sharpe))/sd(sharpe));
    ## fitness <- pnorm(sharpe, mean=mean(sharpe), sd=sd(sharpe));
    N <- ceiling(length(strats) * 0.85);
    winners <- sample(1:length(strats), size=N,
                      prob=fitness, replace=TRUE);
    cat(sprintf("    %d (%d) selected.\n", length(unique(winners)), N));
    strats[1:N] <- strats[winners];
    for (i in 1:N) {
        strats[[i]]$holding <- c(rep(0, p), V[tm]);
    }
    for (i in (N+1):length(strats)) {
        strats[[i]] <- gen.strat(120, c(rep(0, p), V[tm]));
    }
    wealths <- rep(V[tm], length(strats));
    
    ## Survivors mutate
    sig <- 0.05 * exp(1 - V[tm]/V[t1]);
    ## sig <- 0.02;
    t1 <- tm;
    for (i in 1:N) {
        l <- round(log.mutation(strats[[i]]$params$T1, sig));
        l <- min(tm, max(l, 5));
        strats[[i]]$params$T1 <- l;
        strats[[i]]$params$loss.tol <- min(log.mutation(strats[[i]]$params$loss.tol, sig), 1);
        strats[[i]]$params$gda <- log.mutation(strats[[i]]$params$gda, sig);
        strats[[i]]$params$score.min <- log.mutation(strats[[i]]$params$score.min, sig);
    }
    holding <- trade();
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- holding[i, ];
    }
}
stopCluster(cl);

## W <- holding[, 1:p] %*% prices[tm, ] + holding[, p+1];
## DD <- 1 - V[t0:tm]/V.max[t0:tm];
## days <- data[, 1];

T1 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1);
GDA <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$gda);
score.min <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$score.min);
loss.tol <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$loss.tol);
A <- cbind(T1, GDA, score.min, loss.tol);

## save(data, V, W, t0, t1, tm, days, holding,
##      T1, T2, alloc, exposure, confidence,
##      file="evolution.RData");

save(T1, GDA, score.min, loss.tol, R, file="StratsReturns.RData");

## load(file="evolution.RData");
for (i in 1:length(strats)) {
    strats[[i]]$holding <- c(rep(0, p), 1);
}
