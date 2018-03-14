rm(list=ls());
graphics.off();
require(RMySQL);
require(abind);
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
    if (wealth <= 0) {
        return(c(rep(0, p), wealth));
    }
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

qrm <- function(thedate)
{
    p <- dim(prices)[2];
    n <- 252;
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");
    S <- array(NA, dim=c(n+1, p, 3));
    vl <- matrix(NA, n, p);
    ret <- matrix(NA, n, p);
    ## col 1: forecast of the mean return
    ## col 2: forecast of the s.d. of the return
    ## We use univariate models, which don't account for covariances
    ## and hence are inaccurate, but multivariate models are
    ## computationally infeasible due to a large number of parameters.
    forecast <- matrix(NA, nrow=p, ncol=2);
    for (i in 1:p) {
        stmt <- sprintf(paste(
            "select high, low, closing from %s_daily where",
            "datediff('%s', tm) >= 0 order by tm desc limit %d;"),
            symbols[i], thedate, dim(S)[1]);
        rs <- dbSendQuery(database, stmt);
        S[, i, ] <- apply(as.matrix(fetch(rs, n=-1)), MARGIN=2, FUN=rev);
        S[-1, i, 1] <- sapply(2:dim(S)[1], FUN=function(j) max(S[j, i, 1], S[j-1, i, 3]));
        S[-1, i, 2] <- sapply(2:dim(S)[1], FUN=function(j) min(S[j, i, 2], S[j-1, i, 3]));
        dbClearResult(rs);
        vl[, i] <- sapply(2:(n+1), FUN=function(k) log(log(S[k, i, 1]/S[k, i, 2])));
        ret[, i] <- tail(S[, i, 3], n=-1)/head(S[, i, 3], n=-1) - 1;
        model.vol <- fit.arma(vl[, i], order.max=c(2,2), include.mean=TRUE);
        model.ret <- fit.arma(ret[, i], order.max=c(2,2));
        forecast[i, 1] <- predict(model.ret, n.ahead=1)$pred[1];
        forecast[i, 2] <- exp(predict(model.vol, n.ahead=1)$pred[1]);
    }
    dbDisconnect(database);
    C <- cor(ret);
    outer <- forecast[, 2] %*% t(forecast[, 2]);
    C <- C * outer;
    return(list(mean=forecast[, 1], cov.mtx=C));
}

trade <- function(thedate, confidence=0.01, risk.tol=0.02)
{
    p <- dim(prices)[2];
    closing <- tail(prices, n=1);
    export <- c(
        "prices", "strats", "exposure.max",
        "factor.algo.2", "algo.arma", "fit.arma"
    );
    holding <- foreach(i=1:length(strats), .combine=rbind, .export=export) %dopar% {
            factor.algo.2(strats[[i]]$params, strats[[i]]$holding, exposure.max);
    }
    loading <- t(sapply(1:dim(holding)[1], FUN=function(i) holding[i, 1:p] * closing));
    H <- apply(loading, MARGIN=2, FUN=mean);
    worth <- sum(H) + mean(holding[, p+1]);
    exposure <- sum(abs(H));
    if (exposure > 0) {
        G <- H/exposure;

        ## Risk management
        forecast <- qrm(thedate);
        sig <- sqrt(G %*% forecast$cov.mtx %*% G);
        mu <- sum(G * forecast$mean);
        ## expected shortfall
        J <- integrate(f=function(x) x * dnorm(x, mean=mu, sd=sig),
                       lower=-Inf, upper=qnorm(confidence, mean=mu, sd=sig),
                       rel.tol=1.0e-2
                       );
        es <- J$value/confidence;
        ## scaling w.r.t. G, which has exposure 1
        scaling <- min(risk.tol/abs(es), exposure.max)/exposure;
        holding[, 1:p] <- holding[, 1:p] * scaling;
        holding[, p+1] <- holding[, 1+p] + (1 - scaling) * apply(loading, MARGIN=1, FUN=sum);
        loading <- loading * scaling;
        H <- H * scaling;
    }
    L <- which(H[1:p] > 0);
    S <- which(H[1:p] < 0);
    long <- if (length(L) > 0) sum(H[L]) else 0;
    short <- if (length(S) > 0) -sum(H[S]) else 0;
    cat(sprintf("    long %.3f, short %.3f, cash %.3f\n", long, short, mean(holding[, p+1])));

    ## Record the trade
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");
    stmt <- sprintf("insert into trade_log values ('%s'", thedate);
    for (i in 1:p) {
        stmt <- paste(stmt, closing[i], H[i], sep=",");
    }
    stmt <- paste(stmt, ");");
    rs <- dbSendQuery(database, stmt);
    dbClearResult(rs);
    dbDisconnect(database);
    return(list(holding=holding, loading=loading));
}

gen.strat <- function(T1.max, holding)
{
    T1 <- runif(1, min=15, max=T1.max);
    ## loss.tol <- runif(1, min=1.0e-3, max=5.0e-3);
    loss.tol <- 5.0e-3;
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

kendall <- function(positions)
{
    n <- dim(positions)[3];
    X <- as.vector(
        apply(tail(prices, n=n+1), MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1)
    );
    tau <- rep(NA, dim(positions)[1]);
    for (i in 1:dim(positions)[1]) {
        pos <- as.vector(t(positions[i, ,]));
        tau[i] <- if (length(unique(X)) > 1 && length(unique(pos)) > 1)
                      cor(X, pos, method="kendall")
                  else 0;
    }
    return(tau);
}

## save.conf <- function(strats, R)
## {
##     T1 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1);
##     GDA <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$gda);
##     score.min <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$score.min);
##     loss.tol <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$loss.tol);
##     save(T1, GDA, score.min, loss.tol, R, file="StratsReturns.RData");
## }

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
    "spy",  ## S&P 500
    "dia",  ## Dow Jones
    "qqq",  ## Nasdaq
    ## "ezu",  ## Euro zone equities
    ## "ewg",  ## Germany
    ## "ewj",  ## Japan
    ## "ewl",  ## Switzerland
    ## "ewn",  ## Netherlands
    ## "ewp",  ## Spain
    ## "ewq",  ## France
    ## "ewu",  ## UK
    ## "uup",  ## USD
    ## "fxb",  ## British pound
    ## "fxc",  ## Canadian dollar
    ## "fxe",  ## euro
    ## "fxy",  ## Japanese yen
    "iau",  ## gold
    "slv"  ## silver
    ## "vxx",  ## SP500 short term volatility
    ## "vixy", ## SP500 short term volatility
    ## "vxz",  ## SP500 mid term volatility
    ## "viix"  ## SP500 short term volatility
);

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");
rs <- dbSendQuery(database, "select tm from spy_daily where tm between '2006-04-28' and '2018-01-18';");
days <- fetch(rs, n=-1)[[1]];
dbClearResult(rs);

rs <- dbSendQuery(database, "drop table if exists trade_log;");
dbClearResult(rs);

stmt <- paste(
    "create table trade_log (",
    "tm date primary key"
);
for (i in 1:length(symbols)) {
    stmt <- paste(
        stmt,
        sprintf("%1$s_p double not null, %1$s_q double not null", symbols[i]),
        sep=","
    );
}
stmt <- paste(stmt, ");");
rs <- dbSendQuery(database, stmt);
dbClearResult(rs);
dbDisconnect(database);

V <- rep(NA, length(days));
V.max <- rep(1, length(days));
DD <- rep(NA, length(days));

require(foreach);
require(doParallel);
## require(doMC);
cl <- makeCluster(7);
registerDoParallel(cl);


t0 <- 253;
t1 <- t0;
exposure.max <- 0.8;
strats <- vector("list", length=500);
for (i in 1:length(strats)) {
    holding <- c(rep(0, length(symbols)), 1);
    strats[[i]] <- gen.strat(min(120, t0), holding);
}
wealths <- rep(1, length(strats));
HHistory <- NA;
period <- 20;
in.drawdown <- FALSE;
included <- 1:length(symbols);
for (tm in 280:length(days)) {
    ## update prices
    T <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1);
    T.max <- max(T);
    data.new <- get.data(symbols, days[(tm-T.max+1)], days[tm]);
    prices <- data.new$prices;
    p <- dim(prices)[2];
    W <- sapply(1:length(strats), FUN=function(i) {
        H <- strats[[i]]$holding;
        sum(head(H, n=-1) * tail(prices, n=1)) + tail(H, n=1);
    });

    V[tm] <- mean(W);
    V.max[tm] = if (V[tm] > V.max[tm-1]) V[tm] else V.max[tm-1];
    DD[tm] <- 1 - V[tm]/V.max[tm];
    in.drawdown <- DD[tm] >= 0.1;

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
        outcome <- trade(days[tm]);
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- outcome$holding[i, ];
        }
        if (tm == t1) {
            HHistory <- outcome$loading
        } else {
            HHistory <- abind(HHistory, outcome$loading, along=3);
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
    tau <- kendall(HHistory);

    n <- dim(prices)[1];
    cat("    returns: ", c(min(ret), mean(ret), max(ret)), "\n");
    cat("    Sharpe: ", c(min(sharpe), mean(sharpe), max(sharpe)), "\n");
    cat("    tau: ", c(min(tau), mean(tau), max(tau)), "\n");
    ## Bad performance. Search the entire configuration space
    idx <- which.max(tau);
    cat("    Best:  ", paste(strats[[idx]]$params), "\n");
    idx <- which.min(tau);
    cat("    Worst: ", paste(strats[[idx]]$params), "\n");
    cat(sprintf("    SPY:   %.3f\n", tail(prices, n=1)[1]/prices[n-(tm-t1), 1] - 1));

    if (max(tau) < 0.1) {
        cat(sprintf("    regenerate.\n"));
        for (i in 1:length(strats)) {
            holding <- c(rep(0, p), V[tm]);
            strats[[i]] <- gen.strat(min(120, tm), holding);
        }
        t1 <- tm;
        outcome <- trade(days[tm]);
        for (i in 1:length(strats)) {
            strats[[i]]$holding <- outcome$holding[i, ];
        }
        HHistory <- outcome$loading;
        next;
    }

    ## who survive to the next period?
    cat(sprintf("    select.\n"));
    fitness <- exp((tau - mean(tau))/sd(tau));
    N <- ceiling(length(strats) * 0.85);
    winners <- sample(1:length(strats), size=N,
                      prob=fitness, replace=TRUE);
    cat(sprintf("    %d (%d) selected.\n", length(unique(winners)), N));
    strats[1:N] <- strats[winners];
    for (i in 1:N) {
        strats[[i]]$holding <- c(rep(0, p), V[tm]);
    }
    for (i in (N+1):length(strats)) {
        strats[[i]] <- gen.strat(min(120, tm), c(rep(0, p), V[tm]));
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
        ## strats[[i]]$params$loss.tol <- min(log.mutation(strats[[i]]$params$loss.tol, sig), 1);
        strats[[i]]$params$gda <- log.mutation(strats[[i]]$params$gda, sig);
        strats[[i]]$params$score.min <- log.mutation(strats[[i]]$params$score.min, sig);
    }
    outcome <- trade(days[tm]);
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- outcome$holding[i, ];
    }
    HHistory <- outcome$loading;
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
