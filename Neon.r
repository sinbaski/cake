stopCluster(cl);
rm(list=ls());
graphics.off();
require(RMySQL);
require(abind);
require(forecast)
require(tseries);
source("~/cake/libeix.r");

set.seed(0);

population.size <- 500;
mut.rate <- 0.15;
exposure.max <- 1.5;
resample.period <- 3;

T1.min <- 15;
T1.maxmin <- 25;
loss.tol <- 5.0e-3;

num.cores <- 6;
time.span <- "daily";

strats <- vector("list", length=population.size);


propose.trade <- function(T1, exposure.max, Y, E)
{
    p <- dim(Y)[2];
    K <- max(which(E$values > 0));
    metrics <- matrix(NA, nrow=K, ncol=2);
    ES <- function(mu, sig) {
        I <- integrate(
            f=function(x) x * dnorm(x, mean=mu, sd=sig),
            lower=if (mu > 0) -Inf else 0,
            upper=if (mu > 0) 0 else Inf,
            rel.tol=1.0e-2
        );
        if (mu > 0) {
            es <- abs(I$value)/pnorm(0, mean=mu, sd=sig);
        } else {
            es <- abs(I$value)/(1 - pnorm(0, mean=mu, sd=sig));
        }
        return(es);
    }
    for (i in 1:K) {
        ## model <- fit.arma(Y[, i]);
        ## prediction <- predict(model, n.ahead=1);
        ## metrics[i, ] <- c(prediction$pred[1], prediction$se[1]);
        model <- auto.arima(Y[, i]);
        prediction <- forecast(model, h=1);
        metrics[i, ] <- c(prediction$mean[1], model$sigma2);
    }
    if (sum(abs(metrics[, 1])) == 0) {
        return(c(rep(0, p), 0));
    }
    B <- diag(1/metrics[, 2]) %*% metrics[, 1];
    B <- B / sum(abs(B));

    mu <- t(B) %*% metrics[, 1];
    sig <- sqrt(t(B^2) %*% metrics[, 2]);

    M <- E$vector[, 1:K];
    alloc <- M %*% B;
    c <- sum(abs(alloc));
    alloc <- alloc/c;

    mu <- mu/c;
    sig <- sig/c;

    stopifnot(mu > 0);

    es <- ES(mu, sig);
    exposure <- min(loss.tol/es, exposure.max);
    shares <- exposure * as.vector(alloc)/tail(prices, n=1);
    return(c(shares, mu/sig));
}

factor.algo <- function(T1, L, exposure.max)
{
    p <- dim(prices)[2];
    n <- dim(prices)[1];
    idx <- rev(seq(from=n, by=-L, length.out=T1));
    shares <- rep(0, p);

    if (L == 1) {
        S <- prices[idx, ];
    } else {
        S <- t(sapply(idx, FUN=function(k) {
            apply(prices[(k-L+1):k, ], MARGIN=2, FUN=mean);
        }));
    }
   ## ret <- apply(S, MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
    ret <- tail(S, n=-1)/head(S, n=-1) - 1;
    C <- cov(ret);
    E <- eigen(C);
    Y <- ret %*% E$vectors;
    proposal <- propose.trade(T1, exposure.max, Y, E);
    return(proposal);
}

qrm <- function(thedate)
{
    p <- dim(prices)[2];
    n <- 60;
    if (use.database) {
        database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                             dbname='market', host="localhost");
    }
    S <- array(NA, dim=c(n+1, p, 3));
    vl <- matrix(NA, n, p);
    ret <- matrix(NA, n, p);
    ## col 1: forecast of the mean return
    ## col 2: forecast of the s.d. of the return
    ## We use univariate models, which don't account for covariances
    ## and hence are inaccurate, but multivariate models are
    ## computationally infeasible due to a large number of parameters.
    prediction <- matrix(NA, nrow=p, ncol=2);
    for (i in 1:p) {
        if (use.database) {
            stmt <- sprintf(paste(
                "select high, low, closing from %s_daily where",
                "datediff('%s', tm) >= 0 order by tm desc limit %d;"),
                symbols[i], thedate, dim(S)[1]);
            rs <- dbSendQuery(database, stmt);
            S[, i, ] <- apply(as.matrix(fetch(rs, n=-1)), MARGIN=2, FUN=rev);
            dbClearResult(rs);
        } else {
            S[, i, ] <- tail(price.data[, , i], n=dim(S)[1]);
        }
        S[-1, i, 1] <- sapply(2:dim(S)[1], FUN=function(j) {
            max(S[j, i, 1], S[j-1, i, 3])
        });
        S[-1, i, 2] <- sapply(2:dim(S)[1], FUN=function(j) {
            min(S[j, i, 2], S[j-1, i, 3])
        });

        vl[, i] <- sapply(2:(n+1), FUN=function(k) {
            log(log(S[k, i, 1]/S[k, i, 2]))
        });
        ret[, i] <- tail(S[, i, 3], n=-1)/head(S[, i, 3], n=-1) - 1;
        ## model.ret <- fit.arma(ret[, i]);
        ## prediction[i, 1] <- predict(model.ret, n.ahead=1)$pred[1];
        ## model.vol <- fit.arma(vl[, i]);
        ## prediction[i, 2] <- exp(predict(model.vol, n.ahead=1)$pred[1]);
        model.ret <- auto.arima(ret[, i]);
        prediction[i, 1] <- forecast(model.ret, h=1)$mean[1];
        model.vol <- auto.arima(vl[, i]);
        prediction[i, 2] <- exp(forecast(model.vol, h=1)$mean[1]);
    }
    if (use.database) {
        dbDisconnect(database);
    }
    C <- cor(ret) * (prediction[, 2] %*% t(prediction[, 2]));
    return(list(mean=prediction[, 1], cov.mtx=C));
}

send.trade <- function(thedate, worth, holding)
{
    n <- dim(prices)[1];
    H <- tail(prices, n=1) * holding[1:length(symbols)];
    cash <- tail(holding, n=1);
    stopifnot(abs(sum(H) + cash - worth) < 1.0e-6);

    L <- which(H[1:p] > 0);
    S <- which(H[1:p] < 0);
    long <- if (length(L) > 0) sum(H[L]) else 0;
    short <- if (length(S) > 0) -sum(H[S]) else 0;
    cat(sprintf("    long %.3f, short %.3f, cash %.3f\n",
                long, short, cash));

    ## Record the trade
    if (use.database) {
        database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                             dbname='market', host="localhost");
        stmt <- sprintf("insert into trade_log values ('%s', %e", thedate, worth);
        for (i in 1:p) {
            stmt <- paste(stmt, prices[n, i], H[i], sep=",");
        }
        stmt <- paste(stmt, ");");
        rs <- dbSendQuery(database, stmt);
        dbClearResult(rs);
        dbDisconnect(database);
    } else {
        row <- sprintf("%s,%e", thedate, worth);
        for (i in 1:p) {
            row <- paste(row, sprintf("%.4f,%e", prices[n, i], H[i]), sep=",");
        }
        write.table(
            row, "trade_log.csv", append=TRUE,
            row.names=FALSE, col.names=FALSE, quote=FALSE
        );
    }
}

trade <- function(thedate, confidence=0.01, risk.tol=7.5e-3)
{
    p <- dim(prices)[2];
    n <- dim(prices)[1];
    ## WR <- sapply(1:length(strats), FUN=function(i) {
    ##     H <- strats[[i]]$holding;
    ##     x <- sum(H[1:p] * prices[n-1, ]) + H[p+1];
    ##     y <- sum(H[1:p] * prices[n, ]) + H[p+1];
    ##     return(c(y, x, y/x - 1));
    ## });
    ## stopifnot(poor.ratio > 0 || sum(WR[1, ] == W) == length(strats));
    ## stopifnot(min(WR[1, ]) > 0);
    ## stopifnot(min(WR[2, ]) > 0);

    ## if (length(unique(WR[3, ])) > 1) {
    ##     W <- WR[2, ] * exp((WR[3, ] * 20));
    ##     W <- W / mean(W);
    ## } else {
    ##     W <- WR[1, ]/mean(WR[1, ]);
    ## }
    ## stopifnot(min(W) > 0);
    ## W <- rep(1, length(strats));


    timescales <- t(unique(
        sapply(1:length(strats), FUN=function(i) {
            pa <- strats[[i]]$params;
            c(pa$T1, pa$L);
        }), MARGIN=2));
    clusterExport(cl, c("prices", "strats", "exposure.max", "auto.arima",
                        "factor.algo", "propose.trade", "loss.tol",
                        "adf.test", "forecast"));
    clusterExport(cl, c("timescales"), envir=environment());
    proposals <- t(parSapply(
        cl, 1:dim(timescales)[1], FUN=function(i) {
            factor.algo(timescales[i, 1], timescales[i, 2], exposure.max);
        }));
    ## proposals <- matrix(NA, nrow=dim(timescales)[1], ncol=p+1);
    ## for (i in 1:dim(timescales)[1]) {
    ##     proposals[i, ] <- factor.algo(
    ##         timescales[i, 1], timescales[i, 2], exposure.max
    ##     );
    ## }
    clusterExport(cl, c("W", "p", "proposals"), envir=environment());
    holding <- t(parSapply(cl, 1:population.size, FUN=function(i) {
        k <- which(timescales[, 1] == strats[[i]]$params$T1 &
                   timescales[, 2] == strats[[i]]$params$L);
        if (tail(proposals[k, ], n=1) > strats[[i]]$params$sharpe.min) {
            ## shares <- head(proposals[k, ], n=-1) * W[i];
            shares <- head(proposals[k, ], n=-1);
            H <- rep(NA, p+1);
            H[p+1] <- 1 - sum(shares * tail(prices, n=1));
            H[1:p] <- shares;
            H;
        } else {
            c(rep(0, p), 1);
        }
    }));
    H <- apply(holding, MARGIN=2, FUN=mean);
    G <- as.vector(H[1:p] * tail(prices, n=1));
    stopifnot(abs(sum(G) + H[p+1] - 1) < 1.0e-8);

    exposure <- sum(abs(G));
    if (exposure > 0) {
        G <- G/exposure;

        ## Risk management
        prediction <- qrm(thedate);
        sig <- sqrt(t(G) %*% prediction$cov.mtx %*% G);
        mu <- sum(G * prediction$mean);
        ## expected shortfall
        J <- integrate(f=function(x) x * dnorm(x, mean=mu, sd=sig),
                       lower=-Inf, upper=qnorm(confidence, mean=mu, sd=sig),
                       rel.tol=1.0e-2
                       );
        es <- J$value/confidence;
        ## scaling w.r.t. H
        scaling <- min(risk.tol/abs(es), exposure.max)/exposure;
        ## holding[, 1:p] <- holding[, 1:p] * scaling;
        ## holding[, p+1] <- holding[, 1+p] +
        ##     (1 - scaling) * apply(loading, MARGIN=1, FUN=sum);
        ## loading <- loading * scaling;
        H[1:p] <- H[1:p] * scaling;
        H[p+1] <- 1 - sum(H[1:p] * tail(prices, n=1));
    }
    return(list(holding=holding, assets=H));
}

save.stats <- function(thedate, R)
{
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                         dbname='market', host="localhost");
    stmt <- paste(
        "insert into StratsRet values",
        sprintf(
            "('%s', %d, %.3f, %e)",
            thedate, strats[[1]]$params$T1,
            strats[[1]]$params$sharpe.min,
            R[1]
        )
    );
    for (i in 2:length(strats)) {
        stmt <- paste(
            stmt,
            sprintf(
                "('%s', %d, %.3f, %e)",
                thedate, strats[[i]]$params$T1,
                strats[[i]]$params$sharpe.min,
                R[i]
            ),
            sep=","
        );
    }
    stmt <- paste(stmt, ";");
    rs <- dbSendQuery(database, stmt);
    dbClearResult(rs);
    dbDisconnect(database);
}

gen.strat <- function(interval=NA)
{
    ## T1 <- abs(rdsct.exp(1, 0.8)) + 15;
    ## T1 <- 15 + floor(rexp(n=1, rate=1/2));
    T1 <- sample(15:60, size=1);
    holding <-c(rep(0, length(symbols)), 1);
    params <- list(T1=T1, L=1, sharpe.min=0.1);
    return(list(params=params, holding=holding));
}

sample.strats <- function(n)
{
    strats.new <- vector("list", n);
    database = dbConnect(
        MySQL(), user='sinbaski', password='q1w2e3r4',
        dbname='market', host="localhost"
    );
    rs <- dbSendQuery(
        database,
        paste(
            "select T1, sum(ret) as score",
            "from StratsStat",
            "group by T1",
            "order by score desc;"
        )
    );
    D <- fetch(rs, n=-1);
    dbClearResult(rs);
    dbDisconnect(database);
    if (min(D$score) == 0) {
        weights <- 2^(D$score * 1.0e+3);
    } else {
        weights <- sqrt(D$score-min(D$score));
    }
    Ts <- sample(D$T1, size=n, replace=TRUE, prob=weights);

    ## if (sum(D$score > 0) >= 1) {
    ##     if (sum(D$score > 0) == 1) {
    ##         Ts <- rep(D$T1[D$score > 0], n);
    ##     } else {
    ##         Ts <- sample(D$T1[D$score > 0], size=n,
    ##                      replace=TRUE, prob=D$score[D$score > 0]);
    ##     }
    ## } else if (sum(D$score == 0) >= 1) {
    ##     if (sum(D$score == 0) == 1) {
    ##         Ts <- rep(D$T1[D$score == 0], n);
    ##     } else {
    ##         Ts <- sample(D$T1[D$score == 0], size=n, replace=TRUE);
    ##     }
    ## } else { ## all scores are negative
    ##     Ts <- sample(D$T1, size=n, replace=TRUE, prob=max(D$score)/D$score);
    ## }
    for (i in 1:length(strats.new)) {
        mutation <- rdsct.exp(1, mut.rate);
        T1 <- max(T1.min, Ts[i] + mutation);
        strats.new[[i]]$params <- list(T1=T1, L=1, sharpe.min=0.1);
        strats.new[[i]]$holding <- c(rep(0, p), 1);
    }
    return(strats.new);
}

kendall <- function(positions)
{
    if (length(positions) < 10) return(NA);
    n <- min(dim(positions)[1], dim(prices)[1]-1, floor(10/length(symbols)));

    X <- apply(tail(prices, n=n+1), MARGIN=2,
               FUN=function(x) {
                   tail(x, n=-1)/head(x, n=-1) - 1
               });
    ret <- as.vector(X);
    pos <- as.vector(tail(positions, n));
    if (length(unique(ret)) > 1 && length(unique(pos)) > 1) {
        return(cor(pos, ret, method="kendall"));
    } else {
        return(0);
    }
}

get.data <- function(assets, day1, day2)
{
    if (use.database) {
        database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                             dbname='market', host="localhost");
        stmt <- sprintf("select %s_%s.closing", assets[1], time.span);
        for (i in 2:length(assets)) {
            stmt <- paste(stmt,
                          sprintf("%s_%s.closing", assets[i], time.span),
                          sep=",");
        }
        stmt <- paste(stmt, sprintf("from %s_%s", assets[1], time.span));
        for (i in 2:length(assets)) {
            stmt <- paste(stmt,
                          sprintf("%s_%s", assets[i], time.span),
                          sep=" join ");
        }
        stmt <- paste(
            stmt,
            sprintf("on %1$s_%3$s.tm = %2$s_%3$s.tm",
                    assets[1], assets[2], time.span)
        );
        if (length(assets) > 2) {
            for (i in 2:(length(assets)-1)) {
                stmt <- paste(
                    stmt,
                    sprintf("%1$s_%3$s.tm = %2$s_%3$s.tm",
                            assets[i], assets[i+1], time.span),
                    sep=" and "
                )
            }
        }
        stmt <- paste(
            stmt,
            sprintf(
                "where %s_%s.tm between '%s' and '%s'",
                assets[1], time.span, day1, day2
            ),
            sprintf("order by %s_%s.tm;", assets[1], time.span)
        );
        rs <- dbSendQuery(database, stmt);
        data <- fetch(rs, n=-1);
        dbClearResult(rs);
        dbDisconnect(database);
        return(list(prices=as.matrix(data)));
    } else {
        i <- which(days == day1);
        j <- which(days == day2);
        prices <- price.data[i:j, 3, ];
        return(list(prices=prices));
    }
}

update.prices <- function(tm)
{
    T <- sapply(1:length(strats), FUN=function(i) {
        pa <- strats[[i]]$params;
        pa$T1 * pa$L;
    });
    T.max <- max(T);
    data.new <- get.data(symbols, days[tm - T.max + 1], days[tm]);
    P <- data.new$prices;
    return(P);
}

symbols <- c(
    ## "spy",  ## S&P 500
    ## "dia",  ## Dow Jones
    ## "qqq",  ## Nasdaq
    ## "ezu",  ## Euro zone equities
    ## "ewg",  ## Germany
    ## "ewj"  ## Japan
    ## "ewl",  ## Switzerland
    ## "ewn",  ## Netherlands
    ## "ewp",  ## Spain
    ## "ewq",  ## France
    ## "ewu",  ## UK
    "uup",  ## USD
    ## "fxb",  ## British pound
    ## "fxc",  ## Canadian dollar
    "fxe",  ## euro
    "fxy",  ## Japanese yen
    ## "goog", ## Google
    ## "aapl",    ## Apple inc.
    ## "iau",  ## gold
    ## "slv",  ## silver
    ## "uso",  ## US oil fund
    ## "ung"   ## US natural gas fund
    ## "dba",
    ## "jjg",
    ## "corn",
    ## "weat",
    ## "soyb"
    ## "cane"
    ## "nib"
    "vxx"  ## SP500 short term volatility
    ## "vixy", ## SP500 short term volatility
    ## "vxz",  ## SP500 mid term volatility
    ## "viix"  ## SP500 short term volatility
);

use.database <- TRUE;
if (!use.database) {
    load(file="DailyPrices.RData");
} else {
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                         dbname='market', host="localhost");

    ## spy, ewg, ewj
    ## rs <- dbSendQuery(database,
    ##                   paste(sprintf("select tm from %s_daily ", symbols[1]),
    ##                         "where tm between '2016-01-02' and '2018-03-16';"
    ##                         ));
    ## iau, slv
    ## rs <- dbSendQuery(database,
    ##                   paste(sprintf("select tm from %s_daily ", symbols[1]),
    ##                         "where tm between '2016-01-02' and '2018-03-20';"
    ##                         ));

    ## uup, fxe, fxy
    rs <- dbSendQuery(database,
                      paste(sprintf("select tm from %s_daily ", symbols[1]),
                            "where tm between '2009-01-30' and '2018-04-12';"
                            ));

    ## jjg, weat, soyb
    ## rs <- dbSendQuery(database,
    ##                   paste(sprintf("select tm from %s_daily ", symbols[1]),
    ##                         "where tm between '2011-09-19' and '2018-04-02';"
    ##                         ));

    days <- fetch(rs, n=-1)[[1]];
    dbClearResult(rs);

    rs <- dbSendQuery(database, "drop table if exists StratsRet;");
    dbClearResult(rs);
    stmt <- paste(
        "create table StratsRet (",
        "tm date, T1 tinyint unsigned, sm float, ret double);"
    );
    rs <- dbSendQuery(database, stmt);
    dbClearResult(rs);

    rs <- dbSendQuery(database, "drop view if exists StratsStat;");
    dbClearResult(rs);

    rs <- dbSendQuery(
        database,
        paste(
            "create view StratsStat as",
            "select distinct A.tm, A.T1, A.ret from StratsRet as A join (",
            "  select distinct tm from StratsRet order by tm desc limit ",
            resample.period, ") as B on A.tm = B.tm"
        ));
    dbClearResult(rs);

    rs <- dbSendQuery(database, "drop table if exists trade_log;");
    dbClearResult(rs);

    stmt <- paste(
        "create table trade_log (",
        "tm date primary key,",
        "worth double"
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
}

require(foreach);
require(doParallel);
## require(doMC);
cl <- makeCluster(num.cores);
registerDoParallel(cl);

V <- rep(1, length(days));
V.max <- rep(1, length(days));
DD <- rep(NA, length(days));
wealth <- rep(1, length(days));
wealth.max <- rep(1, length(days));


t0 <- 80;
## t0 <- 410;
t1 <- t0;
V[t0] <- 1;
p <- length(symbols);

for (i in 1:length(strats)) {
    strats[[i]] <- gen.strat();
}
assets <- matrix(
    rep(c(rep(0, length(symbols)), V[t0]), 2),
    nrow=2, ncol=1+p,
    byrow=TRUE
);
status <- list(lookback=T1.min);
for (tm in t0:length(days)) {
    ## update prices
    prices <- update.prices(tm);

    V[tm] <- sum(tail(prices, n=1) * assets[1, 1:p]) + assets[1, p+1];
    V.max[tm] = max(V.max[tm - 1], V[tm]);
    wealth[tm] <- sum(tail(prices, n=1) * assets[2, 1:p]) + assets[2, p+1];
    wealth.max[tm] <- max(wealth.max[tm - 1], wealth[tm]);
    DD[tm] <- 1 - wealth[tm]/wealth.max[tm];

    W <- rep(NA, length(strats));
    ret <- rep(NA, length(strats));
    for (i in 1:length(strats)) {
        H <- strats[[i]]$holding;
        W[i] <- sum(H[1:p] * prices[dim(prices)[1], ]) + H[p+1];
        x <- sum(H[1:p] * prices[dim(prices)[1]-1, ]) + H[p+1];
        ret[i] <- W[i]/x - 1;
    }
    stopifnot(max(ret) < 1 && min(ret) > -1);
    save.stats(days[tm], ret);

    cat("\n", sprintf("On day %d, %s, DD=%.3f, value = %.3f, wealth = %.3f\n",
                      tm, days[tm], DD[tm], V[tm], wealth[tm]));
    cat(sprintf("    ret: %.3fe-4\n", (V[tm]/V[tm-1] - 1) * 1.0e+4));
    cat("    ret quantiles: ",
        quantile(ret, probs=c(0.02, 0.16, 0.5, 0.84, 0.98)), "\n"
        );
    cat(sprintf("    E(W): %.3f (%.3fe-2)\n", mean(W), (mean(W) - 1) * 100));
    if (tm - t0 >= resample.period) {
        N <- round(length(strats) * 0.99);
        strats[1:N] <- sample.strats(N);
        for (i in (N+1):length(strats)) {
            strats[[i]] <- gen.strat();
        }
        prices <- update.prices(tm);
    }
    T1 <- sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1);
    Q <- quantile(T1, probs=c(0.05, 0.95));
    cat("    T1: ", min(T1), Q[1], mean(T1), Q[2], max(T1), "\n");

    outcome <- trade(days[tm]);
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- outcome$holding[i, ];
    }
    assets[1, ] <- outcome$assets;
    stopifnot(
        abs(sum(tail(prices, n=1) * assets[1, 1:p]) + assets[1, p+1] - 1) < 1.0e-9
    );


    assets[2, ] <- assets[1, ];

    assets[1, ] <- assets[1, ] * V[tm];
    assets[2, ] <- assets[2, ] * wealth[tm];
    send.trade(days[tm], wealth[tm], assets[2, ]);
}
stopCluster(cl);

