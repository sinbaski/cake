rm(list=ls());
graphics.off();
require(RMySQL);
require(abind);
source("~/cake/libeix.r");

set.seed(0);

population.size <- 500;
T1.min <- 15;
T1.max <- 45;
loss.tol <- 5.0e-3;
time.span <- "daily";

num.cores <- 6;


propose.trade <- function(T1, exposure.max, Y, E)
{
    p <- dim(Y)[2];
    K <- 1;
    while(K <= p && E$values[K] > 1.0e-6) K <- K + 1;
    K <- K - 1;
    metrics <- matrix(NA, nrow=K, ncol=4);
    SR <- function(mu, sig) {
        I <- integrate(
            f=function(x) x * dnorm(x, mean=mu, sd=sig),
            lower=if (mu > 0) -Inf else 0,
            upper=if (mu > 0) 0 else Inf,
            rel.tol=1.0e-2
        );
        ## score <- mu + params$gda * I$value;
        score <- mu/sig;
        if (mu > 0) {
            es <- abs(I$value)/pnorm(0, mean=mu, sd=sig);
        } else {
            es <- abs(I$value)/(1 - pnorm(0, mean=mu, sd=sig));
        }
        return(c(score, es));
    }
    d <- floor(log(dim(Y)[1]));
    for (i in 1:K) {
        model <- fit.arma(Y[, i], order.max=c(d, d), include.mean=NA);
        prediction <- predict(model, n.ahead=1);
        if (length(model$coef) > 0) {
            T <- tryCatch(t.test(Y[, i]), error=function(e) list(p.value=1));
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
        return(list(comb=rep(0, p), sharpe=0));
    }
    i <- which.max(abs(metrics[, 3]));
    exposure <- min(
        loss.tol/metrics[i, 4],
        exposure.max/sum(abs(E$vectors[, i]))
    );
    shares <- sign(metrics[i, 1]) * exposure * E$vectors[, i]/tail(prices, n=1);
    return(list(comb=shares, sharpe=metrics[i, 3]));
}

factor.algo <- function(T1, exposure.max)
{
    p <- dim(prices)[2];
    n <- dim(prices)[1];
    idx <- rev(seq(from=n, by=-1, length.out=T1));
    shares <- rep(0, p);

    S <- prices[idx, ];
    ret <- apply(S, MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
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
    d <- floor(log(n));
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
    forecast <- matrix(NA, nrow=p, ncol=2);
    for (i in 1:p) {
        if (use.database) {
            stmt <- sprintf(paste(
                "select high, low, closing from %s_%s where",
                "datediff('%s', tm) >= 0 order by tm desc limit %d;"),
                symbols[i], time.span, thedate, dim(S)[1]);
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
        model.ret <- fit.arma(ret[, i], order.max=c(d,d), include.mean=NA);
        if (length(coef(model.ret)) >= 1) {
            forecast[i, 1] <- predict(model.ret, n.ahead=1)$pred[1];
        } else {
            forecast[i, 1] <- 0;
        }
        model.vol <- fit.arma(vl[, i], order.max=c(d,d), include.mean=NA);
        if (length(coef(model.vol)) >= 1) {
            forecast[i, 2] <- exp(predict(model.vol, n.ahead=1)$pred[1]);
        } else {
            forecast[i, 2] <- exp(mean(vl[, i]));
        }
    }
    if (use.database) {
        dbDisconnect(database);
    }
    C <- cor(ret);
    outer <- forecast[, 2] %*% t(forecast[, 2]);
    C <- C * outer;
    return(list(mean=forecast[, 1], cov.mtx=C));
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
    W <- rep(1, length(strats));


    timescales <- unique(
        sapply(1:length(strats), FUN=function(i) strats[[i]]$params$T1)
    );
    clusterExport(cl, c("prices", "strats", "exposure.max",
                        "factor.algo", "propose.trade",
                        "fit.arma", "loss.tol"));
    proposals <- parLapply(
        cl=cl, X=timescales, fun=function(l) {
            factor.algo(l, exposure.max);
        }
    );
    holding <- t(sapply(X=1:length(strats), FUN=function(i) {
        k <- which(timescales == strats[[i]]$params$T1);
        if (abs(proposals[[k]]$sharpe) > strats[[i]]$params$sharpe.min) {
            shares <- proposals[[k]]$comb * W[i];
            H <- rep(NA, p+1);
            H[p+1] <- W[i] - sum(shares * tail(prices, n=1));
            H[1:p] <- shares;
            return(H);
        } else {
            return(c(rep(0, p), W[i]));
        }
    }));
    loading <- matrix(NA, nrow=length(strats), ncol=p);
    for (i in 1:dim(loading)[1]) {
        loading[i, ] <- holding[i, 1:p] * prices[n, ];
    }
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
        ## scaling w.r.t. H
        scaling <- min(risk.tol/abs(es), exposure.max)/exposure;
        holding[, 1:p] <- holding[, 1:p] * scaling;
        holding[, p+1] <- holding[, 1+p] +
            (1 - scaling) * apply(loading, MARGIN=1, FUN=sum);
        loading <- loading * scaling;
        H <- H * scaling;
    }
    return(list(holding=holding, loading=loading, worth=worth));
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
    if (is.na(interval)) {
        ## rate <- -log(1 - 0.5^(1/population.size))/(T1.max - T1.min);
        rate <- -log(1 - 0.95^(1/population.size))/(60 - T1.min);
        T1 <- round(rexp(n=1, rate=rate)) + T1.min;
    } else {
        T1 <- round(runif(n=1, min=interval[1], max=interval[2]));
    }

    ## X = sharpe.min
    ## P(X <= 1) = 0.99
    ## E(X) = 0.3
    sharpe.min <-rgamma(n=1, shape=3.922878, rate=13.07631);
    holding <-c(rep(0, length(symbols)), 1);
    params <- list(T1=T1, sharpe.min=sharpe.min);
    return(list(params=params, holding=holding));
}

sample.strats <- function(n, probs)
{
    strats.new <- vector("list", length=n);
    for (i in 1:n) {
        parents <- sample(
            1:length(strats), size=2,
            prob=probs, replace=TRUE
        );
        f <- parents[1];
        m <- parents[2];

        ## probability of not changing is 0.7
        ## sig <- 0.4824237;
        ## sig <- 0.5;
        ## mutation <- round(rnorm(1, mean=0, sd=sig));
        mutation <- sample(c(-1, 0, 1), size=1, prob=c(0.1, 0.8, 0.1));
        val <- max(T1.min, strats[[f]]$params$T1 + mutation);
        strats.new[[i]]$params$T1 <- val;
        strats.new[[i]]$params$sharpe.min <- strats[[m]]$params$sharpe.min;
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
        strats[[i]]$params$T1
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
    ## "uup",  ## USD
    ## "fxb",  ## British pound
    ## "fxc",  ## Canadian dollar
    ## "fxe",  ## euro
    ## "fxy",  ## Japanese yen
    ## "goog", ## Google
    ## "aapl",    ## Apple inc.
    ## "iau",  ## gold
    ## "slv",  ## silver
    ## "uso",  ## US oil fund
    ## "ung"   ## US natural gas fund
    ## "dba",
    ## "jjg",
    "weat",
    ## "soyb",
    "cane",
    "corn",
    "nib"
    ## "vxx"  ## SP500 short term volatility
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
    ##                   paste(sprintf("select tm from %s_%s ", symbols[1], time.span),
    ##                         "where tm between '2011-09-19' and '2018-03-16';"
    ##                         ));

    ## cane, corn, nib, weat
    rs <- dbSendQuery(database,
                      paste(sprintf("select tm from %s_%s ", symbols[1], time.span),
                            "where tm between '2011-09-19' and '2018-04-02';"
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

t0 <- 237;
t1 <- t0;
V[t0] <- 1;
p <- length(symbols);
exposure.max <- 1;
strats <- vector("list", length=population.size);
for (i in 1:length(strats)) {
    strats[[i]] <- gen.strat();
}
HHistory <- {};
assets <- matrix(
    rep(c(rep(0, length(symbols)), V[t0]), 2),
    nrow=2, ncol=1+p,
    byrow=TRUE
);
ret.fcst <- rep(NA, length(days));
status <- list(lookback=T1.min, t1=t0);
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
    timescales <- rep(NA, length(strats));
    for (i in 1:length(strats)) {
        H <- strats[[i]]$holding;
        W[i] <- sum(H[1:p] * prices[dim(prices)[1], ]) + H[p+1];
        x <- sum(H[1:p] * prices[dim(prices)[1]-1, ]) + H[p+1];
        ret[i] <- W[i]/x - 1;
        timescales[i] <- strats[[i]]$params$T1;
    }
    if (length(unique(ret)) == 1) {
        weights <- rep(1, length(strats));
    } else {
        ## weights <- exp((ret - mean(ret))/(10 * sd(ret)));
        ## weights <- exp(5e+4 * sd(ret) * ret);
        weights <- exp(20 * ret);
    }

    if (tm - t0 >= status$lookback + 1) {
        R <- sapply((tm-status$lookback+1):tm, FUN=function(k) {
            V[k]/V[k-1] - 1;
        });
        ret.fcst[tm] <- mean(R);
    } else {
        ret.fcst[tm] <- NA;
    }

    tau <- kendall(HHistory);
    Q <- quantile(timescales, probs=c(0.5, 0.95));
    cat("\n", sprintf("On day %d, %s, DD=%.3f, value = %.3f, wealth = %.3f\n",
                      tm, days[tm], DD[tm], V[tm], wealth[tm]));
    cat(sprintf("    ret: %.3fe-2, %.3fe-2\n", mean(ret)*100, sd(ret)*100));
    cat("    ret quantiles: ",
        quantile(ret, probs=c(0.02, 0.16, 0.5, 0.84, 0.98)), "\n"
        );
    cat("    weights quantiles: ",
        quantile(weights, probs=c(0.02, 0.16, 0.5, 0.84, 0.98)), "\n"
        );
    cat(sprintf("    E(W): %.3f (%.3fe-2)\n", mean(W), (mean(W) - 1) * 100));
    cat("    coverage: ", min(timescales), Q[1],
        mean(timescales), Q[2], max(timescales), "\n");
    cat("    tau: ", tau, "\n");
    cat("    forecast: ", ret.fcst[tm], "\n");
    cat("    leverage: ", exp(1000 * ret.fcst[tm]), "\n");

    ## if (!is.na(ret.fcst[tm]) && ret.fcst[tm] < 0 &&
    ##     tm - t1 > status$lookback && ) {
    ##     cat("    expand coverage.\n");
    ##     ## cat("    regenerate.\n");
    ##     N <- round(length(strats) * 0.1);
    ##     for (i in 1:N) {
    ##         strats[[i]] <- gen.strat(interval=c(T1.min, ));
    ##     }
    ##     strats[(N+1):length(strats)] <- sample.strats(length(strats)-N, weights);
    ##     prices <- update.prices(tm);
    ##     t1 <- tm;
    ## } else if (tm > t0) {
    ##     strats <- sample.strats(length(strats), weights);
    ##     prices <- update.prices(tm);
    ## }

    strats <- sample.strats(length(strats), weights);
    prices <- update.prices(tm);

    outcome <- trade(days[tm]);
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- outcome$holding[i, ];
    }
    assets[1, ] <- apply(outcome$holding, MARGIN=2, FUN=mean)/outcome$worth;
    stopifnot(
        abs(sum(tail(prices, n=1) * assets[1, 1:p]) + assets[1, p+1] - 1) < 1.0e-9
    );

    if (length(HHistory) == 0) {
        HHistory <- apply(outcome$loading, MARGIN=2, FUN=mean);
    } else {
        HHistory <- rbind(
            HHistory,
            apply(outcome$loading, MARGIN=2, FUN=mean)/outcome$worth
        );
    }
    save.stats(days[tm], ret);

    if (is.na(ret.fcst[tm])) {
        assets[2, ] <- c(rep(0, p), 1);
    } else {
        assets[2, 1:p] <- assets[1, 1:p] * exp(ret.fcst[tm] * 1000);
        assets[2, p+1] <- 1 - sum(tail(prices, n=1) * assets[2, 1:p]);
    }
    assets[1, ] <- assets[1, ] * V[tm];
    assets[2, ] <- assets[2, ] * wealth[tm];
    send.trade(days[tm], wealth[tm], assets[2, ]);
}
stopCluster(cl);
