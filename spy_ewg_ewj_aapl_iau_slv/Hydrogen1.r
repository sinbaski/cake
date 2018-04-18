rm(list=ls());
graphics.off();
require(RMySQL);
require(abind);
source("~/cake/libeix.r");

algo.arma.single <- function(params, exposure.max, Y)
{
    SR <- function(mu, sig) {
        I <- integrate(
            f=function(x) x * dnorm(x, mean=mu, sd=sig),
            lower=if (mu > 0) -Inf else 0,
            upper=if (mu > 0) 0 else Inf,
            rel.tol=1.0e-2
        );
        score <- mu + params$gda * I$value;
        ## score <- mu/sig;
        if (mu > 0) {
            es <- abs(I$value)/pnorm(0, mean=mu, sd=sig);
        } else {
            es <- abs(I$value)/(1 - pnorm(0, mean=mu, sd=sig));
        }
        return(c(score, es));
    }
    d <- floor(log(length(Y)));
    metrics <- rep(NA, 4);
    model <- fit.arma(Y, order.max=c(d, d), include.mean=NA);
    prediction <- predict(model, n.ahead=1);
    if (length(model$coef) > 0) {
        mu <- prediction$pred[1];
        sig <- prediction$se[1];
        metrics[1:2] <- c(mu, sig);
        metrics[3:4] <- SR(mu, sig);
    } else {
        return(0);
    }
    exposure <- min(params$loss.tol/metrics[4], exposure.max);
    shares <- 0;
    ## T <- tryCatch(t.test(Y), error=function(e) list(p.value=1));
    if (abs(metrics[3]) > params$score.min &&
        abs(metrics[1]/metrics[2]) > params$sharpe.min) {
        i <- which(params$include);
        shares <- sign(metrics[1]) * exposure / tail(prices[, i], n=1);
        if (shares < 0 && params$LO[i]) {
            shares <- 0;
        }
    }
    return(shares);
}

algo.arma <- function(params, exposure.max, Y, E)
{
    p <- dim(Y)[2];
    K <- 1;
    while(K <= p && E$values[K] > 1.0e-6) K <- K + 1;
    K <- K - 1;
    ## mean, sd, GDA score, expected loss when we are in loss for each pound exposed.
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
        return(rep(0, p));
    }
    i <- which.max(abs(metrics[, 3]));
    shares <- rep(0, p);
    if (abs(metrics[i, 3]) > params$score.min &&
        abs(metrics[i, 1]/metrics[i, 2]) > params$sharpe.min) {
        exposure <- min(params$loss.tol/metrics[i, 4],
                        exposure.max/sum(abs(E$vectors[, i])));
        shares <- sign(metrics[i, 1]) * exposure *
            E$vectors[, i]/tail(prices[, params$include], n=1);
        shares[shares < 0 & params$LO[params$include]] <- 0;
    }
    return(shares);
}

## factor.algo.2 <- function(params, holding, exposure.max)
factor.algo <- function(params, wealth, exposure.max)
{
    p <- dim(prices)[2];
    n <- dim(prices)[1];
    if (wealth <= 0) {
        return(c(rep(0, p), wealth));
    }
    idx <- rev(seq(from=n - params$T2 + 1, by=-params$T2, length.out=params$T1));
    if (sum(params$include) >= 2) {
        shares <- rep(0, p);
        if (params$T2 > 1) {
            S <- t(sapply(idx, FUN=function(k) {
                apply(prices[k:(k + params$T2 - 1), params$include], MARGIN=2, FUN=mean);
            }));
        } else {
            S <- prices[idx, params$include];
        }
        ret <- apply(S, MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
        C <- cov(ret);
        E <- eigen(C);
        Y <- ret %*% E$vectors;
        s <- algo.arma(params, exposure.max, Y, E);
        shares[params$include] <- s;
    } else if (sum(params$include) == 1) {
        shares <- rep(0, p);
        i <- which(params$include);
        S <- sapply(idx, FUN=function(k) {
            mean(prices[k:(k + params$T2 - 1), i]);
        });
        Y <- tail(S, n=-1)/head(S, n=-1) - 1;
        s <- algo.arma.single(params, exposure.max, Y);
        shares[i] <- s;
    } else {
        shares <- rep(0, p);
    }
    shares <- shares * wealth;
    holding <- rep(NA, p+1);
    holding[p+1] <- wealth - sum(shares * tail(prices, n=1));
    holding[1:p] <- shares;
    return(holding);
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
                "select high, low, closing from %s_daily where",
                "datediff('%s', tm) >= 0 order by tm desc limit %d;"),
                symbols[i], thedate, dim(S)[1]);
            rs <- dbSendQuery(database, stmt);
            S[, i, ] <- apply(as.matrix(fetch(rs, n=-1)), MARGIN=2, FUN=rev);
            dbClearResult(rs);
        } else {
            S[, i, ] <- tail(price.data[, , i], n=dim(S)[1]);
        }
        S[-1, i, 1] <- sapply(2:dim(S)[1], FUN=function(j) max(S[j, i, 1], S[j-1, i, 3]));
        S[-1, i, 2] <- sapply(2:dim(S)[1], FUN=function(j) min(S[j, i, 2], S[j-1, i, 3]));

        vl[, i] <- sapply(2:(n+1), FUN=function(k) log(log(S[k, i, 1]/S[k, i, 2])));
        ret[, i] <- tail(S[, i, 3], n=-1)/head(S[, i, 3], n=-1) - 1;
        model.vol <- fit.arma(vl[, i], order.max=c(d,d), include.mean=NA);
        model.ret <- fit.arma(ret[, i], order.max=c(d,d), include.mean=NA);
        forecast[i, 1] <- predict(model.ret, n.ahead=1)$pred[1];
        forecast[i, 2] <- exp(predict(model.vol, n.ahead=1)$pred[1]);
    }
    if (use.database) {
        dbDisconnect(database);
    }
    C <- cor(ret);
    outer <- forecast[, 2] %*% t(forecast[, 2]);
    C <- C * outer;
    return(list(mean=forecast[, 1], cov.mtx=C));
}

trade <- function(thedate, confidence=0.01, risk.tol=7.5e-3)
{
    p <- dim(prices)[2];
    n <- dim(prices)[1];
    WR <- sapply(1:length(strats), FUN=function(i) {
        x <- sum(strats[[i]]$holding[1:p] * prices[n-1, ]) + strats[[i]]$holding[p+1];
        y <- sum(strats[[i]]$holding[1:p] * prices[n, ]) + strats[[i]]$holding[p+1];
        c(y, x, y/x - 1);
    });
    if (length(unique(WR[3, ])) > 1) {
        ## R <- WR[3, ] * exp(abs(WR[3, ] - mean(WR[3, ]))/sd(WR[3, ]));
        R <- WR[3, ] * 100;
        W <- WR[2, ] * (R + 1);
        W <- W/sum(W) * sum(WR[1, ]);
    } else {
        W <- WR[1, ];
    }
    export <- c(
        "prices", "strats", "exposure.max",
        "factor.algo", "algo.arma", "algo.arma.single",
        "fit.arma"
    );
    holding <- foreach(i=1:length(strats), .combine=rbind, .export=export) %dopar% {
            factor.algo(strats[[i]]$params, W[i], exposure.max);
    }
    loading <- matrix(NA, nrow=length(strats), ncol=p);
    for (i in 1:dim(loading)[1]) {
        loading[i, ] <- holding[i, 1:p] * prices[n, ];
    }
    H <- apply(loading, MARGIN=2, FUN=mean);
    worth <- sum(H) + mean(holding[, p+1]);
    stopifnot(abs(worth/V[tm] - 1) < 1.0e-6);
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
    stopifnot(abs((sum(H) + mean(holding[, p+1]))/V[tm] - 1) < 1.0e-6);

    L <- which(H[1:p] > 0);
    S <- which(H[1:p] < 0);
    long <- if (length(L) > 0) sum(H[L]) else 0;
    short <- if (length(S) > 0) -sum(H[S]) else 0;
    cat(sprintf("    long %.3f, short %.3f, cash %.3f\n",
                long, short, mean(holding[, p+1])));

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
    return(list(holding=holding, loading=loading));
}

gen.strat <- function(T1.max, T2.max, include, holding)
{
    T2 <- sample(1:T2.max, size=1);
    T1.max <- min(252 %/% T2, T1.max);
    T1 <- sample(15:T1.max, size=1);

    gda <- runif(1, min=0.05, max=1);
    score.min <- runif(1, min=1.0e-3, max=1.0e-2);
    ## gda <- 1;
    sharpe.min <- runif(1, min=0.3, max=1.0);
    ## ret.conf <- runif(1, min=0.3, max=0.4);
    ## ret.conf <- runif(1, min=0.05, max=0.5);
    if (sum(is.na(include)) >= 1) {
        ## include <- sample(c(TRUE, FALSE), size=length(symbols),
        ##                   prob=c(0.5, 0.5), replace=TRUE);
        ## include <-c(TRUE, rep(FALSE, length(symbols) - 1));
        include <- rep(TRUE, length(symbols));
    } else {
        stopifnot(length(include) == length(symbols));
    }
    if (sum(is.na(holding)) >= 1) {
        holding <-c(rep(0, length(symbols)), 1);
    }
    ## i <- sample(1:length(symbols), size=1,
    ##             prob=rep(1, length(symbols)), replace=TRUE);
    ## include <- rep(FALSE, length(symbols));
    ## include[i] <- TRUE;

    LO <- rep(TRUE, length(symbols));
    ## LO <- sample(c(TRUE, FALSE), size=length(symbols),
    ##              prob=c(0.5, 0.5), replace=TRUE);
    ## loss.tol <- runif(1, min=1.0e-3, max=5.0e-3);
    loss.tol <- 5.0e-3;
    params <- list(T1=T1,
                   T2=T2,
                   gda=gda,
                   score.min=score.min,
                   sharpe.min=sharpe.min,
                   ## The following are more often fixed than not
                   include=include,
                   LO=LO,
                   loss.tol=loss.tol
                   );
    return(list(params=params, holding=holding));
}

## renew.strategies <- function(tau, sig, worth, strats)
## {
##     strats.new <- vector("list", length=length(strats));

##     fitness <- exp((tau - mean(tau))/sd(tau));
##     N <- ceiling(length(strats) * 0.85);
##     winners <- sample(1:length(strats), size=N,
##                       prob=fitness, replace=TRUE);
##     cat(sprintf("    %d (%d) selected.\n", length(unique(winners)), N));
##     strats.new[1:N] <- strats[winners];
##     ## Survivors mutate
##     holding <- c(rep(0, p), worth);
##     for (i in 1:N) {
##         include <- exp.mutation(strats[[i]]$params$include,
##                                 1/(6*period), tm - t1);
##         if (sum(strats[[i]]$params$include) > 1) {
##             include <- exp.mutation(strats[[i]]$params$include,
##                                     1/(6*period), tm - t1);
##         } else {
##             include <- switch.mutation(strats[[i]]$params$include, 7/8);
##         }
##         if (sum(include != strats[[i]]$params$include) > 0) {
##             strats.new[[i]] <- gen.strat(
##                 min(60, tm), include=include, holding=holding
##             );
##         } else {
##             l <- round(log.mutation(strats[[i]]$params$T1, sig));
##             l <- min(tm, max(l, 5));
##             strats.new[[i]]$params$T1 <- l;
##             strats.new[[i]]$params$gda <- log.mutation(strats[[i]]$params$gda, sig);
##             strats.new[[i]]$params$score.min <- log.mutation(
##                 strats[[i]]$params$score.min, sig);
##             ## strats[[i]]$params$LO <- exp.mutation(strats[[i]]$params$LO,
##             ##                                       1/(3*period), tm - t1);
##             strats.new[[i]]$holding <- holding;
##         }
##     }
##     for (i in (N+1):length(strats)) {
##         strats.new[[i]] <- gen.strat(min(60, tm), include=NA, holding=holding);
##     }
##     return(strats.new);
## }

## log.mutation <- function(x, sig)
## {
##     stopifnot(min(x) > 0);
##     x * exp(rnorm(n=length(x), mean=0, sd=sig));
## }

## exp.mutation <- function(x, rate, elapsed)
## {
##     y <- x;
##     mutations <- which(rexp(n=length(x), rate) < elapsed);
##     y[mutations] <- !y[mutations];
##     return(y);
## }

## switch.mutation <- function(x, prob)
## {
##     i <- which(x);
##     stopifnot(length(i) == 1);
##     p <- length(symbols);
##     probs <- rep(NA, p);
##     probs[i] <- prob;
##     probs[setdiff(1:p, i)] <- (1 - prob)/(p - 1);
##     j <- sample(1:p, size=1, replace=TRUE, prob=probs);
##     y <- rep(FALSE, p);
##     y[j] <- TRUE;
##     return(y);
## }

kendall <- function(positions)
{
    n <- dim(positions)[3];
    X <- apply(tail(prices, n=n+1), MARGIN=2,
               FUN=function(x) {
                   tail(x, n=-1)/head(x, n=-1) - 1
               });
    tau <- rep(NA, dim(positions)[1]);
    for (i in 1:dim(positions)[1]) {
        include <- strats[[i]]$params$include;
        pos <- as.vector(t(positions[i, include, ]));
        ret <- as.vector(X[, include]);
        if (length(unique(ret)) > 1 && length(unique(pos)) > 1) {
            tau[i] <- cor(ret, pos, method="kendall");
        } else {
            tau[i] <- 0;
        }
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
    if (use.database) {
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
    } else {
        i <- which(days == day1);
        j <- which(days == day2);
        prices <- price.data[i:j, 3, ];
        included <- 1>dim(price.data)[3];
    }
    return(list(prices=prices, included=included));
}

symbols <- c(
    "spy",  ## S&P 500
    ## "dia",  ## Dow Jones
    ## "qqq",  ## Nasdaq
    ## "ezu",  ## Euro zone equities
    "ewg",  ## Germany
    "ewj",  ## Japan
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
    "aapl",    ## Apple inc.
    "iau",  ## gold
    "slv"  ## silver
    ## "vxx",  ## SP500 short term volatility
    ## "vixy", ## SP500 short term volatility
    ## "vxz",  ## SP500 mid term volatility
    ## "viix"  ## SP500 short term volatility
);

## database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
##                      dbname='market', host="localhost");
## rs <- dbSendQuery(database,
##                   paste("select tm from spy_daily ",
##                         "where tm between '2008-03-27' and '2018-03-16';"
##                         ));
## days <- fetch(rs, n=-1)[[1]];
## dbClearResult(rs);
use.database <- FALSE;
if (!use.database) {
    load(file="DailyPrices.RData");
} else {
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                         dbname='market', host="localhost");
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

V <- rep(NA, length(days));
V.max <- rep(1, length(days));
DD <- rep(NA, length(days));

require(foreach);
require(doParallel);
## require(doMC);
cl <- makeCluster(detectCores() * 3 / 8);
registerDoParallel(cl);


## We need at least one year's worth of data
## to determine the position size
## t0 <- 252
t0 <- 478;
t1 <- t0;
exposure.max <- 1;
strats <- vector("list", length=500);
for (i in 1:length(strats)) {
    strats[[i]] <- gen.strat(
        T1.max=60,
        T2.max=1,
        include=NA,
        holding=c(rep(0, length(symbols)), 1.107654e+00)
    );
}
wealths <- rep(1.107654e+00, length(strats));
HHistory <- NA;
## period <- 20;
for (tm in t0:length(days)) {
    ## update prices
    T <- sapply(1:length(strats), FUN=function(i) {
        strats[[i]]$params$T1 * strats[[i]]$params$T2
    });
    T.max <- max(T);
    data.new <- get.data(symbols, days[tm - T.max + 1], days[tm]);
    prices <- data.new$prices;
    p <- dim(prices)[2];
    W <- sapply(1:length(strats), FUN=function(i) {
        sum(strats[[i]]$holding[1:p] * tail(prices, n=1)) + strats[[i]]$holding[p+1];
    });
    ## stopifnot(min(W) > 0);

    V[tm] <- mean(W);
    V.max[tm] = if (V[tm] > V.max[tm-1]) V[tm] else V.max[tm-1];
    DD[tm] <- 1 - V[tm]/V.max[tm];

    if (tm == t1) {
        wealths <- W;
    } else {
        wealths <- rbind(wealths, W);
    }

    cat("\n", sprintf("On day %d, %s, DD=%.3f, value = %.3f\n",
                      tm, days[tm], DD[tm], V[tm]));
    tau <- rep(NA, length(strats));
    if (tm - t1 >= 10) {
        tau <- kendall(HHistory);
        if (mean(tau) > 0.1) {
            action <- "go";
        } else {
            action <- "evaluate";
        }
        cat("    tau: ", c(min(tau), mean(tau), max(tau)), "\n");
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
            X <- t(sapply(1:length(strats), FUN=function(i) {
                outcome$loading[i, ]/W[i];
            }));
            HHistory <- X;
        } else {
            X <- t(sapply(1:length(strats), FUN=function(i) {
                outcome$loading[i, ]/W[i];
            }));
            HHistory <- abind(HHistory, X, along=3);
        }
        next;
    }
    ## Compute statistics since the last selection or regeneration
    cat(sprintf("    evaluate.\n"));

    R <- apply(wealths, MARGIN=2, FUN=function(x) tail(x, n=-1)/head(x, n=-1) - 1);
    ## sharpe may be NaN when the wealths over the entire period have not changed.
    sharpe <- sapply(1:dim(R)[2], FUN=function(i) mean(R[, i])/sd(R[, i]));
    sharpe[is.nan(sharpe)] <- 0;
    ret <- sapply(1:dim(wealths)[2], FUN=function(i)
        tail(wealths[, i], n=1)/head(wealths[, i], n=1) - 1);

    n <- dim(prices)[1];
    cat("    returns: ", c(min(ret), mean(ret), max(ret)), "\n");
    cat("    Sharpe: ", c(min(sharpe), mean(sharpe), max(sharpe)), "\n");
    ## Bad performance. Search the entire configuration space
    idx <- which.max(tau);
    cat("    Best:  ", paste(strats[[idx]]$params), "\n");
    idx <- which.min(tau);
    cat("    Worst: ", paste(strats[[idx]]$params), "\n");
    cat(sprintf("    %s:   %.3f\n", symbols[1],
                tail(prices, n=1)[1]/prices[n-(tm-t1), 1] - 1));

    H <- c(rep(0, length(symbols)), V[tm]);
    if (max(tau) < 1) {
        cat(sprintf("    regenerate.\n"));
        for (i in 1:length(strats)) {
            strats[[i]] <- gen.strat(60, 1, include=NA, holding=H);
        }
    } else {
        cat(sprintf("    select.\n"));
        sig <- 0.05 * exp(1 - V[tm]/V[t1]);
        strats <- renew.strategies(tau, sig, V[tm], strats);
    }
    outcome <- trade(days[tm]);
    for (i in 1:length(strats)) {
        strats[[i]]$holding <- outcome$holding[i, ];
    }
    HHistory <- outcome$loading;
    wealths <- rep(V[tm], length(strats));
    t1 <- tm;
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
