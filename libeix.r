require(sde);
require(tseries);

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1;
last <- function(x) tail(x, n=1);
first <- function(x) head(x, n=1);

dir.cos <- function(x, y)
{
    sum(x * y)/sqrt(sum(x^2) * sum(y^2));
}

norm.vec <- function(x, p=2)
{
    sum(x^p)^(1/p);
}

proj.vec <- function(x, y)
{
    ## project vector x onto y
    sum(x * y) / sum(y^2) * y;
}

acf.test <- function(X, lag=1)
{
    r <- cor(head(X, n=-lag), tail(X, n=-lag));
    n <- length(X) - lag;
    T <- sqrt(n - 2) * r / sqrt(1 - r^2);
    p.value <- pt(q=T, df=n-2, lower.tail=(T < 0));
    return(list(estimate=r, p.value=p.value));
}

rdsct.exp <- function(n, q)
{
    U <- runif(n);
    S <- sample(c(-1, 1), size=n, replace=TRUE, prob=c(1, 1));
    return(S * floor(log(1-U)/log(q)));
}

## fit.arma <- function(X, order.max=c(1,1), include.mean=NA)
fit.arima <- function(X)
{
    X1 <- X;
    d <- 0;
    while (adf.test(X1)$p.value > 0.2 && d < 2) {
        X1 <- diff(X1);
        d <- d + 1;
    }
    include.mean <- d == 0 && t.test(X1)$p.value < 0.1;
    order <- max(floor(log(length(X))) - 1, 1);
    aic <- Inf;
    model <- list(aic=Inf);
    for (j in 0:order) {
        for (k in 0:order) {
            mdl <- tryCatch(
            {
                arima(X, order=c(j, d, k), include.mean=include.mean);
            }, warning=function(w) {
                list(code=-1, aic=Inf);
            }, error=function(e) {
                list(code=-1, aic=Inf);
            });
            if (mdl$aic < aic) {
                model <- mdl;
                aic <- mdl$aic;
            }
        }
    }
    return(model);
}

## fit.arma.2 <- function(X, order=c(1,1), include.mean=NA)
## {
##     bic <- Inf;
##     model <- list(bic=Inf);
##     if (is.na(include.mean)) {
##         flags <- c(FALSE, TRUE);
##     } else {
##         flags <- include.mean;
##     }
##     mdl <- vector("list", length=length(flags));
##     for (flag in flags) {
##         for (m in 1:length(mdl)) {
##             mdl[[m]] <- tryCatch(
##             {
##                 arima(X, order=c(order[1], 0, order[2]), include.mean=flag)
##             }, warning=function(w) {
##                 list(code=-1, BIC=Inf);
##             }, error=function(e) {
##                 list(code=-1, BIC=Inf);
##             });
##         }
##         bics <- c(
##             sapply(1:length(flags),
##                    function(i) if (mdl[[i]]$code == 0) BIC(mdl[[i]]) else Inf
##                    ),
##             bic
##         );
##         selected <- which.min(bics);
##         if (selected <= length(flags)) {
##             model <- mdl[[selected]];
##             bic <- bics[selected];
##         }
##     }
##     return(model);
## }

fit.dist <- function(data)
{
    fit <- list(bic=Inf);

    mu <- mean(data);
    sig <- sd(data);
    if (sig == 0) return(list(bic=Inf));

    ## First try normal distribution
    params <- optim(
        fn=function(arg) -sum(dnorm(x=data, mean=arg[1], sd=arg[2], log=TRUE)),
        par=c(mu, sig),
        lower=c(min(data), sig/2),
        upper=c(max(data), sig*2),
        method="L-BFGS-B"
    );
    if (params$convergence == 0) {
        fit <- list(
            dist="norm",
            bic=2*log(length(data)) + 2 * params$value,
            mu=params$par[1],
            sig=params$par[2],
            fun.d=function(x) dnorm(x=x, mean=params$par[1], sd=params$par[2]),
            fun.q=function(x) qnorm(p=x, mean=params$par[1], sd=params$par[2]),
            fun.p=function(x) pnorm(q=x, mean=params$par[1], sd=params$par[2])
            );
    }

    ## non-central t distribution
    ## nu <- 2*sig^2/(sig^2 - 1);
    ## df <- if (nu > 0) nu else 1.5;
    ## params <- optim(
    ##     fn=function(arg) -sum(dt(x=data - arg[1], df=arg[2], log=TRUE)),
    ##     par=c(mu, df)
    ##     ## lower=c(min(data), 1),
    ##     ## upper=c(max(data), 10),
    ##     ## method="L-BFGS-B"
    ## );
    ## if (params$convergence == 0) {
    ##     bic <- 2*log(length(data)) + 2 * params$value;
    ##     if (fit$bic > bic) {
    ##         fit <- list(
    ##             dist="t",
    ##             bic=bic,
    ##             mu=params$par[1],
    ##             df=params$par[2],
    ##             sig=if (df > 2) sqrt(df/(df - 2)) else Inf,
    ##             fun.d=function(x) dt(x=x - params$par[1], df=params$par[2]),
    ##             fun.q=function(x) qt(p=x, df=params$par[2]) + params$par[1],
    ##             fun.p=function(x) pt(q=x - params$par[1], df=params$par[2])
    ##         );
    ##     }
    ## }
    return(fit);
}

fit.garch <- function(data, max.order=c(3,3))
{
    score <- Inf;
    result <- list(bic=Inf);
    bics <- matrix(NA, nrow=max.order[1], ncol=max.order[2]);
    for (i in 1:max.order[1]) {
        for (j in 1:max.order[2]) {
            score <-  tryCatch(
                expr = {
                    mdl <- garchFit(as.formula(sprintf("~garch(%d, %d)", i, j)),
                                    data=data,
                                    trace=FALSE);
                    bics[i, j] <- mdl@fit$ics[2];
                    if (score > mdl@fit$ics[2]) {
                        par <- coef(mdl);
                        result <- list(mu=par[1], omega=par[2],
                                       alpha=par[3:(3 + i - 1)],
                                       beta=par[(3 + i):length(par)],
                                       sigma.t=tail(mdl@sigma.t, n=j),
                                       r.t=tail(data, n=i),
                                       inno=(data - par[1])/mdl@sigma.t,
                                       bic=bics[i, j]
                                       );
                    }
                    min(score, bics[i, j]);
                }, error = function(e) {
                    score;
                }
            );
        }
    }
    return(result);
}

fit.OU <- function(ts)
{
    minus.log.lik <- function(mu, theta, sig)
    {
        -sum(dcOU(x=tail(ts, n=-1), Dt=1, x0=head(ts, n=-1), theta=c(mu * theta, theta, sig), log=TRUE))
    }
    tmp <- tail(ts, n=-1)/head(ts, n=-1);
    theta.init <- -log(mean(tmp[tmp > 0]));
    if (theta.init <= 0) return(list(fitted=FALSE));
    sig.init <- sd(diff(ts));
    mu.init <- mean(ts);
    mu.lb <- min(mu.init/2, mu.init*2);
    mu.ub <- max(mu.init/2, mu.init*2);

    fit <- tryCatch(
        expr={
            params <- optim(par=c(mu.init, theta.init, sig.init),
                            fn=function(arg) minus.log.lik(arg[1], arg[2], arg[3]),
                            method="L-BFGS-B",
                            lower=c(mu.lb, theta.init/10, sig.init/10),
                            upper=c(mu.ub, theta.init*4, sig.init*4),
                            control=list(factr=0.001, maxit=500)
                            );
            if (params$convergence != 0) list(fitted=FALSE) else list(fitted=TRUE, par=params$par);
        }, error=function(e) {
            list(fitted=FALSE);
c        }
    );
    return(fit);
}

fit.BS <- function(ts)
{
    minus.log.lik <- function(mu, sig)
    {
        -sum(dcBS(x=tail(ts, n=-1), Dt=1, x0=head(ts, n=-1),
                  theta=c(mu, sig), log=TRUE))
    }
    ## X <- diff(log(ts));
    X <- tail(ts, n=-1)/head(ts, n=-1) - 1;
    sig.init <- sd(X);
    mu.init <- mean(X) + sig.init^2/2;
    fit <- tryCatch(
        expr={
            params <- optim(par=c(mu.init, sig.init),
                            fn=function(arg) minus.log.lik(arg[1], arg[2]),
                            control=list(factr=0.001, maxit=500)
                            );
            if (params$convergence != 0)
                list(fitted=FALSE)
            else list(
                     fitted=TRUE, par=params$par,
                     E.ret=function(tm) {
                         (params$par[1] - params$par[2]^2/2) * tm
                     },
                     sd.ret=function(tm) {
                         params$par[2] * sqrt(tm)
                     },
                     E.proc=function(S0, tm) {
                         S0 * exp(params$par[1]* tm)
                     },
                     sd.proc=function(S0, tm) {
                         S0 * exp(params$par[1] * tm) *
                             sqrt(exp(params$par[2]^2 * tm) - 1)
                     }
                 );
        }, error=function(e) {
            list(fitted=FALSE);
        }
    );
    return(fit);
}

fit.BM <- function(ts)
{
    n <- length(ts);
    minus.log.lik <- function(mu, sig)
    {
        s <- 0;
        for (i in 1:length(ts)) {
            s <- s + dnorm(ts[i], mean=i*mu, sd=sig * sqrt(i), log=TRUE);
        }
        -s;
    }
    X <- diff(ts);
    sig.init <- sd(X);
    mu.init <- mean(X);
    mu.lb <- min(mu.init/2, mu.init*2);
    mu.ub <- max(mu.init/2, mu.init*2);

    fit <- tryCatch(
        expr={
            params <- optim(par=c(mu.init, sig.init),
                            fn=function(arg) minus.log.lik(arg[1], arg[2]),
                            ## method="L-BFGS-B",
                            ## lower=c(mu.lb, sig.init/2),
                            ## upper=c(mu.ub, sig.init*2),
                            control=list(factr=0.001, maxit=500)
                            );
            if (params$convergence != 0) list(fitted=FALSE)
            else list(fitted=TRUE, par=params$par);
        }, error=function(e) {
            list(fitted=FALSE);
        }
    );
    return(fit);
}

fit.wave <- function(S)
{
    N <- length(S);
    mu <- mean(S);
    F <- fft(S - mu);
    k <- which.max(abs(head(F, n=floor(N/2))));
    omega <- (k-1)/N*2*pi;
    result <- optim(
        fn=function(arg) {
            ref <- arg[1] * sin(omega * (1:N) + arg[2]);
            sum(abs(S - mu - ref));
        },
        par=c(mean(abs(S - mu)), 0.01),
        lower=c(min(abs(S - mu)), -pi),
        upper=c(max(abs(S - mu)), pi),
        method="L-BFGS-B"
    );
    if (result$convergence == 0) {
        return(list(convergence=0,
                    mu=mu,
                    A=result$par[1],
                    omega=omega,
                    phi=result$par[2]));
    } else {
        return(list(convergence=result$convergence));
    }
}


cummax <- function(X)
{
    n <- length(X);
    Y <- rep(NA, n);
    val <- -Inf;
    for (i in 1:n) {
        if (X[i] > val) {
            val <- X[i];
        }
        Y[i] <- val;
    }
    return(Y);
}

## Integrate the lower-tail of the return distribution. Always positive
## params: parameters of the return distribution
expected.shortfall <- function(params, alpha, lower=TRUE)
{
    if (lower) {
        integral <- integrate(f=function(x) qnorm(p=x, mean=params[1], sd=params[2]), lower=0, upper=alpha);
    } else {
        integral <- integrate(f=function(x) qnorm(p=x, mean=params[1], sd=params[2]), lower=1-alpha, upper=1);
    }
    return(abs(integral$value/alpha));
}

MA <- function(x, span=1, fun=function(lags) rep(1, length(lags)))
{
    if (span == 1) return(x);
    w <- fun(0:(span-1));
    stopifnot(sum(w > 0) == span);
    w <- w/sum(w);
    y <- lapply(span:length(x), FUN=function(i) sum(w * x[(i-span+1):i]));
    return(c(rep(NA, span - 1), unlist(y)));
}

## Categorise the market at the absence of a clear trend
categorise.mkt <- function(S, lookback, confidence)
{
    X <- log(S);
    ret <- diff(X);
    trend <- t.test(x=ret);
    if (trend$p.value < confidence) {
        category <- if(trend$estimate > 0) "up" else "down";
        trend <- trend$estimate;
        return(list(category=category, trend=trend));
    }
    correlation <- cor.test(x=head(X, n=-1), y=ret);
    if (correlation$estimate < 0 && correlation$p.value < 0.1) {
        proc.ou <- fit.OU(X);
        if (proc.ou$fitted) {
            return(list(category="OU", par=proc.ou$par));
        }
    }
    return(list(category="unknown"));
}

save.data <- function(assets, day1, day2)
{
    price.data <- {};
    days <- {};
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                         dbname='market', host="localhost");
    for (i in 1:length(assets)) {
        stmt <- sprintf(
            "select tm, high, low, closing from %s_daily where tm between '%s' and '%s'",
            assets[i], day1, day2
        );
        rs <- dbSendQuery(database, stmt);
        X <- fetch(rs, n=-1);
        dbClearResult(rs);
        if (length(price.data) == 0) {
            price.data <- as.matrix(X[, -1]);
            days <- X$tm;
        } else {
            price.data <- abind(price.data, as.matrix(X[, -1]), along=3);
        }
    }
    save(days, price.data, file="DailyPrices.RData");
    dbDisconnect(database);
}
