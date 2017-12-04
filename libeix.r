erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1;

fit.dist <- function(data)
{
    fit <- list(bic=Inf);

    mu <- mean(data);
    sig <- sd(data);

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
            sig=params$par[2]
            );
    }

    ## non-central t distribution
    nu <- 2*sig^2/(sig^2 - 1);
    df <- if (nu > 0) nu else 1.5;
    params <- optim(
        fn=function(arg) -sum(dt(x=data - arg[1], df=arg[2], log=TRUE)),
        par=c(mu, df),
        lower=c(min(data), 1),
        upper=c(max(data), 10),
        method="L-BFGS-B"
    );
    if (params$convergence == 0) {
        bic <- 2*log(length(data)) + 2 * params$value;
        if (fit$bic > bic) {
            fit <- list(
                dist="t",
                bic=bic,
                mu=params$par[1],
                df=params$par[2]
            );
        }
    }
    return(fit);
}

fit.garch <- function(data, max.order=c(3,3))
{
    score <- Inf;
    result <- {};
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

## fit.OU <- function(ts)
## {
##     minus.log.lik <- function(theta, sig)
##     {
##         -sum(dcOU(x=tail(ts, n=-1), Dt=1, x0=head(ts, n=-1), theta=c(0, theta, sig), log=TRUE))
##     }
##     tmp <- tail(ts, n=-1)/head(ts, n=-1);
##     theta.init <- -log(mean(tmp[tmp > 0]));
##     if (theta.init <= 0) return list(fitted=FALSE);
##     sig.init <- sd(diff(R));
    
##     fit <- trycatch(
##         expr={
##             params <- optim(par=c(theta.init, sig.init),
##                             fn=function(arg) minus.log.lik(arg[1], arg[2]),
##                             method="L-BFGS-B",
##                             lower=c(theta.init/10, sig.init/10),
##                             upper=c(theta.init*4, sig.init*4),
##                             control=list(factr=0.001, maxit=500)
##                             );
##             if (params$convergence != 0) list(fitted=FALSE) else list(fitted=TRUE, params$par);
##         }, error=function(e) {
##             list(fitted=FALSE);
##         }
##     );
##     return(fit);
## }

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
expected.shortfall <- function(params, alpha)
{
    integral <- integrate(f=function(x) qnorm(p=x, mean=params[1], sd=params[2]), lower=0, upper=alpha);
    return(-integral$value/alpha);
}

