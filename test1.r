rm(list=ls());
library(RMySQL);
library(alabama);
## library(forecast);


source("libxxie.r");

my.fun <- function(coef, R) {
    auto <- acf(R %*% coef, plot = FALSE)$acf[-1];
    p <- length(auto);
    I <- which(abs(auto) <= 2/sqrt(dim(R)[1]));
    auto[I] <- 0;
    ## auto <- auto * exp(-(0:(p-1)/10));
    return(-sum(auto^2));
}

heq <- function(coef, R) {
    return(sum(abs(coef)) - 1);
}

day2 = '2015-05-28';
day1 = '2010-01-01';

## databased located at 85.228.154.211

assetSet <- "indices";
if (length(assetSet)) {
    database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
        dbname='avanza', host=Sys.getenv("PB"));

    if (assetSet == "indices") {
        results = dbSendQuery(database, sprintf("select tblname from %s;",
            assetSet));
        tables <- fetch(results, n=-1)[[1]];
        dbClearResult(results);
    } else if (length(assetSet) && length(grep(".+_components", assetSet))) {
        results = dbSendQuery(database, sprintf("select symbol from %s;",
            assetSet));
        tables <- fetch(results, n=-1)[[1]];
        p = length(tables);
        dbClearResult(results);
        for (i in 1 : p) {
            tables[i] <- gsub("[.]", "_", tables[i]);
            tables[i] <- gsub("-", "_series_", tables[i]);
            tables[i] <- paste(tables[i], "_SE", sep="");
        }
    }
    dbDisconnect(database);
} else {
    tables <- c("DAX", "CAC40", "FTSE100", "SP500", "Nikkei225", "OMXS30");
}
p = length(tables);
                                        # data <- getAssetReturns(day1, day2, tables);
data <- getInterpolatedReturns(day1, day2, tables);
R = matrix(unlist(data[, -1]), nrow=dim(data)[1], byrow=FALSE);
T = dim(R)[1];
## C <- t(R) %*% R / T;
## E <- eigen(C);
## comb <- E$vectors[,p];
## comb <- comb /sum(abs(comb));
## ret <- R %*% comb;

A <- rep(1/p, p);
A[1] <- 1;
result <- auglag(par=A, fn=my.fun, R=R, heq=heq);
ret <- R %*% result$par;

library(rugarch);
## For the generalized hyperbolic distribution
## which is a normal mean-variance mixture, there
## exists location and scale invariant parametrization,
## namely ((lambda,zeta), rho), where (lambda, zeta)
## constitute the shape parameters and rho is the skewness
## parameter.
##
## When lambda is fixed
## zeta = delta * (alpha + beta)^{1/2}
## rho = beta/alpha
##
## For the generalized hyperbolic skewed t distribution
## alpha 
res <- inferInnovations(ret);
fitted <- fitdist(distribution="ghyp", res);
U <- pdist(distribution="ghyp", q=res,
           mu=fitted$pars[1],
           sigma=fitted$pars[2],
           skew=fitted$pars[3],
           shape=fitted$pars[4],
           lambda=fitted$pars[5]
           );
library(nortest);
ad.test(qnorm(U))

## In case the situation is complicated
##
## Akaike <- matrix(Inf, nrow=5,ncol=5);
## Baysian <- matrix(Inf, nrow=5,ncol=5);
## for (p in 1:3) {
##     for (q in 1:3) {
##         spec <- ugarchspec(mean.model=list(
##                                armaOrder=c(p, q),
##                                include.mean=FALSE),
##                            distribution.model="ghyp",
##                            variance.model=list(
##                                model="gjrGARCH",
##                                garchOrder=c(1, 1)
##                            ),
##                            fixed.pars=list(
##                                ## mu=fitted$pars[1],
##                                ## sigma=fitted$pars[2],
##                                skew=fitted$pars[3],
##                                shape=fitted$pars[4],
##                                ghlambda=fitted$pars[5]
##                            ),
##                            );
##         model <- ugarchfit(spec=spec, data=ret);
##         if (0 == convergence(model)) {
##             Akaike[p,q] <- infocriteria(model)[1];
##             Baysian[p,q] <- infocriteria(model)[2];
##         }
##     }
## }

## I <- which(Baysian==min(Baysian), arr.ind=TRUE);
spec <- ugarchspec(mean.model=list(
                       armaOrder=c(1,1),
                       include.mean=FALSE),
                   distribution.model="ghyp",
                   variance.model=list(
                       model="gjrGARCH",
                       garchOrder=c(1, 1)
                   ),
                   fixed.pars=list(
                       ## mu=fitted$pars[1],
                       ## sigma=fitted$pars[2],
                       skew=fitted$pars[3],
                       shape=fitted$pars[4],
                       ghlambda=fitted$pars[5]
                   ),
                   );
model <- ugarchfit(spec=spec, data=ret);
cluster <- makePSOCKcluster(strtoi(Sys.getenv("NumCores")));
roll <- ugarchroll(spec, ret, n.start=floor(T*0.8), refit.every=40,
                   refit.window="moving", window.size=1000,
                   solver="hybrid", calculate.VaR=TRUE,
                   VaR.alpha=0.05, cluster=cluster, keep.coef=TRUE);

stopCluster(cluster);



