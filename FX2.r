library("nortest");
rm(list=ls());
source("/home/lxb353/hdrive/work/r/libxxie.r");

currencies <- c(
    "AUD_SEK_Rates",
    "CAD_SEK_Rates",
    ## "CHF_SEK_Rates",

    "CNY_SEK_Rates",
    "CZK_SEK_Rates",
    "DKK_SEK_Rates",

    "EUR_SEK_Rates",
    "GBP_SEK_Rates",
    "HKD_SEK_Rates",

    "HUF_SEK_Rates",
    "JPY_SEK_Rates",
    "KRW_SEK_Rates",

    "MAD_SEK_Rates",
    "MXN_SEK_Rates",
    "NOK_SEK_Rates",

    "NZD_SEK_Rates",
    ## "PLN_SEK_Rates",
    ## "SAR_SEK_Rates",

    "SGD_SEK_Rates",
    ## "THB_SEK_Rates",
    ## "TRY_SEK_Rates",

    "USD_SEK_Rates"
    );

names <- c(
    "AUD",
    "CAD",
    ## "CHF",

    "CNY",
    "CZK",
    "DKK",

    "EUR",
    "GBP",
    "HKD",

    "HUF",
    "JPY",
    "KRW",

    "MAD",
    "MXN",
    "NOK",

    "NZD",
    ##"SAR",
    "SGD",
    "USD"
    );

prices <- getAssetPrices("2010-01-04", "2016-04-01",
                         currencies, 1,
                         "rate", "localhost");
ret <- diff(log(prices));
n <- dim(ret)[1];
p <- dim(ret)[2];
h <- 20;
m <- 3;
lookback <- m*h;
I <- 8;
N <- 4;
components <- 1:N;

actions <- rep(0, n - lookback + 1);
portfolio <- rep(NA, p);
wealth <- 1;
W <- rep(-1, n - lookback + 1);
for (i in  lookback : n) {
    if (i %% h == 0) {
        R <- ret[(i - lookback + 1):i, ];
        E <- eigen(cov(R));
        Y <- R %*% E$vectors;
        X <- R[, I];
        model <- lm(X ~ Y[, 1:N]);
        composition <- E$vectors[, 1:N] %*% model$coefficients[components + 1];
    }
    ## F <- ret[(i - h + 1):i, ] %*% E$vectors[, 1:N];
    ## Z <- F %*% model$coefficients[2:5];
    Z <- ret[(i - h + 1):i, ] %*% composition;
    S1 <- cumsum(Z);
    S2 <- cumsum(ret[(i - h + 1) : i, I]);
    D <- S2 - S1;
    dev <- sd(S2 - S1);

    if (D[h] > 2 * dev && is.na(portfolio[1])) {
        actions[i - lookback + 1] <- -1;

        portfolio[I] <- -wealth / 2 / prices[(i+1), I];
        portfolio[-I] <- rep(0, p-1);
        portfolio <- portfolio + wealth / 2 * (composition/sum(abs(composition))/prices[i+1,]);
        wealth <- wealth - drop(prices[i+1, ] %*% portfolio);

    } else if (D[h] < -2 * dev && is.na(portfolio[1])) {
        actions[i - lookback + 1] <- 1;

        portfolio[I] <- wealth / 2 / prices[(i+1), I];
        portfolio[-I] <- rep(0, p-1);
        portfolio <- portfolio + (wealth / 2) * (-composition/sum(abs(composition))/prices[i+1,]);
        wealth <- wealth - drop(prices[i+1, ] %*% portfolio);

    } else if (!is.na(portfolio[1])) {
        k <- max(which(actions != 0));
        if (actions[k] == 1 && D[h] > 0) {
            actions[i - lookback + 1] <- -1;
            wealth <- wealth + sum(portfolio * prices[i+1, ]);
            portfolio[1] <- NA;
        } else if (actions[k] == -1 && D[h] < 0) {
            actions[i - lookback + 1] <- 1;
            wealth <- wealth + sum(portfolio * prices[i+1, ]);
            portfolio[1] <- NA;
        }
    }
    W[i - lookback + 1] <- wealth;
}
J <- which(actions != 0);
## l <- n - lookback + 1;
## plot(1:l, cumsum(N), type="l");

## E <- eigen(cov(ret));
## D <- eigen(cov(abs(ret)));
## F <- eigen(cov(ret^2));

## m <- floor(n/2);
## E <- eigen(cov(ret[1:m, ]));
## Y <- ret %*% E$vectors;
## X1 <- ret[1:m, 1];
## model <- lm(X1 ~ Y[(1:m), 1:N]);
## Z <- Y[(m+1):n, 1:N] %*% model$coefficients[2:5];
## S1 <- cumsum(Z);
## S2 <- cumsum(ret[(m+1):n, 1]);
## M <- lm(S2 ~ S1);

## plot((m+1):n, M$fitted.values, type="l", ylim=c(min(c(S1, S2)), max(c(S1, S2))));
## lines((m+1):n, S2, col="#0000FF");

