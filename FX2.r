rm(list=ls());
require("fGarch");
source("common/libxxie.r");

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
warmup <- 400;
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
for (i in  (lookback + warmup) : n) {
    if ((i - warmup) %% h == 0) {
        R <- ret[(i - lookback + 1):i, ];
        E <- eigen(cov(R));
        Y <- R %*% E$vectors;
        X <- R[, I];
        model <- lm(X ~ Y[, 1:N]);
        composition <- E$vectors[, 1:N] %*% model$coefficients[components + 1];

        ## A GARCH(1, 1) model for each sequence. The innovations are correlated.
        garch11 <- {};
        res <- matrix(NA, nrow=i, ncol=p);
        for (j in 1:p) {
            M <- garchFit(~garch(1,1), data=(ret[1:i, j] - mean(ret[1]), trace=FALSE);
            garch11 <- c(garch11, M);
            res[, j] <- garch11[[j]]@residuals / garch11[[j]]$sigma.t;
        }
        C <- cor(res);
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

