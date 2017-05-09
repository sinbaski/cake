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
n <- dim(ret)[1];
p <- dim(ret)[2];
I <- 8;
N <- 4;
explanatory <- 1:p;
## one month, 22 work days
h <- 22;
## regress on the past 2 years
lookback <- h * 24;
components <- 1:N;


actions <- rep(0, n - lookback + 1);
portfolio <- rep(NA, p);
wealth <- 1;
W <- rep(-1, n - lookback + 1);
garch11 <- vector("list", p);

for (i in  lookback : n) {
    if (i %% h == 0) {
        R <- tail(ret[1:i, explanatory], lookback);
        E <- eigen(cov(R));
        Y <- R %*% E$vectors;
        X <- tail(ret[1:i, I], lookback);
        model <- lm(X ~ Y[, 1:N]);
        composition <- E$vectors[, 1:N] %*% model$coefficients[1:N + 1];

        ## A GARCH(1, 1) model for each sequence. The innovations are correlated.
        res <- matrix(NA, nrow=lookback, ncol=N);
        for (j in 1:N) {
            gm <- garchFit(~garch(1,1), data=Y[, j], trace=FALSE);
            garch11[[j]] <- gm;
            res[, j] <- model@residuals / model@sigma.t;
        }
        C <- cor(res);
    }
    ## F <- ret[(i - h + 1):i, ] %*% E$vectors[, 1:N];
    ## Z <- F %*% model$coefficients[2:5];
    Z <- tail(ret[1:i, explanatory], h) %*% composition;
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

