rm(list=ls());
graphics.off();
require(RMySQL);
require("xts");
require("fGarch");
require("sde");
require("zoo");
require("TTR");

database = dbConnect(MySQL(), user='sinbaski', password='q1w2e3r4',
                     dbname='market', host="localhost");


stmt <- paste(
    "select ko_daily.tm as tm,",
    "ko_daily.closing as ko,",
    "pep_daily.closing as pep,",
    "spy_daily.closing as spy,",
    "xlp_daily.closing as xlp",
    "from ko_daily join pep_daily",
    "join spy_daily",
    "join xlp_daily",
    "on ko_daily.tm = pep_daily.tm",
    "and ko_daily.tm = spy_daily.tm",    
    "and ko_daily.tm = xlp_daily.tm"    
);

results <- dbSendQuery(database, stmt);
data <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

X <- data[, -1];
tm <- data[, 1];
rm(data);

n <- dim(X)[1];
p <- dim(X)[2];
look.back <- 60;
prob.trig <- 0.05;
## free cash
cash <- rep(0, n); 
wealth <- rep(0, nrow=n, ncol=p);
deviations <- rep(NA, n);
## KO, reserved cash, PEP, SPY, XLP
holding <- matrix(0, nrow=n, ncol=p+1);
purchase <- matrix(0, nrow=n, ncol=p+1);
ratio.cal <- matrix(NA, nrow=n, ncol=p+1);
for (t in (look.back + 1):n) {
    period <- (t-look.back):(t-1);
    model <- lm(ko~pep+spy+xlp, data=X[period, ]);
    shares <- c(1, -coef(model));
    ratio.cal[t, ] <- shares;
    deviations[t] <- X[t, 1] - predict(model, newdata=X[t, ], n.ahead=1);
    
    value <- deviations[t];
    F <- ecdf(residuals(model));

    inc.neutral <- rep(NA, p+1);
    if (holding[t-1, 1] > 0) {## currently long on KO
        inc.neutral[-1] <- shares[-1] * holding[t-1, 1] - holding[t-1, -1];
        inc.neutral[1] <- 0;
    } else if (holding[t-1, 1] == 0) {## currently neutral on KO
        inc.neutral <- rep(0, p);
    } else {## currently short on KO
        inc.neutral[-1] <- shares[-1] * holding[t-1, 1] - holding[t-1, -1];
        inc.neutral[1] <- 0;
    }
    
    if (holding[t-1, 1] > 0) {
        if (value < quantile(F, prob.trig)) {
            ## bottom: Long KO
            purchase[t, ] <- inc.neutral + shares;
        } else if (value > quantile(F, 0.5 - prob.trig) && value <= quantile(F, 1 - prob.trig)) {
            ## middle and upper: clear the portfolio, ignoring the neutrality increment
            purchase[t, ] <- -holding[t-1, ];
        } else if (value > quantile(F, 1 - prob.trig)) {
            ## top: clear the portfolio and take the short KO position
            purchase[t, ] <- -holding[t-1, ] - shares;
        } else { ## value > quantile(F, prob.trig) && value < quantile(F, 0.5 - prob.trig)
            ## lower: adjust the portfolio
            ## NOTE: value < 0
            value.prev <- sum(X[t, -1] * ratio.cal[t-1, ]);
            if (sign(value.prev) < 0) {
                k <- value.prev / value;
            } else if (sign(value.prev) >= 0) {
                ## Adjust to one unit of the portfolio
                k <- 1;
            }
            purchase[t, ] <- k * shares - holding[t-1, ];
        }
    } else if (holding[t-1, 1] < 0) {    ## currently short on KO
        if (value > quantile(F, 1 - prob.trig)) {
            ## top: short KO
            purchase[t, ] <- inc.neutral - shares;
        } else if (value < quantile(F, 0.5 + prob.trig) && value > quantile(F, prob.trig)) {
            ## middle and lower: clear the portfolio, ignoring the neutrality increment
            purchase[t, ] <- -holding[t-1, ];
        } else if (value < quantile(F, prob.trig)) {
            ## bottom: clear the portfolio and take the long KO position
            purchase[t, ] <- -holding[t-1, ] + shares;
        } else { ## value > quantile(F, prob.trig) && value > quantile(F, 0.5 + prob.trig)
            ## upper: adjust the portfolio
            ## NOTE: value < 0
            value.prev <- sum(X[t, -1] * ratio.cal[t-1, ]);
            if (sign(value.prev) > 0) {
                k <- value.prev / value;
            } else if (sign(value.prev) <= 0) {
                ## Adjust to one unit of the portfolio
                k <- 1;
            }
            purchase[t, ] <- -k * shares - holding[t-1, ];
        }
    } else { ## cash-only
        if (value < quantile(F, prob.trig)) {
            ## bottom: Long KO
            purchase[t, ] <- inc.neutral + shares;
        } else if (value > quantile(F, 1 - prob.trig)) {
            ## top: short KO
            purchase[t, ] <- inc.neutral - shares;
        }
    }
    holding[t, ] <- holding[t-1, ] + purchase[t, ];
    cash[t] <- cash[t-1] - sum(purchase[t, ] * X[t, -1]);
    wealth[t] <- cash[t] + sum(holding[t, ] * X[t, -1]);
}



I <- 785:791;
plot(I, deviations[I], type="l");

plot(1:n, wealth, type="l");
plot(1:n, cash, type="l");
plot(1:n, loadings[, 1], type="l");
plot(1:n, deviations[, 1], type="l");
