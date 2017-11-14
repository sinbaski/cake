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
X <- fetch(results, n=-1);
dbClearResult(results);
dbDisconnect(database);

n <- dim(X)[1];
p <- dim(X)[2]-1;
look.back <- 60;
ma.t <- 10;
action <- {};
prob.trig <- 0.05;
holding <- matrix(0, nrow=n, ncol=p);
cash <- matrix(NA, nrow=n, ncol=p); 
wealth <- matrix(NA, nrow=n, ncol=p);
purchase <- matrix(NA, nrow=n, ncol=p);

deviations <- rep(NA, n);
ratio.hold <- matrix(NA, nrow=n, ncol=p);
ratio.cal <- matrix(NA, nrow=n, ncol=p);
for (t in (look.back + 1):n) {
    period <- (t-look.back):(t-1);
    prices <- X[period, ];
    model <- lm(ko~pep+spy+xlp, data=prices);
    deviations[t] <- X$ko[t] - predict(model, newdata=X[t, ], n.ahead=1);
    shares <- c(1, -tail(coef(model), n=-1));
    ratio.cal[t, ] <- shares;
    
    value <- deviations[t];
    F <- ecdf(residuals(model));

    inc.neutral <- rep(NA, p);
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
            ## Long KO
            purchase[t, ] <- inc.neutral + shares;
        } else if (value > quantile(F, 0.5 - prob.trig) && value < quantile(F, 1 - prob.trig)) {
            ## dump the portfolio, ignoring the neutrality increment
            purchase[t, ] <- -holding[t-1, ];
        } else if (value > quantile(F, 0.5 - prob.trig) && value < quantile(F, 1 - prob.trig)) {
            purchase[t, ] <- -holding[t-1, ] - shares;
        }
    } else if (holding[t-1, 1] < 0) {    ## currently short on KO
        if (value > quantile(F, 1 - prob.trig)) {
            ## short even further
            purchase[t, ] <- inc.neutral - shares;
        }
    }
    holding[t, ] <- holding[t-1, ] + purchase[t, ];    
}
