queryDatabase <- function(user, password, dbname, host, symbols, d1, d2)
{
    require(RMySQL);
    database = dbConnect(MySQL(), user=user, password=password,
                         dbname=dbname, host=host);
    n <- length(symbols);
    stmt <- "select";
    fragment1 <- "from";
    fragment2 <- "on";
    fragment3 <- sprintf("where %1$s_daily.tm between '%2$s' and '%3$s' order by %1$s_daily.tm asc;",
                         symbols[n], d1, d2);
    for (i in 1:(n-1)) {
        str <- sprintf("%1$s_daily.closing as %1$s,", symbols[i]);
        stmt <- paste(stmt, str);
        fragment1 <- paste(fragment1, sprintf("%s_daily", symbols[i]), "join");
        if (i == n-1) {
            stmt <- paste(stmt, sprintf("%1$s_daily.closing as %1$s", symbols[n]));
            fragment1 <- paste(fragment1, sprintf("%s_daily", symbols[n]));
        }
        if (i < n - 1) {
            fragment2 <- paste(fragment2, sprintf("%s_daily.tm=%s_daily.tm and", symbols[i], symbols[n]));
        } else {
            fragment2 <- paste(fragment2, sprintf("%s_daily.tm=%s_daily.tm", symbols[i], symbols[n]));
        }
    }
    stmt <- paste(stmt, fragment1, fragment2, fragment3);
    results <- dbSendQuery(database, stmt);
    data <- fetch(results, n=-1);
    dbClearResult(results);
    dbDisconnect(database);
    return(as.matrix(data));
}

