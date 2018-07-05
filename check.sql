drop view if exists monthly_values;
create view monthly_values as
select year(tm) as yr, month(tm) as mn, avg(worth) as worth
from trade_log
group by yr, mn order by yr, mn;

drop view if exists trades;
create view trades as
select A.tm, A.worth/B.worth - 1 as ret
from trade_log as A join trade_log as B
on A.tm = (
   select tm from trade_log where tm > B.tm
   and worth != B.worth limit 1
);

drop view if exists drawdowns;
create view drawdowns as (
select A.tm, A.worth as worth,
max(B.worth) as sup,
1 - A.worth/max(B.worth) as DD
from
trade_log as A join
trade_log as B on A.tm >= B.tm
group by A.tm
);

-- drop view if exists exposure;
-- create view exposure as 
-- select tm, (abs(uup_q) + abs(fxe_q) + abs(fxy_q)) as exposed
-- from trade_log
-- where tm <= '2016-06-23';

-- drop view if exists turnover;
-- create view turnover as
-- select B.tm,
-- (abs(B.uup_q - A.uup_q) + abs(B.fxe_q - A.fxe_q) + abs(B.fxy_q - A.fxy_q) + abs(B.fxb_q - A.fxb_q) + abs(B.fxc_q - A.fxc_q))/A.worth
-- as ratio
-- from trade_log as A join trade_log as B
-- on B.tm = (
--    select tm from trade_log where tm > A.tm limit 1
-- )
-- where A.worth != B.worth;
-- and B.tm < '2016-06-23';

-- select avg(ratio) from turnover;

select avg(ret) as R, avg(ret)/std(ret) as daily_sharpe from trades;
-- where tm < '2016-06-23';

select avg(ret) as R, avg(ret)/std(ret) as monthly_sharpe from (
select A.yr, A.mn, A.worth/B.worth - 1 as ret
from monthly_values as A join monthly_values as B
on
A.yr = B.yr and A.mn = B.mn + 1
or
A.yr = B.yr + 1 and A.mn = 1 and B.mn = 12
-- where A.yr < 2016 or A.yr = 2016 and A.mn < 6
) as tbl;


select max(DD) from
(select * from drawdowns
-- where tm < '2016-06-23'
) as tbl;

-- select avg(exposure) from (
--        select tm, (abs(uup_q) + abs(fxe_q) + abs(fxy_q) + abs(fxb_q) + abs(fxc_q))/worth as exposure
--        from trade_log
-- --       where tm < '2016-06-23'
-- ) as tbl;

