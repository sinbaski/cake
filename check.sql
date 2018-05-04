drop view if exists monthly_values;
create view monthly_values as
select year(tm) as yr, month(tm) as mn, avg(worth) as worth
from master_jjg_weat_soyb_trade_log
group by yr, mn order by yr, mn;

drop view if exists trades;
create view trades as
select A.tm, A.worth/B.worth - 1 as ret
from master_jjg_weat_soyb_trade_log as A join master_jjg_weat_soyb_trade_log as B
on A.tm = (
   select tm from master_jjg_weat_soyb_trade_log where tm > B.tm
   and worth != B.worth limit 1
);

drop view if exists drawdowns;
create view drawdowns as (
select A.tm, A.worth as worth,
max(B.worth) as sup,
1 - A.worth/max(B.worth) as DD
from
master_jjg_weat_soyb_trade_log as A join
master_jjg_weat_soyb_trade_log as B on A.tm >= B.tm
group by A.tm
);

select avg(ret)/std(ret) as daily_sharpe from trades
where tm < '2016-06-23';

-- select year(tm) as yr, month(tm) as mn, format(avg(ret)/std(ret), 3) as daily_sharpe
-- from trades group by yr, mn order by yr, mn;


select avg(ret)/std(ret) as monthly_sharpe from (
select A.yr, A.mn, A.worth/B.worth - 1 as ret
from monthly_values as A join monthly_values as B
on
A.yr = B.yr and A.mn = B.mn + 1
or
A.yr = B.yr + 1 and A.mn = 1 and B.mn = 12
where A.yr < 2016 or A.yr = 2016 and A.mn < 6
) as tbl;


select max(DD) from
(select * from drawdowns
where tm < '2016-06-23'
) as tbl;

select avg(exposure) from (
       select tm, (abs(jjg_q) + abs(weat_q) + abs(soyb_q))/worth as exposure
       from master_jjg_weat_soyb_trade_log
       where tm < '2016-06-23'
) as tbl;
