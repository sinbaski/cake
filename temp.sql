-- NDA_series_SEK_ST_SE

-- drop table if exists Deviations;
-- create table Deviations (
--        day date primary key,
--        -- closing decimal(7,2),
--        price double,
--        deviation real
-- ) as
-- select T2.day, T2.rate as price, (T2.rate - avg(T1.rate))/avg(T1.rate) as deviation from EUR_SEK_Rates as T2 join EUR_SEK_Rates as T1 on datediff(T2.day, T1.day) between 1 and 60 where T2.day between '2013-01-02' and '2014-09-30' group by T2.day;

drop table if exists Deviations;
create table Deviations (
       day date primary key,
       price double,
       deviation real
) as
select T2.day, T2.closing as price,  log(T2.closing) - log(avg(T1.closing)) as deviation from KO_US as T2 join KO_US as T1 on datediff(T2.day, T1.day) between 1 and 60 where T2.day between '2013-01-02' and '2014-09-30' group by T2.day;



-- drop function if exists padd;
-- delimiter #
-- create function padd (x real, n int) returns real
-- begin
-- 	if x > 0 then
-- 	   set @y = concat(' ', x);
-- 	else
-- 	   set @y = x;
-- 	end if;
-- 	return rpad(@y, n, '0');
-- end#
-- delimiter ;

-- drop function if exists todo;
-- delimiter #
-- create function todo (x float) returns varchar(4)
-- begin
-- 	if x < -0.03671877 then
-- 	   set @s = 'buy';
-- 	elseif x > 0.04061545  then
-- 	   set @s = 'sell';
-- 	else
-- 	   set @s = '';
-- 	end if;
-- 	return @s;
-- end#
-- delimiter ;

-- select day, closing, deviation, todo(deviation) from (
-- select T2.day, T2.closing, (T2.closing - avg(T1.closing))/avg(T1.closing) as deviation from NDA_series_SEK_ST_SE as T2 join NDA_series_SEK_ST_SE as T1 on T2.day > T1.day and datediff(T2.day, T1.day) <= 60 where T2.day between '2014-10-01' and '2014-12-31' group by T2.day ) as tbl ;

--- maybe extreme deviations from the moving average should be excluded ---
--- Extremes should be considered seperately.
--- How do we identify extreme data points?
