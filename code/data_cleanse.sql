drop table if exists weibo.week27;
create table weibo.week27 (
	mid character varying,
	retweeted_status_mid character varying,
	uid character varying,
	retweeted_uid character varying,
	source character varying,
	image character varying,
	text character varying,
	geo character varying,
	created_at timestamp without time zone,
	deleted_last_seen character varying,
	permission_denied character varying
);

drop table if exists weibo.censor;
create table weibo.censor (
	province character varying,
	gender character varying,
	verified character varying,
	mid character varying,
	retweet_mid_indicator character varying,
	uid character varying,
	retweet_uid_indicator character varying,
	source character varying,
	image character varying,
	text character varying,
	geo character varying,
	created_at timestamp without time zone,
	delete_indicator character varying,
	censor_indicator character varying
);

select count(*) from weibo.censor
copy weibo.censor(province, gender, verified, mid,retweet_mid_indicator,uid,retweet_uid_indicator,source,image,text,geo,created_at,delete_indicator,censor_indicator)
from 'D:/weibo/thesis/censor.csv'
WITH DELIMITER ','
CSV HEADER;

select * from weibo.week26 limit 100

drop table if exists weibo.geo;
create table weibo.geo as
select geo,count(*) from weibo.keywords group by geo;

select * from weibo.keywords --289,646
select count from (select geo, count(*) from weibo.keywords group by geo) as foo group by count order by count
select * from weibo.geo

select regexp_replace(geo, '([^ ]+) .*)')  from weibo.geo
trim(trailing ')' from trim(leading 'POINT(' from geo))

select text from weibo.keywords where text like '%#%'
select * from weibo.keywords where text like '%#保钓#%' or text like '%#钓鱼岛#%'
select text, date(created_at) from weibo.keywords where text like '%#保钓%' or text like '%#钓鱼岛%' group by date(created_at),text order by date(created_at)
select text, date(created_at) from weibo.keywords where text like '%新浪军事%' group by date(created_at),text order by date(created_at)

select count(distinct mid) from weibo.censor

select * from weibo.week32 limit 100
select * from weibo.week32 where text like '%钓鱼岛%'
-- 1, 3, 6, 9, 12, 13, 16, 18, 19, 21, 28, 44, 48
select * from weibo.week28 limit 100

--7/16-10/28 29-43
drop table if exists weibo.week41;
create table weibo.week2843 as 
select * from weibo.week38;


insert into weibo.week2843
select * from weibo.week39

select date(created_at) from weibo.week2843 group by date(created_at)
select * from weibo.week2843 limit 100

create table weibo.kw as
select * from weibo.week2843 where text like '%保钓%' or text like '%钓鱼岛%' or text like '%反日%' or text like '%领土争端%'  or text like '%领土主权%'  or text like '%领土完整%' or text like '%钓鱼台%'  or text like '%尖阁诸岛%' or text like '%DIAOYU%' or text like '%钓岛%' or text like '%主权%' or text like '%游行%'


select count(*) from weibo.keywords --289,646
select * from weibo.kw 
SELECT trunc('hour', created_at) AS stamp from weibo.kw
select date_trunc('hour', created_at::timestamp) from weibo.kw limit 20
select date_trunc('hour', now()) 

select now()
select * from weibo.kw limit 100
--
select date(created_at), count(distinct uid) from weibo.kw group by date(created_at)
select date_trunc('hour', created_at::timestamp),count(distinct uid) from weibo.kw group by date_trunc('hour', created_at::timestamp)
select uid, count(*) from weibo.kw group by uid
select date_trunc('hour', created_at::timestamp),count(*) as count from weibo.kw group by date_trunc('hour', created_at::timestamp)
select date_trunc('hour', created_at::timestamp) as hr ,count(retweeted_status_mid) as count from weibo.kw group by hr order by hr
select date_trunc('hour', created_at::timestamp),count(distinct retweeted_uid) as count from weibo.kw group by date_trunc('hour', created_at::timestamp)
select  text from weibo.kw where text like '%#%' 
SELECT regexp_matches(text, '#([^#]+)#'),text FROM weibo.kw 
select hr, count(*) from (
select date_trunc('hour', created_at::timestamp) as hr, regexp_matches(text, '#([^#]+)#') from weibo.kw ) as foo --#([^#]+)#
group by hr
order by hr;

select count(*) from weibo.kw

from (
    select regexp_matches(text, '#([^#]+)#','g' ) a
    from weibo.kw
) sselect rtrim(regexp_matches(text, '#([^#]+)#') , text), text from weibo.kw

-- deleted
select hr, count(*) from (
select date_trunc('hour', created_at::timestamp) as hr, deleted_last_seen, permission_denied from weibo.kw where deleted_last_seen is not null or permission_denied is not null) as fooo 
group by hr
order by hr

select foo.i, (case when a.count is not null then a.count else 0 end) as count from
(select i, 0 as z
from generate_series('2012-08-02 00:00:00'::timestamp - interval '24' hour,'2012-10-15 23:00:00'::timestamp, interval '1' hour) i) as foo
left join 
(select date_trunc('hour', created_at::timestamp) as hr, count(*) from weibo.kw where deleted_last_seen is null and permission_denied is null
group by hr
order by hr) as a
on foo.i= a.hr

-- all weibo hr
select foo.i, (case when a.count is not null then a.count else 0 end) as count from
(select i, 0 as z
from generate_series('2012-08-02 00:00:00'::timestamp - interval '24' hour,'2012-10-15 23:00:00'::timestamp, interval '1' hour) i) as foo
left join 
(select date_trunc('hour', created_at::timestamp) as hr, count(*) from weibo.kw
group by hr
order by hr) as a
on foo.i= a.hr

-- all weibo date
select date(created_at)as date, count(*)
from weibo.kw
where date(created_at) between '2012-08-01' and '2012-10-15'
group by date(created_at)
order by date

-- all weibo uid date
select date(created_at)as date, count(distinct uid)
from weibo.kw
where date(created_at) between '2012-08-01' and '2012-10-15'
group by date(created_at)
order by date



-- undeleted
select hr, count(*) from (
select date_trunc('hour', created_at::timestamp) as hr, deleted_last_seen, permission_denied from weibo.kw where deleted_last_seen is null and permission_denied is null) as fooo 
group by hr
order by hr

-- uid
select foo.i, (case when a.count is not null then a.count else 0 end) as count from
(select i, 0 as z
from generate_series('2012-08-02 00:00:00'::timestamp - interval '24' hour,'2012-10-15 23:00:00'::timestamp, interval '1' hour) i) as foo
left join 
(select date_trunc('hour', created_at::timestamp) as hr,count(distinct uid) 
from weibo.kw 
group by date_trunc('hour', created_at::timestamp)
order by date_trunc('hour', created_at::timestamp)) as a
on foo.i= a.hr

-- reposted uid
select foo.i, (case when a.count is not null then a.count else 0 end) as count from
(select i, 0 as z
from generate_series('2012-08-02 00:00:00'::timestamp - interval '24' hour,'2012-10-15 23:00:00'::timestamp, interval '1' hour) i) as foo
left join 
(select date_trunc('hour', created_at::timestamp) as hr ,count(retweeted_uid) as count from weibo.kw group by hr order by hr) as a
on foo.i= a.hr

-- hashtags
select foo.i, (case when a.count is not null then a.count else 0 end) as count from
(select i, 0 as z
from generate_series('2012-08-02 00:00:00'::timestamp - interval '24' hour,'2012-10-15 23:00:00'::timestamp, interval '1' hour) i) as foo
left join 
(select hr, count(*) from (
select date_trunc('hour', created_at::timestamp) as hr, regexp_matches(text, '#([^#]+)#') from weibo.kw ) as foo --#([^#]+)#
group by hr
order by hr) as a
on foo.i= a.hr


select * from weibo.user limit 100
select * from weibo.weibo_user limit 100


drop table if exists weibo.weibo_user;
create table weibo.weibo_user as
select a.* ,b.province, b.gender,b.verified
from weibo.kw as a
left join (select * from weibo.user) as b
on a.uid=b.uid

select foo.i, (case when a.count is not null then a.count else 0 end) as count from
(select i, 0 as z
from generate_series('2012-08-02 00:00:00'::timestamp - interval '24' hour,'2012-10-15 23:00:00'::timestamp, interval '1' hour) i) as foo
left join 
(select  date_trunc('hour', created_at::timestamp) as hr, count(*) from weibo.weibo_user
where verified='True' group by hr order by hr) as a
on foo.i= a.hr

select date_trunc('hour', created_at::timestamp) as hr, count(*) from weibo.weibo_user
where gender='f'
group by hr
order by hr

select date_trunc('hour', created_at::timestamp) as hr, province, count(*) from weibo.weibo_user group by hr, province order by hr, province

select foo.i, a.province, (case when a.count is not null then a.count else 0 end) as count from
(select i, 0 as z
from generate_series('2012-08-02 00:00:00'::timestamp - interval '24' hour,'2012-10-15 23:00:00'::timestamp, interval '1' hour) i) as foo
left join 
(select date_trunc('hour', created_at::timestamp) as hr, province, count(*) from weibo.weibo_user group by hr, province order by hr, province) as a
on foo.i= a.hr

select * from weibo.weibo_user limit 100

select count(*) from (
select mid, uid, source, image, date_trunc('hour', created_at::timestamp) as hour, gender, province, verified, case when deleted_last_seen is not null then 1 else 0 end as delete
from weibo.weibo_user limit 100000) as t
where t.delete=1

(select mid, uid, case when retweeted_status_mid is not null then 1 else 0 end as retweet_mid, case when retweeted_uid is not null then 1 else 0 end as retweet_uid, 
source, image, case when geo is not null then 1 else 0 end as geo, date_trunc('hour', created_at::timestamp) as start_time, date_trunc('hour', deleted_last_seen::timestamp) as end_time,  
round(extract(epoch from (deleted_last_seen::timestamp  - created_at::timestamp ))/3600) as hours,
gender, province, verified, case when deleted_last_seen is not null then 1 else 0 end as delete
from weibo.weibo_user
where deleted_last_seen is not null
limit 10000) 
union all
(select mid, uid, case when retweeted_status_mid is not null then 1 else 0 end as retweet_mid, case when retweeted_uid is not null then 1 else 0 end as retweet_uid, 
source, image, case when geo is not null then 1 else 0 end as geo, date_trunc('hour', created_at::timestamp) as start_time, date_trunc('hour', deleted_last_seen::timestamp) as end_time,
round(extract(epoch from (deleted_last_seen::timestamp  - created_at::timestamp ))/3600) as hours,  
gender, province, verified, case when deleted_last_seen is not null then 1 else 0 end as delete
from weibo.weibo_user
where deleted_last_seen is null
limit 10000) 

select count(*)
from weibo.weibo_user
where deleted_last_seen is null --29,741 / 214,743
kw
select deleted_last_seen, created_at, (date_trunc('hour', deleted_last_seen::timestamp)- date_trunc('hour', created_at::timestamp) )as hour, text
from weibo.weibo_user
where deleted_last_seen is not null
order by hour

select round(extract(epoch from (timestamp '2007-02-07 16:24:00' - timestamp '2007-02-07
13:00:01'))/3600) as hours; 

select min(date_trunc('hour',created_at::timestamp)),max(date_trunc('hour', created_at::timestamp))
from weibo.weibo_user --"2012-07-16 00:00:00" "2012-10-28 23:59:59"

select mid, uid, case when retweeted_status_mid is not null then 1 else 0 end as retweet_mid, case when retweeted_uid is not null then 1 else 0 end as retweet_uid, source, image, case when geo is not null then 1 else 0 end as geo, date_trunc('hour', created_at::timestamp) as start_time, 
case when date_trunc('hour', deleted_last_seen::timestamp)is null then '2012-10-15 23:00:00':: timestamp else date_trunc('hour', deleted_last_seen::timestamp) end as end_time,
--round(extract(epoch from (deleted_last_seen::timestamp  - created_at::timestamp ))/3600) as hours,  
gender, province, verified, case when deleted_last_seen is not null then 1 else 0 end as delete
from weibo.weibo_user
where created_at::timestamp >= '2012-08-01 00:00:00'::timestamp and created_at::timestamp < '2012-10-16 00:00:00'::timestamp
order by created_at

 
select created_at, text  gender, province, verified, 
from weibo.weibo_user
order by created_at asc

select mid, uid,  retweeted_status_mid, retweeted_uid,source, image, case when geo is not null then 1 else 0 end as geo, date(created_at) as created_at, 
date(deleted_last_seen) as deleted_last_seen, case when deleted_last_seen is not null then 1 else 0 end as delete_indicator,
date(permission_denied) as permision_denied, case when permission_denied is not null then 1 else 0 end as censor_indicator
from weibo.week2843
order by created_at
limit 100


select mid, case when retweeted_status_mid is not null then 1 else 0 end as retweet_mid_indicator, uid, case when retweeted_uid is not null then 1 else 0 end as retweet_uid_indicator, source, image, text, 
case when geo is not null then 1 else 0 end as geo,date(created_at) as created_at,
case when deleted_last_seen is not null then 1 else 0 end as delete_indicator, case when permission_denied is not null then 1 else 0 end as censor_indicator
from weibo.week2843 -

select * from weibo.wu limit 100

select sum(delete_indicator), sum(censor_indicator) from weibo.wu --3,670,847 18,024

-- not delete not censor
select * from weibo.wu where delete_indicator=0 and censor_indicator=0

-- delete
select * from weibo.wu where delete_indicator=1 

-- delete and censor
select * from weibo.wu where delete_indicator=1 and censor_indicator=1
-- censor
select * from weibo.wu where censor_indicator=1

select count(*) from weibo.wu where censor_indicator=0

select row_to_json(b) 
from (select mid, retweeted_status_mid, uid, retweeted_uid, source, image, geo, created_at, province, gender, verified, text, deleted_last_seen, permission_denied from weibo.weibo_user limit 100) as b

copy(select mid, retweet_mid_indicator, uid, retweet_uid_indicator, source, image, geo, created_at, province, gender, verified, text, delete_indicator, censor_indicator from weibo.wu  where delete_indicator=1)  to 'D:\weibo\thesis\delete_v1.csv'


copy(select mid, retweet_mid_indicator, uid, retweet_uid_indicator, source, image, geo, created_at, province, gender, verified, text, delete_indicator, censor_indicator from weibo.wu  where delete_indicator=0) to 'D:\weibo\thesis\undelete_v1.csv'

----------------------------------------------------------------------------------------------------------------------------------------------
-- Weibo.weibo-users 
drop table if exists weibo.wu;
create table weibo.wu as
select b.province, b.gender,b.verified, a.* 
from 
(select mid, case when retweeted_status_mid is not null then 1 else 0 end as retweet_mid_indicator, uid, case when retweeted_uid is not null then 1 else 0 end as retweet_uid_indicator, source, image, text, 
case when geo is not null then 1 else 0 end as geo,date(created_at) as created_at,
case when deleted_last_seen is not null then 1 else 0 end as delete_indicator, case when permission_denied is not null then 1 else 0 end as censor_indicator
from weibo.week2843 ) as a
left join (select * from weibo.user) as b
on a.uid=b.uid