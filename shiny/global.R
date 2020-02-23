rm(list = ls())
request_1 <- "
select
  c.id_crime as id
  , c.age_group as age
  , c.cr_type as type
  , c.time
  , c.date
  , p.region as region
  , st_x(c.geom) as lng
  , st_y(c.geom) as lat
from 
  ny_crimes c
  left join ny_parts p
  on st_intersects(c.geom, p.geom)"

request_2 <- c("
select
  c.id_crime as id
  , c.age_group as age
  , c.cr_type as type
  , c.time
  , c.date
  , p.region as region
  , st_x(c.geom) as lng
  , st_y(c.geom) as lat
from
  ny_crimes c
  left join ny_parts p
  on st_intersects(c.geom, p.geom)
order by c.geom <-> st_point(","______",", ","______",")
limit ","______")

r3_coord <- data.frame(numeric(), numeric())
r3_cooord <- data.frame(numeric(), numeric())

request_3_1 <- c("
select st_astext(
  st_multi(","_______","(
      'LINESTRING(
        ","_______","
      )'::geometry
  ))
) as text")

request_3_2 <- c("

select 
  c.id_crime as id,
  c.age_group as age,
  c.cr_type as type,
  c.time,
  c.date,
  st_x(c.geom) as lng,
  st_y(c.geom) as lat
from 
  ny_crimes c
  left join ny_parts p
  on st_intersects(
    c.geom,
    p.geom
  )
where st_intersects(
  c.geom,
  st_multi(","_______","(
    'LINESTRING(
      ","_______","
    )'::geometry
  ))
)")


request_4_1 <- "
select
  column_name as params
  , data_type as type
from information_schema.columns
where 
  (table_name = 'ny_crimes' or 
    table_name = 'ny_parts') and
    data_type = 'character varying'"

request_4_2 <- c("

select distinct
  tab.","_______"," as levels
from (select * from ny_crimes c
  left join ny_parts p
  on st_intersects(c.geom, p.geom)) tab")

request_4_3 <- c("

select 
  c.id_crime as id
  , c.age_group as age
  , c.cr_type as type
  , c.time
  , c.date
  , p.region as region
  , st_x(c.geom) as lng
  , st_y(c.geom) as lat
from 
  ny_crimes c 
  left join ny_parts p 
  on st_intersects(c.geom, p.geom) 
where ","_____"," like '","_____","%' ")

request_5 <- c("

select
  c.id_crime as id
  , c.age_group as age
  , c.cr_type as type
  , c.time
  , c.date
  , p.region as region
  , st_x(c.geom) as lng
  , st_y(c.geom) as lat
from
  ny_crimes c
  left join ny_parts p
  on st_intersects(c.geom, p.geom) 
where
  st_dwithin(
    c.geom, 
    st_makepoint(","_____",", ","_____",")::geography, 
    ","_____","
  )")

insert_6_1 <- data.frame(
  numeric(), # id_crime
  character(), # age_group
  character(), # vic_sex
  character(), # cr_type
  character(), # geom
  character(), # geom
  character(), # date
  character() # time
)

request_6_1_1 <- c("
insert into ny_crimes 
values (
  '","______","',
  '","______","',
  '","______","',
  st_point(","______",", ","______","),
  '","______","',
  '","______","'
)
returning 
  id_crime, 
  age_group, 
  vic_sex,
  cr_type, 
  st_x(geom) as lng, 
  st_y(geom) as lat,
  date,
  time")



r6_2_coord <- data.frame(numeric(), numeric())
r6_2_cooord <- data.frame(numeric(), numeric())
request_6_2_1 <- c("
select st_astext(
  st_multi(","_______","(
    'LINESTRING(
      ","_______","
    )'::geometry
  ))
) as text")

request_6_2_2 <- c("

insert into ny_parts
values (
  '","______","',
  st_multi(","_______","(
    'LINESTRING(
      ","_______","
    )'::geometry
  ))
)
returning 
  id_part,
  region,
  st_astext(geom)")



request_7_1 <- "
select
  column_name as params
  , data_type as type
from information_schema.columns
where 
  (table_name = 'ny_crimes' or 
    table_name = 'ny_parts') and
    data_type = 'character varying'"

request_7_2 <- c("

select distinct
  tab.","_______"," as levels
from (select * from ny_crimes c
  left join ny_parts p
  on st_intersects(c.geom, p.geom)) tab")


request_7_3 <- c("

select 
  *
from (
  select
    c.id_crime as id_crime_1
    , c.age_group as age_group_1
    , c.cr_type as cr_type_1
    , c.vic_sex as vic_sex_1
    , c.time as time_1
    , c.date as date_1
    , p.region as region_1
    , st_x(c.geom) as lng_1
    , st_y(c.geom) as lat_1
  from ny_crimes c
  join ny_parts p on st_intersects(c.geom, p.geom)
) view_1
cross join (
  select
    c.id_crime as id_crime_2
    , c.age_group as age_group_2
    , c.cr_type as cr_type_2
    , c.vic_sex as vic_sex_2
    , c.time as time_2
    , c.date as date_2
    , p.region as region_2
    , st_x(c.geom) as lng_2
    , st_y(c.geom) as lat_2
  from ny_crimes c
  join ny_parts p on st_intersects(c.geom, p.geom)
) view_2
where view_1.","_____","_1 like '","_____","%'"," 
  and view_2.","_____","_2 like '","_____","%'")

request_7_4 <- c("

select 
  AVG(st_distancesphere(geom_1, geom_2)) as UPGMA,
  st_distancesphere(
    st_point(
      AVG(st_x(geom_1)), 
      AVG(st_y(geom_1))
    ), 
    st_point(
      AVG(st_x(geom_2)), 
      AVG(st_y(geom_2))
    )
  ) as CENTROID,
  MAX(st_distancesphere(geom_1, geom_2)) as COMPLETE,
  MIN(st_distancesphere(geom_1, geom_2)) as SINGLE
from (
  select
    c.id_crime as id_crime_1
    , c.age_group as age_group_1
    , c.cr_type as cr_type_1
    , c.vic_sex as vic_sex_1
    , c.time as time_1
    , c.date as date_1
    , p.region as region_1
    , c.geom as geom_1
  from ny_crimes c
  join ny_parts p on st_intersects(c.geom, p.geom)
) view_1
cross join (
  select
    c.id_crime as id_crime_2
    , c.age_group as age_group_2
    , c.cr_type as cr_type_2
    , c.vic_sex as vic_sex_2
    , c.time as time_2
    , c.date as date_2
    , p.region as region_2
    , c.geom as geom_2
  from ny_crimes c
  join ny_parts p on st_intersects(c.geom, p.geom)
) view_2
where view_1.","_____","_1 like '","_____","%'"," 
  and view_2.","_____","_2 like '","_____","%'")