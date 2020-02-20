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