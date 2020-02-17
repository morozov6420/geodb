request_1 <- "select
  id_crime as id,
  age_group as age,
  cr_type as type,
  time,
  date,
  st_x(geom) as lng,
  st_y(geom) as lat
from 
  ny_crimes"

request_2 <- c(
  "select
  id_crime as id,
  age_group as age,
  cr_type as type,
  time,
  date,
  st_x(geom) as lng,
  st_y(geom) as lat
from
  ny_crimes
order by geom <-> st_makepoint(",
"______", # 2
", ",
"______", # 4
")
limit ",
"______" # 6
)

r3_coord <- data.frame(numeric(), numeric())
r3_cooord <- data.frame(numeric(), numeric())

request_3_1 <- c("select st_astext(
  st_multi(",
    "_______", # 2   st_convexhull or st_makepolygon
    "(
      'LINESTRING(",
      "_______", # 4   1 10, 2 20, 3 30
      ")'::geometry
    )
  )
) as text")

request_3_2 <- c("select 
  id_crime as id,
  age_group as age,
  cr_type as type,
  time,
  date,
  st_x(ny_crimes.geom) as lng, 
  st_y(ny_crimes.geom) as lat
from 
  ny_crimes 
where st_intersects(
  ny_crimes.geom, 
  st_multi(",
    "_______", # 2   st_convexhull or st_makepolygon
    "(
      'LINESTRING(",
      "_______", # 4   1 10, 2 20, 3 30
      ")'::geometry
    )
  )
)")


request_4_1 <- "select
  column_name as params, 
  data_type as type
FROM information_schema.columns
WHERE 
  (table_name = 'ny_crimes' or 
    table_name = 'ny_parts') AND
    data_type = 'character varying'"

request_4_2 <- c("select distinct
  tab.",
  "_______", # 2 
  " as levels
from (select * from ny_crimes
  join ny_parts on st_intersects(ny_crimes.geom, ny_parts.geom)) tab")

request_4_3 <- c("select 
	id_crime as id,
	age_group as age,
	cr_type as type,
	time,
	date,
	ny_parts.region as region,
	st_x(ny_crimes.geom) as lng, 
	st_y(ny_crimes.geom) as lat
from 
	ny_crimes 
join ny_parts on st_intersects(ny_crimes.geom, ny_parts.geom) 
where ",
"_____", # 2
" like '",
"_____", # 4
"%' ")

