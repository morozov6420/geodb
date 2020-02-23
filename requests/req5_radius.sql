-- преступления в радиусе 6 км
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
join ny_parts p on st_intersects(c.geom, p.geom) 
where
	st_dwithin(
	  c.geom, 
		st_makepoint(-74.162627, 40.576205)::geography, 
		6000
  )