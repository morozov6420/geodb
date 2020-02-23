select 
	*
from (
  select
	  c.id_crime as id_crime_1
    , c.age_group as age_group_1
    , c.cr_type as cr_type_1
    , c.time as time_1
    , c.date as date_1
    , p.region as region_1
    , st_x(c.geom) as lng_1
    , st_y(c.geom) as lat_1
  from ny_crimes c
  join ny_parts p on st_intersects(c.geom, p.geom)
) view_1
CROSS JOIN (
  select
	  c.id_crime as id_crime_2
    , c.age_group as age_group_2
    , c.cr_type as cr_type_2
    , c.time as time_2
    , c.date as date_2
    , p.region as region_2
    , st_x(c.geom) as lng_2
    , st_y(c.geom) as lat_2
  from ny_crimes c
  join ny_parts p on st_intersects(c.geom, p.geom)
) view_2
where view_1.region_1 like 'MMM%' and view_2.region_2 like 'Staten%';



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
CROSS JOIN (
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
where view_1.region_1 like 'Staten%' and view_2.region_2 like 'MMM%';