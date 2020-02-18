-- первые пять соседей от точки
select 
	id_crime as id,
	age_group as age,
	cr_type as type,
	time,
	date,
	st_x(geom) as lng,
	st_y(geom) as lat
from 
	ny_crimes 
order by geom <-> st_makepoint(-74.162627,40.576205) 
limit 5