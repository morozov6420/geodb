-- преступления в радиусе 6 км
select
	ny_parts.region as region,
	ny_crimes.cr_type as type, 
	st_x(ny_crimes.geom) as lng, 
	st_y(ny_crimes.geom) as lat
from
	ny_crimes
join ny_parts on st_intersects(ny_crimes.geom, ny_parts.geom) 
where
	st_dwithin(ny_parts.geom, st_makepoint(-74.162627, 40.576205)::geography, 6000)