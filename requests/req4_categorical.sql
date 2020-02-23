-- Ищем категориальные признаки
select
  column_name as params, 
  data_type as type
FROM information_schema.columns
WHERE 
  (table_name = 'ny_crimes' or 
    table_name = 'ny_parts') AND
    data_type = 'character varying'

select distinct
  tab.cr_type as levels
from (select * from ny_crimes
  join ny_parts on st_intersects(ny_crimes.geom, ny_parts.geom)) tab



-- преступления в определённом районе 
select 
	st_x(ny_crimes.geom) as lng, 
	st_y(ny_crimes.geom) as lat,
	ny_parts.region as region
from 
	ny_crimes 
join ny_parts on st_intersects(ny_crimes.geom, ny_parts.geom) 
where ny_parts.region like'Staten%' 
--limit 5