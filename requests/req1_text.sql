-- вывести всё из таблицы ny_crimes
select
	id_crime as id,
	age_group as age,
	cr_type as type,
	time,
	date,
	st_x(geom) as lng,
	st_y(geom) as lat
from ny_crimes