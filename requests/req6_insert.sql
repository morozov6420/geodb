-- имена таблиц
select table_name as tables
FROM information_schema.tables
where table_schema = 'public' and 
  table_type = 'BASE TABLE' and 
	table_name != 'spatial_ref_sys'
	


-- вставка данных
INSERT INTO ny_crimes 
VALUES (
--   (select max(id_crime) + 1 from ny_crimes)
--   10000,
	'young',
	'M',
	'studying',
	ST_Point(-73.7648270894136, 40.5033579037453),
	'2020-02-17',
	'3:24:15'
	)
RETURNING 
  id_crime, 
	age_group, 
	vic_sex,
	cr_type, 
	st_x(geom) as lng, 
	st_y(geom) as lat,
	date,
	time;

DELETE FROM ny_crimes WHERE cr_type = 'studying';

select * FROM ny_crimes WHERE cr_type = 'studying';