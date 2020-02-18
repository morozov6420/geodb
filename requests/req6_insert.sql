-- вставка данных
INSERT INTO ny_crimes 
VALUES (
  10000,
	'young',
	'M',
	'studying',
	ST_Point(-73.7648270894136, 40.5033579037453),
	'2020-02-17',
	'3:24:15'
	)
	RETURNING *;
	DELETE FROM ny_crimes WHERE cr_type = 'studying';