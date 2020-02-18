CREATE VIEW brook AS
	select
	c.geom as bro
from ny_crimes c
join ny_parts p on st_intersects(c.geom, p.geom) 
where p.region like 'Brook%';

CREATE VIEW queen AS
	select
	c.geom as que
from ny_crimes c
join ny_parts p on st_intersects(c.geom, p.geom) 
where p.region like 'Queen%';

select 
	AVG(st_distance_sphere(bro, que)) as UPGMA,
	st_distance_sphere(
		st_point(
			AVG(st_x(bro)), 
			AVG(st_y(bro))
		), 
		st_point(
			AVG(st_x(que)), 
			AVG(st_y(que))
		)
	) as CENTROID,
	MAX(st_distance_sphere(bro, que)) as COMPLETE,
	MIN(st_distance_sphere(bro, que)) as SINGLE
from brook
CROSS JOIN queen;

-- drop view brook;
-- drop view queen;