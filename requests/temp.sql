select *
from ny_crimes

select *
from ny_parts

delete
from ny_parts
where id_part = 8

select *
from ny_crimes c
left join ny_parts p
  on st_intersects(c.geom, p.geom) 
order by c.id_crime desc
limit 1

delete
from ny_crimes
where id_crime = 991

-- ALTER TABLE ny_crimes 
-- DROP COLUMN region;

-- ALTER TABLE ny_parts
-- DROP COLUMN shape__are;
-- 
-- ALTER TABLE ny_parts
-- DROP COLUMN shape__len;
-- 
-- ALTER TABLE ny_parts
-- DROP COLUMN objectid;
-- 
-- ALTER TABLE ny_parts
-- DROP COLUMN borocode;

-- 
-- ALTER TABLE ny_parts
-- RENAME COLUMN boroname TO region;

-- ALTER TABLE ny_parts
-- RENAME COLUMN gid TO id_part;

-- ALTER TABLE ny_crimes ADD COLUMN ids serial;
-- update ny_crimes set ids = id_crime;
-- alter table ny_crimes drop column id_crime;
-- alter table ny_crimes rename column ids to id_crime;

-- select * from ny_parts
-- ALTER TABLE ny_parts ADD COLUMN ids serial;
-- update ny_parts set ids = id_part;
-- alter table ny_parts drop column id_part;
-- alter table ny_parts rename column ids to id_part;


