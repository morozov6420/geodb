select 
	-- расстояние между преступлениями в метрах 
	st_distance( 
		(
			select 
				ny_crimes.geom 
			from 
				ny_crimes 
			join ny_parts on st_intersects(ny_crimes.geom, ny_parts.geom) 
			where /*ny_parts.boroname like 'BROOK%' and*/ id_crime = 2
		), 
		(
			select 
				ny_crimes.geom 
			from 
				ny_crimes
			join ny_parts on st_intersects(ny_crimes.geom, ny_parts.geom) 
			where /*ny_parts.boroname like 'BROOK%' and*/ id_crime = 7
		), 
		true 
) as dist