SELECT DISTINCT b.* 

FROM public.bus_incidents a
inner join public.bus_incidents b on a."IncidentID" = b."IncidentID" and a."DirectionAffected" = b."DirectionAffected" and b."trigger_timestamp" = (select b1."trigger_timestamp"
																																					 	from public.bus_incidents b1
																																					 	where b1."IncidentID" = b."IncidentID"
																																						 	  and b1."DirectionAffected" = b."DirectionAffected"
																																					 	order by b1."trigger_timestamp" desc
																																					 	limit 1)
order by b."trigger_timestamp" desc, b."RoutesAffected"

