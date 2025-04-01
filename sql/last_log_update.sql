SELECT * FROM public.log
where "RouteID" <> '999'
order by "trigger_timestamp" desc
LIMIT 1
