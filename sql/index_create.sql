create index concurrently "bus_incidents_indices"
on public.bus_incidents using btree ("trigger_timestamp", "IncidentID", "IncidentLastUpdated", "DirectionAffected");