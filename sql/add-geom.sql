SELECT AddGeometryColumn ('bus_pos','geom',4326,'POINT',2);
UPDATE bus_pos SET geom = ST_SetSRID(ST_MakePoint("Lon", "Lat"), 4326);