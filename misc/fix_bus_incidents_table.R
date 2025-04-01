rm(list=setdiff(ls(), c("bi_db")))
closeAllConnections()
start_time <- Sys.time()
print(paste0("Start time: ",start_time))

###
pg <- dbConnect(RPostgres::Postgres()
                      , host=Sys.getenv("pg_host")
                      , port=Sys.getenv("pg_port")
                      , dbname="wmata"
                      , user=Sys.getenv("pg_user")
                      , password=Sys.getenv("pg_password"))

bi_db <- dbGetQuery(pg,paste0('select b.* 
                                      from public.bus_incidents a
                                                    inner join public.bus_incidents b on a."IncidentID" = b."IncidentID" and a."DirectionAffected" = b."DirectionAffected" and b."trigger_timestamp" = (select b1."trigger_timestamp"
                                                                                                                    from public.bus_incidents b1
                                                                                                                    where b1."IncidentID" = b."IncidentID"
                                                                                                                        and b1."DirectionAffected" = b."DirectionAffected"
                                                                                                                    order by b1."trigger_timestamp" desc
                                                                                                                    limit 1)                                                               
                                      order by b."trigger_timestamp" desc'))
#bi_db_deduped <- bi_db[!is.na(bi_db$StartTime),]
 bi_db_deduped <- bi_db %>% group_by(IncidentID,DirectionAffected) %>%
     arrange(!desc(StartTime)) %>%
     filter(row_number() == 1)

dbWriteTable(pg,"bus_incidents",bi_db_deduped,row.names = FALSE, overwrite = TRUE, append = FALSE) 
dbDisconnect(pg) 
closeAllConnections()

