### remove tables that got added with 0 rows
rm(list = ls(all.names = TRUE)) 
closeAllConnections()
start_time <- Sys.time()
print(paste0("Start time: ",Sys.time()))
pg <- dbConnect(RPostgres::Postgres()
		                 , host=Sys.getenv("pg_host")
		                 , port=Sys.getenv("pg_port")
		                 , dbname="wmata"
		                 , user=Sys.getenv("pg_user")
		                 , password=Sys.getenv("pg_password"))
tbls <- as.array(dbListTables(pg))
tbls <- subset(tbls, tbls %like% 'stops_' | tbls %like% 'sched_' | tbls %like% 'routes_')
i <- 1

for (i in 1:length(tbls)) {
	res <- dbGetQuery(pg,paste0('SELECT COUNT(*) FROM public."',tbls[i],'"'))	
	res$tbl <- tbls[i]
{
	if (((res$tbl %like% 'sched_' | res$tbl %like% 'routes_') & (res$count < 1))|((res$tbl %like% 'stops_') & (res$count < 65))) {
		print(paste0(Sys.time(),": ",i," of ",length(tbls)," // Removing ",res$tbl," (nrow = ",res$count,")"))
		dbRemoveTable(pg,res$tbl)
	}
	else {
		print(paste0(Sys.time(),": ",i," of ",length(tbls)," // Skipping ",res$tbl," (nrow = ",res$count,")"))
	}
}
	dbDisconnect(pg)
	closeAllConnections()
	gc()
	suppressWarnings(rm(res,pg))
	pg <- dbConnect(RPostgres::Postgres()
		                 , host=Sys.getenv("pg_host")
		                 , port=Sys.getenv("pg_port")
		                 , dbname="wmata"
		                 , user=Sys.getenv("pg_user")
		                 , password=Sys.getenv("pg_password"))
	i <- i+1
}

print(paste0("End time: ",Sys.time()," (",round(difftime(Sys.time(),start_time,units = "secs"))," secs)"))
