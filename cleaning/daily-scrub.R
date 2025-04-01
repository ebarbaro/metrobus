rm(list = ls(all.names = TRUE)) 
closeAllConnections()
start_time <- Sys.time()
print(paste0(Sys.getpid()," - Start time: ",start_time))
date <- as.character(str_sub(Sys.time(),1,10))

{
  if (Sys.info()['sysname'] == "Linux") {
    setwd("/home/eab")
    wd <- "/home/eab"
    path <- "/home/eab/Projects/metrobus"
    }
    else if (Sys.info()['sysname'] == "Windows") {
      setwd("C:/Users/ebarbaro")
      wd <- "C:/Users/ebarbaro"
      path <- "C:/Users/ebarbaro/R/Sandbox/metrobus"
    }
}


{
  if (Sys.info()['sysname'] == "Linux") {
    dirs <- list.dirs(path)
    dirs <- subset(dirs, !(dirs %like% "/home/eab/Projects/metrobus/.git")
                        &!(dirs %like% "/home/eab/Projects/metrobus/.Rproj.user")
                        &!(dirs %like% "/home/eab/Projects/metrobus/routes/images"))
  }
  else if (Sys.info()['sysname'] == "Windows") {
    dirs <- list.dirs(path)
    dirs <- subset(dirs, !(dirs %like% "C:/Users/ebarbaro/R/Sandbox/metrobus/.git")&!(dirs %like% "C:/Users/ebarbaro/R/Sandbox/metrobus/.Rproj.user"))
  }
}

#dirs
dirs <- as.data.frame(dirs)
dirs <- dirs[order(-xtfrm(dirs$dirs), decreasing = FALSE),]
dirs <- as.data.frame(dirs)
d <- 1
would_delete <- data.frame()
would_not_delete <- data.frame()

for (d in d:nrow(dirs)) {

  suppressWarnings(rm(sub_dirs,sd,f,files))
  sd <- file.info(dirs$dirs[d], full.names = FALSE, recursive = FALSE)
  sd$DirPath <- row.names(sd)
  sub_dirs <- sd[!is.na(sd$isdir),] 
  #print(paste0(sub_dirs$DirPath," atime = ",sub_dirs$atime))

  {
    if ((sub_dirs$DirPath %like% year(Sys.Date()))&(sub_dirs$atime < Sys.Date())) {
      unlink(dirs$dirs[d], recursive = TRUE)
      df <- data.frame(
        path = sub_dirs$DirPath,
        type = "folder"
        )
      would_delete <- bind_rows(would_delete,df)
      suppressWarnings(rm(df))
    }
    
    else if (!(sub_dirs$DirPath %like% year(Sys.Date()))) {
      files <- file.info(list.files(sub_dirs$DirPath, full.names = TRUE, recursive = TRUE))
     
      {
        if (nrow(files)>0) { 
            files$FilePath <- row.names(files)
            files$FileName <- sub("^(?:[^/]*/)*\\s*(.*)", "\\1", files$FilePath)
            f <- 1
            
            for (f in f:nrow(files)) {
              fx <- files[f,]
              #print(paste0(Sys.time(),": Checking ",f," of ",nrow(files)," ",fx$FilePath,"...."))
              {
                  if (fx$isdir == TRUE | (!(fx$FileName %like% ".txt")&!(fx$FileName %like% ".R")&!(fx$FileName %like% ".r")&!(fx$FileName %like% ".sql"))) {
                          df <- data.frame(
                                    path = fx$FilePath,
                                    type = "file"
                                    )
                          would_delete <- bind_rows(would_delete,df)
                          suppressWarnings(rm(df))
                          file.remove(fx$FilePath)
                          #print(paste0("Removed ",fx$FilePath))
                  } 
                  else {
                          df <- data.frame(
                                    path = fx$FilePath,
                                    type = "file"
                                    )
                          would_not_delete <- bind_rows(would_not_delete,df)
                          suppressWarnings(rm(df))
                          #print(paste0("Skipped ",fx$FilePath))
                  }               
              }
              suppressWarnings(rm(fx))
              f <- f+1
          }
        }
        else {
          #print(paste0(Sys.time(),": No eligible files found for deletion. Skipping directory..."))
          df <- data.frame(
            path = sub_dirs$DirPath,
            type = "directory"
            )
          would_not_delete <- bind_rows(would_not_delete,df)
          suppressWarnings(rm(df))
        }
      }
    }
  }
  d <- d + 1
  #print(paste0(Sys.time(),": Cleaned directory ",d," of ",nrow(dirs)))
}

#### scrubadubdub :)
rm(list=setdiff(ls(),c("date","path","start_time","wd","would_delete","would_not_delete")))
invisible(gc())

if (!dir.exists(paste0(path,"/logs/",date))) { dir.create(paste0(path,"/logs/",date)) }

write.table(would_delete,paste0(path,"/logs/",date,"/deleted_",date,".txt"))
write.table(would_not_delete,paste0(path,"/logs/",date,"/not_deleted_",date,".txt"))

df_would_delete <- data.frame(would_delete)
print(paste0(Sys.time(),": Good morning!!!!! Here are the files I deleted:"))
print(df_would_delete)

#### remove old db records
print(paste0(Sys.time(),": Deleting old records from log table..."))
pg <- dbConnect(RPostgres::Postgres()
                , host=Sys.getenv("pg_host")
                , port=Sys.getenv("pg_port")
                , dbname="wmata"
                , user=Sys.getenv("pg_user")
                , password=Sys.getenv("pg_password"))

uu <- suppressWarnings(dbSendQuery(pg,"delete from public.log where trigger_timestamp < now() - interval '7 days'"))
print(uu)
dbClearResult(uu)
dbDisconnect(pg)
closeAllConnections()

#### run autoupdate
print(paste0(Sys.time(),": Fetching updates. Please hold...."))
system("sudo apt-get update && sudo apt-get -y upgrade",wait = TRUE)
print(paste0(Sys.time(),": Updates complete! Rebooting. Bye!!!"))
system("sudo reboot",wait = TRUE)
