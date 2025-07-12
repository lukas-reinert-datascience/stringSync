# Database functions
establishSqliteDatabaseConnection <- function(path = "www/database/database.db"){
  library(RSQLite)
  dbConn <<- dbConnect(RSQLite::SQLite(), path)
}
closeSqliteDatabaseConnection <- function(){
  RSQLite::dbDisconnect(dbConn)
  rm(dbConn, envir = .GlobalEnv)
  gc()
}
