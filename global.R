#Clean up
rm(list = ls())
gc()
cat("\014")

# Set color scheme
background_light <- "#666666"
text_bright      <- "#f6f6f6"
value_col <- "#8FDB8F"


#Functions
add_pth <- ""
source(paste0(add_pth, "www/functions/functions_plot.R"), encoding = "UTF-8")
source(paste0(add_pth, "www/functions/functions_dt.R"), encoding = "UTF-8")
source(paste0(add_pth, "www/functions/functions_db.R"), encoding = "UTF-8")
source(paste0(add_pth, "www/functions/functions_base.R"), encoding = "UTF-8")

# Load model for matrix computation duration prediction
load(file = "matrix_computation_time_fit.RData")

# Setup database
# establishSqliteDatabaseConnection(path = <path_to_newly_created_sqlite_db>)
# dbListTables(dbConn)
# MATCHER_PROJECTS <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ID", "NAME", "CREATIONDATE"))
# MATCHER_REF_COLUMN <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("PROJECT_ID", "COLUMN_NAME"))
# MATCHER_CODE_COLUMN <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("PROJECT_ID", "COLUMN_NAME"))
# dbWriteTable(dbConn, name = "MATCHER_PROJECTS", value = MATCHER_PROJECTS, overwrite = TRUE)
# dbWriteTable(dbConn, name = "MATCHER_REF_COLUMN", value = MATCHER_REF_COLUMN, overwrite = TRUE)
# dbWriteTable(dbConn, name = "MATCHER_CODE_COLUMN", value = MATCHER_CODE_COLUMN, overwrite = TRUE)
# closeSqliteDatabaseConnection()


# Get projects
establishSqliteDatabaseConnection()
MATCHER_PROJECTS <- dbGetQuery(dbConn, "SELECT * FROM MATCHER_PROJECTS")
closeSqliteDatabaseConnection()
