#' @name table_exists
#' @title table_exists
#' @description Helper function to check if the supplied database table exists
#' @param .db database connection object (from `DBI` package)
#' @param .table name of table potentially the supplied database `db`. Eg. d36109 is data for geoid 36109.
#' @importFrom DBI dbListTables
#' @export

table_exists = function(.db, .table = "d36109"){
  # Get tables from database
  tables = db %>% dbListTables()
  # Is that table in your database? TRUE or FALSE
  check = .table %in% tables
  return(check)
}
