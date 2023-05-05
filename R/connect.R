#' @name connect()
#' @description Connect to a database of interest to this project. Only works on Tim's computer currently.
#' @param .type (character) "data" = Cat-formatted MOVES grand data database; "cov" = census covariates
#' @export

connect = function(.type){
  require(DBI)
  require(RSQLite)

  switch(
    EXPR = .type,
    
    "data" = {
      path = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/data.sqlite"
      conn = dbConnect(RSQLite::SQLite(), path)
    },
    
    "cov" = {
      path =  "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/cov.sqlite"
      conn = dbConnect(RSQLite::SQLite(), path)
    }
  )
  return(conn)
}