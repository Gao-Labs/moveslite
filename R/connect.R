#' @name connect()
#' @description Connect to a database of interest to this project.
#' @param .type (character) "data" = Cat-formatted MOVES grand data database
#' @export

connect = function(.type = "data"){
  require(DBI)
  require(RSQLite)

  switch(
    EXPR = .type,

    "data" = {
      # On Tim's computer:
      conn = dbConnect(
        drv = RMySQL::MySQL(),
        username = Sys.getenv("CATSERVER_USERNAME"),
        password = Sys.getenv("CATSERVER_PASSWORD"),
        host = Sys.getenv("CATSERVER_HOST"),
        port = as.integer(Sys.getenv("CATSERVER_PORT")),
        dbname = "granddata")
      # On Tim's computer:
      # path = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/data.sqlite"
      # On Yan's computer:
      # path = "z/db.sqlite"
      # On Repository computers:
      # path = "z/data.sqlite"
      # conn = dbConnect(RSQLite::SQLite(), path)
    },

    "cov" = {
      #path =  "z/cov.sqlite"
      #conn = dbConnect(RSQLite::SQLite(), path)
      # On Tim's computer:
      conn = dbConnect(
        drv = RMySQL::MySQL(),
        username = Sys.getenv("CATSERVER_USERNAME"),
        password = Sys.getenv("CATSERVER_PASSWORD"),
        host = Sys.getenv("CATSERVER_HOST"),
        port = as.integer(Sys.getenv("CATSERVER_PORT")),
        dbname = "cov")
    }
  )
  return(conn)
}
