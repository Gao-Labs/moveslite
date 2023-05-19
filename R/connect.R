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
      path = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/data.sqlite"
      # On Repository computers:
      # path = "z/db.sqlite"
      conn = dbConnect(RSQLite::SQLite(), path)
    },

    "cov" = {
      path =  "z/cov.sqlite"
      conn = dbConnect(RSQLite::SQLite(), path)
    }
  )
  return(conn)
}
