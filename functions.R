library(DBI)
library(RPostgres)
library(sf)

load_balanco_hidrico <- function() {
  con <- dbConnect(RPostgres::Postgres(),
                   user = "postgres",
                   password = "senha",
                   dbname = "tdb_final",
                   host = "localhost",
                   port = 5432)
  
  sf_data <- st_read(con, query = "SELECT * FROM public.unidades_balanco_hidrico")
  
  dbDisconnect(con)

  return(sf_data)
}
