library(DBI)
library(RPostgres)
library(sf)
library(raster)

conn <- dbConnect(
  RPostgres::Postgres(),
  user = "postgres",
  password = "senha",
  dbname = "tdb_final",
  host = "localhost",
  port = 5432
)

onStop(function() {
  if (dbIsValid(conn)) dbDisconnect(conn)
})


load_balanco_hidrico <- function() {
  query <- "
    SELECT 
      gid,
      bacia,
      upg,
      id_balanco,
      geom
    FROM public.unidades_balanco_hidrico
  "
  
  sf_data <- st_read(conn, query = query)  # usa conn global
  sf_data <- st_zm(sf_data, drop = TRUE, what = "ZM")  # remove Z/M
  
  return(sf_data)
}

load_raster_files <- function() {
  dbGetQuery(conn, "SELECT id, filename FROM raster_data ORDER BY date DESC")
}

load_raster_by_id <- function(id) {
  query <- paste0("
    SELECT ST_AsGDALRaster(rast, 'GTiff') AS rast_bin 
    FROM raster_data 
    WHERE id = ", id)
  
  res <- dbSendQuery(conn, query)
  on.exit(dbClearResult(res), add = TRUE)
  
  bin_result <- dbFetch(res)
  
  if (nrow(bin_result) == 0 || !is.raw(bin_result$rast_bin[[1]])) {
    stop("O raster retornado está vazio ou inválido.")
  }
  
  temp_file <- tempfile(fileext = ".tif")
  writeBin(bin_result$rast_bin[[1]], temp_file)
  
  raster::raster(temp_file)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}
