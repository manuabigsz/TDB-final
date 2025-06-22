library(DBI)
library(RPostgres)
library(sf)

load_balanco_hidrico <- function() {
  
  con <- dbConnect(RPostgres::Postgres(),
                   user = "postgres",
                   password = "senha",
                   dbname = "trab_final_tdb",
                   host = "localhost",
                   port = 5432)
  
  query <- "
    SELECT 
      gid,
      bacia,
      upg,
      id_balanco,
      geom
    FROM public.unidades_balanco_hidrico
  "
  
  sf_data <- st_read(con, query = query)
  
  dbDisconnect(con)
  sf_data <- st_zm(sf_data, drop = TRUE, what = "ZM")
  
  
  return(sf_data)
}

load_raster_metadata <- function() {
  con <- dbConnect(RPostgres::Postgres(),
                   user = "postgres",
                   password = "senha",
                   dbname = "trab_final_tdb",
                   host = "localhost",
                   port = 5432)
  
  query <- "
    SELECT 
      id,
      date,
      filename
    FROM public.raster_data
    ORDER BY date DESC
  "
  
  df <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  return(df)
}

load_raster_data <- function(raster_id) {
  con <- dbConnect(RPostgres::Postgres(),
                   user = "postgres",
                   password = "senha",
                   dbname = "trab_final_tdb",
                   host = "localhost",
                   port = 5432)
  
  query <- paste0("
    SELECT 
      id,
      date,
      filename,
      ST_AsGDALRaster(rast, 'GTiff') as raster_data,
      ST_Envelope(rast) as bbox,
      ST_SRID(rast) as srid
    FROM public.raster_data 
    WHERE id = ", raster_id)
  
  result <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  return(result)
}

get_raster_info <- function(raster_id) {
  con <- dbConnect(RPostgres::Postgres(),
                   user = "postgres",
                   password = "senha",
                   dbname = "trab_final_tdb",
                   host = "localhost",
                   port = 5432)
  
  query <- paste0("
    SELECT 
      id,
      date,
      filename,
      ST_Width(rast) as width,
      ST_Height(rast) as height,
      ST_NumBands(rast) as num_bands,
      ST_SRID(rast) as srid,
      ST_AsText(ST_Envelope(rast)) as bbox_wkt
    FROM public.raster_data 
    WHERE id = ", raster_id)
  
  result <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  return(result)
}


`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x