library(leaflet)

mapMake <- function(latP = 50.87959, lngP = 4.70093){
  require(magrittr)
  require(leaflet)
  require(RMariaDB)
  ma <<- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    setView(zoom=4, lat = latP, lng = lngP)

  con <- dbConnect(
    drv = RMariaDB::MariaDB(),
    db = 'Opencpu'
  )

  res <- dbSendQuery(con, "SELECT SensorID, Latitude, Longitude from Sensor;")
  res2 <-dbFetch(res)

  for (row in 1:nrow(res2))
  {
    sid <- res2[row, "SensorID"]
    latid <- res2[row, "Latitude"]
    longid <- res2[row, "Longitude"]
    mapAdd(latid, longid)
  }

  dbClearResult(res)
  dbDisconnect(con)

  mapGet()
}

mapAdd <- function(latP = 48.874065, lngP = 9.596336){
  require(magrittr)
  require(leaflet)
  require(RMariaDB)
  ma <<- ma %>% addMarkers(lat = latP, lng = lngP)
  mapGet()
}


mapGet <- function(){
  ma
}
