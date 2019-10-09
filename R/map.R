library(leaflet)
getIDS <- function()
{
  require(RMariaDB)
  con <- dbConnect(
    drv = RMariaDB::MariaDB(),
    db = 'Opencpu'
  )

  res <- dbSendQuery(con, "SELECT SensorID FROM Sensor;")
  res2 <-dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  res2
}
mapMake <- function(latP = 50.87959, lngP = 4.70093){
  require(magrittr)
  require(leaflet)
  require(RMariaDB)
  require(htmltools)
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
    mapAdd(latid, longid, sid)
  }

  dbClearResult(res)
  dbDisconnect(con)

  mapGet()
}

mapAdd <- function(latP = 48.874065, lngP = 9.596336, sid = 929){
  require(magrittr)
  require(leaflet)
  require(htmlwidgets)
  require(widgetframe)
  sidString <- toString(sid)
  ma <<- ma %>% addMarkers(lat = latP, lng = lngP, popup = sidString)
  mapGet()
}


mapGet <- function(){
  require(htmlwidgets)
  require(widgetframe)

  ma

  htmlwidgets::saveWidget(widgetframe::frameableWidget(ma),
                          "mapfoto.html", selfcontained = TRUE)
}


lime <- function(){
  require(LimeRick)
  require(knitr)
  require(markdown)
  require(ggradar)
  knit("~/Survey2PDF.Rmd")
  markdownToHTML('~/Survey2PDF.md', '~/Survey2PDF.html', options=c("use_xhml"))
}
