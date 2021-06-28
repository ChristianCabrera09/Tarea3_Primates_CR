# Carga de paquetes
library(dplyr)
library(DT)
library(plotly)
library(sf)
library(leaflet)
library(raster)
library(rgdal)

# Cargar de los datos
primates <-
  st_read(
    'https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/gbif/primates-cr-registros.csv',
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Asignacion de CRS
st_crs(primates) = 4326

# Datos geoespaciales de cantones
cantones <-
  st_read(
    "https://raw.githubusercontent.com/gf0604-procesamientodatosgeograficos/2021i-datos/main/ign/delimitacion-territorial-administrativa/cr_cantones_simp_wgs84.geojson",
    quiet = TRUE
  )

# Cruce de datos de cantones
primates <- 
  primates %>%
  st_join(cantones["canton"])

# Tabla de registros de presencia
primates %>%
  st_drop_geometry() %>%
  dplyr::select(family, species, stateProvince, canton, eventDate) %>%
  datatable(colnames = c(
    "Familia",
    "Especie",
    "Provincia",
    "Cantón",
    "Fecha"), 
    options = list(searchHighlight = TRUE,
                   language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json'),
                   pageLength = 10),
    class = 'cell-border stripe'
  )

# Asignación de paleta de color
colors <- c('#FF00FF','#60DE63','#E983A0')

# Gráfico tipo pastel
Grafico_pastel <-  plot_ly(primates, labels =  ~species, type = 'pie',
                           textposition = 'inside',
                           insidetextfont = list(color = '#FFFFFF'),
                           textinfo = 'label+percent',
                           hoverinfo = "value",
                           showlegend = FALSE,
                           marker = list(colors = colors, line = list(color = "#ffffff", width = 1))
) %>%
  layout(title = 'Registro de primates en Costa Rica') %>%
  config(locale = "es")

Grafico_pastel

# Descarga de capa World Clim
alt <- getData(
  "worldclim",
  var = "alt",
  res = .5,
  lon = -84,
  lat = 10
)

# Capa de altitud recortada para los límites aproximados de Costa Rica
altitud <- crop(alt, extent(-86, -82.3, 8, 11.3))

# Paleta de colores
pal <- colorNumeric(
  c("#006400", "#FFFF00", "#3F2817"), 
  values(altitud), 
  na.color = "transparent"
)

# Asignar variables
Ateles_geoffroyi <- primates %>%
  filter(species == "Ateles geoffroyi")

Alouatta_palliata <- primates %>%
  filter(species == "Alouatta palliata")

Cebus_capucinus <- primates %>%
  filter(species == "Cebus capucinus")

Saimiri_oerstedii <- primates %>%
  filter(species == "Saimiri oerstedii")

# Mapa de registros de presencia
primates %>%
  dplyr::select(stateProvince,
                canton,
                eventDate,
                species) %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  addCircleMarkers(
    data = Ateles_geoffroyi,
    stroke = F,
    radius = 4,
    fillColor = '#3cbefc',
    fillOpacity = 1,
    popup = paste(
      primates$stateProvince,
      primates$canton,
      primates$eventDate,
      primates$species,
      sep = '<br/>'
    ),
    group = "Ateles geoffroyi"
  ) %>%
  addCircleMarkers(
    data = Alouatta_palliata,
    stroke = F,
    radius = 4,
    fillColor = '#f8877f',
    fillOpacity = 1,
    popup = paste(
      primates$stateProvince,
      primates$canton,
      primates$eventDate,
      primates$species,
      sep = '<br/>'
    ),
    group = "Alouatta palliata"
  ) %>%
  addCircleMarkers(
    data = Cebus_capucinus,
    stroke = F,
    radius = 4,
    fillColor = '#00c87a',
    fillOpacity = 1,
    popup = paste(
      primates$stateProvince,
      primates$canton,
      primates$eventDate,
      primates$species,
      sep = '<br/>'
    ),
    group = "Cebus capucinus"
  ) %>%
  addCircleMarkers(
    data = Saimiri_oerstedii,
    stroke = F,
    radius = 4,
    fillColor = '#9d64e2',
    fillOpacity = 1,
    popup = paste(
      primates$stateProvince,
      primates$canton,
      primates$eventDate,
      primates$species,
      sep = '<br/>'
    ),
    group = "Saimiri oerstedii"
  ) %>%
  addRasterImage(
    altitud,
    colors = pal,
    opacity = 0.8,
    group = "Altitud"
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Imágenes de ESRI"),
    overlayGroups = c("Altitud", "Ateles geoffroyi", "Alouatta palliata", "Cebus capucinus", "Saimiri oerstedii")
  ) %>%
  addMiniMap(
    tiles = providers$Stamen.OpenStreetMap.Mapnik,
    position = "bottomleft",
    toggleDisplay = TRUE
  )