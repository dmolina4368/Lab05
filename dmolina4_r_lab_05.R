library(tidyverse)
library(terra)
library(sf)
library(leaflet) #new
library(leaflet.extras) #new
library(spdep)
library(spData)
library(dplyr)

# Task 1: Recreate Lab 2 Task 2.3

bmps <- read_csv("~/dmolina4_r_lab_05/data/CBW/BMPreport2016_landbmps.csv")

counties <- sf::read_sf("~/dmolina4_r_lab_05/data/CBW/County_Boundaries.shp")
counties %>% sf::st_is_valid()
counties <- counties %>% sf::st_make_valid() 

county_bmp_total_cost <- bmps %>%
  group_by(GeographyName) %>% 
  summarise(total_bmp_cost = sum(Cost, na.rm = TRUE)) %>%
  mutate(tempID = stringr::str_sub(GeographyName, 1, 5))

county_map_data <- counties %>%
  left_join(county_bmp_total_cost, by = c("GEOID10" = "tempID"))

pal = colorNumeric("YlOrRd", domain = county_map_data$total_bmp_cost,n=5)

leaflet(data = county_map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = county_map_data, fillColor = ~pal(total_bmp_cost), stroke = TRUE, fillOpacity = 0.9, color = "black",
              weight = 0.3, popup = ~paste0(GeographyName, ": ", total_bmp_cost)) %>%
  addLegend(pal = pal, values = ~total_bmp_cost, title = "Total BMP Cost", position = "bottomright", bins = 5)

# Task 2: Recreate Lab 3 Bonus Task 2
iailmowi <- sf::read_sf("~/dmolina4_r_lab_05/data/Lab3-main/task1subset.shp")

nb <- poly2nb(iailmowi)
lw <- nb2listw(nb, style = "W")

local_moran <- localmoran(iailmowi$B01001e1, lw)

z <- scale(iailmowi$B01001e1)[,1]
lag_z <- lag.listw(lw, z)

p_values <- local_moran[, "Pr(z != E(Ii))"]
lisa_quadrants <- case_when(
  z > 0 & lag_z > 0 & p_values < 0.05 ~ "HH",
  z > 0 & lag_z < 0 & p_values < 0.05 ~ "HL",
  z < 0 & lag_z < 0 & p_values < 0.05 ~ "LL",
  z < 0 & lag_z > 0 & p_values < 0.05 ~ "LH",
  TRUE ~ "Not significant"
)
iailmowi$lisa_quad <- factor(
  lisa_quadrants,
  levels = c("HH", "HL", "LL", "LH", "Not significant")
)
quad_palette <- colorFactor(
  palette = c("purple", "yellow", "red", "lightgreen", "blue"),
  domain = iailmowi$lisa_quad
)

leaflet(iailmowi) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Streets") %>%
  addPolygons(
    fillColor = ~quad_palette(lisa_quad),
    color = "black",
    weight = 1,
    popup = ~paste0(
      "<strong>County:</strong> ", NAME, "<br>",
      "<strong>LISA Quadrant:</strong> ", lisa_quad, "<br>",
      "<strong>p-value:</strong> ", p_values
    ),
    label = ~paste0(NAME, ": ", lisa_quad),
    group = "LISA Clusters"
  ) %>%
  addLegend(
    pal = quad_palette,
    values = ~lisa_quad,
    title = "LISA Cluster Type",
    position = "bottomright"
  ) %>%
  addLayersControl(
    baseGroups = c("Light", "Satellite", "Streets"),
    overlayGroups = c("LISA Clusters"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Task 3: Recreate Lab 4 Task 2
neoh_dem <- terra::rast("~/dmolina4_r_lab_05/data/static_mapping/neoh_dem.tif")
ohparks <- st_read("~/dmolina4_r_lab_05/data/static_mapping/oh_parks.gpkg")
oh_counties <- st_read("~/dmolina4_r_lab_05/data/static_mapping/oh_counties.gpkg")

cuyahogacounty <- filter(ohcounties, NAME %in% c("Cuyahoga"))

st_crs(ohparks)
st_crs(cuyahogacounty)

parks <- st_transform(ohparks, st_crs(cuyahogacounty))
cuyahogaparks <- st_intersection(parks, cuyahogacounty)

st_crs(neoh_dem)
st_crs(ohcounties)

neoh_dem_rp <- project(neoh_dem, crs(ohcounties))

portsum <- ohcounties %>% dplyr::filter(NAME=="Cuyahoga")

x <- vect(portsum)

demposu <- terra::crop(neoh_dem_rp, x)

parks_sf <- st_as_sf(cuyahogaparks)
dem_raster_vals <- terra::values(demposu)

dem_raster_vals <- dem_raster_vals[!is.na(dem_raster_vals)]

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addRasterImage(demposu, colors = terrain.colors(10), opacity = 0.7, group = "Elevation") %>% 
  addPolygons(data = parks_sf, color = "green", weight = 1, opacity = 0.7, fillOpacity = 0.3, group = "Parks") %>% 
  addLayersControl(
    overlayGroups = c("Elevation", "Parks"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(position = "bottomright", 
            pal = colorNumeric("YlGnBu", domain = dem_raster_vals),
            values = dem_raster_vals,
            title = "Elevation (m)")
