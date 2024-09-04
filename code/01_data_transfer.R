#########################
### 01. data transfer ###
#########################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name -- "sw" for Stellwagen
region <- "sw"

## coordinate reference system
### set the coordinate reference system that data should become (NAD83 UTM 19N: https://epsg.io/26919)
crs <- "EPSG:26919"

## designate date
date <- format(Sys.Date(), "%Y%m%d")

#####################################
#####################################

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(renv,
               dplyr,
               ggplot2,
               janitor,
               plyr,
               purrr,
               rmapshaper,
               RSelenium,
               sf,
               sp,
               stringr,
               targets,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
### input directories

#### state data directory
federal_dir <- "11_Stellwagen_Cable_Routing/Data/Stellwagen_FGDB_2024_01_02nd_v01.gdb"
# data_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/02_data"
# 
# disposal_dir <- "data/a_raw_data/OceanDisposalSite/OceanDisposalSite.gpkg"
# channel_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/02_data/National_Channel_Framework_3671545617139226677/e8bd0677-da9b-4705-a389-c6a31fc03be8.gdb"
# anchorage_dir <- "data/a_raw_data/Anchorage/Anchorage.gpkg"
# habitat_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/02_data/Habitat/Habitat/Habitat.gdb"
# energy_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/02_data/EnergyAndInfrastructure/EnergyAndInfrastructure/EnergyAndInfrastructure.gdb"

conmapsg_coral_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/03_CableRouteModel/model_2/model_2_data.gdb"
cape_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/03_CableRouteModel/model_2/model_2.gdb"
stellwagen_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/02_data/StellwagenCableRoute_data.gdb"

### output directories
#### raw data geopackage
# state_gpkg <- "data/a_raw_data/state_costs.gpkg"
federal_gpkg <- "data/a_raw_data/federal_costs.gpkg"
barriers_gpkg <- "data/a_raw_data/barriers.gpkg"

#####################################

# inspect layers within geodatabases and geopackages
sf::st_layers(dsn = federal_dir,
              do_count = T)

# sf::st_layers(dsn = data_dir,
#               do_count = T)
# 
# sf::st_layers(dsn = energy_dir,
#               do_count = T)
# 
# sf::st_layers(dsn = habitat_dir,
#               do_count = T)
# 
# sf::st_layers(dsn = channel_dir,
#               do_count = T)
# 
# sf::st_layers(dsn = disposal_dir,
#               do_count = T)
# 
# sf::st_layers(dsn = anchorage_dir,
#               do_count = T)

sf::st_layers(dsn = conmapsg_coral_dir,
              do_count = T)

sf::st_layers(dsn = cape_dir,
              do_count = T)

sf::st_layers(dsn = stellwagen_dir,
              do_count = T)

#####################################
#####################################

# # load data
# ## state costs
# conmapsg <- sf::st_read(dsn = conmapsg_coral_dir, layer = "CONMAPSSG") %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ### inspect CRS values for the data
# cat(crs(conmapsg))
# st_crs(conmapsg, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### active and inactive disposal sites (source: https://marinecadastre.gov/downloads/data/mc/OceanDisposalSite.zip)
# #### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::ocean-disposal-sites
# #### metadata: https://www.fisheries.noaa.gov/inport/item/54193
# disposal_sites <- sf::st_read(dsn = disposal_dir,
#                               layer = sf::st_layers(dsn = disposal_dir)[[1]][grep(pattern = "Disposal",
#                                                                                   x = sf::st_layers(dsn = disposal_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ### inspect CRS values for the data
# cat(crs(disposal_sites))
# st_crs(disposal_sites, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### sand patches
# sand_patches <- sf::st_read(dsn = file.path(data_dir, "sand_patches.shp")) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(sand_patches))
# st_crs(sand_patches, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### channel areas
# channel_areas <- sf::st_read(dsn = channel_dir,
#                              layer = sf::st_layers(dsn = channel_dir)[[1]][grep(pattern = "Channel",
#                                                                                 x = sf::st_layers(dsn = channel_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(channel_areas))
# st_crs(channel_areas, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### anchorage areas (source: https://marinecadastre.gov/downloads/data/mc/Anchorage.zip)
# #### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::anchorages
# #### metadata: https://www.fisheries.noaa.gov/inport/item/48849
# anchorage_areas <- sf::st_read(dsn = anchorage_dir,
#                                layer = sf::st_layers(dsn = anchorage_dir)[[1]][grep(pattern = "Anchorage",
#                                                                                     x = sf::st_layers(dsn = anchorage_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(anchorage_areas))
# st_crs(anchorage_areas, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### eelgrass meadows
# eelgrass <- sf::st_read(dsn = habitat_dir,
#                         layer = sf::st_layers(dsn = habitat_dir)[[1]][grep(pattern = "Eelgrass",
#                                                                            x = sf::st_layers(dsn = habitat_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(eelgrass))
# st_crs(eelgrass, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### cable and pipelines
# cable_pipelines <- sf::st_read(dsn = energy_dir,
#                                layer = sf::st_layers(dsn = energy_dir)[[1]][grep(pattern = "CableAndPipeline",
#                                                                                  x = sf::st_layers(dsn = energy_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(cable_pipelines))
# st_crs(cable_pipelines, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### submarine cables
# submarine_cables <- sf::st_read(dsn = energy_dir,
#                                 layer = sf::st_layers(dsn = energy_dir)[[1]][grep(pattern = "Submarine",
#                                                                                   x = sf::st_layers(dsn = energy_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(submarine_cables))
# st_crs(submarine_cables, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### LNG sites
# lng_sites <- sf::st_read(dsn = energy_dir,
#                              layer = sf::st_layers(dsn = energy_dir)[[1]][grep(pattern = "LNGsites",
#                                                                                x = sf::st_layers(dsn = energy_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(lng_pipelines))
# st_crs(lng_pipelines, parameters = TRUE)$units_gdal
# 
# #####################################
# 
# ### LNG pipelines
# lng_pipelines <- sf::st_read(dsn = energy_dir,
#                              layer = sf::st_layers(dsn = energy_dir)[[1]][grep(pattern = "LNGpipelines",
#                                                                                x = sf::st_layers(dsn = energy_dir)[[1]])]) %>%
#   # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
#   sf::st_transform(x = .,
#                    crs = crs)
# 
# ## inspect CRS values for the data
# cat(crs(lng_pipelines))
# st_crs(lng_pipelines, parameters = TRUE)$units_gdal

#####################################

## federal costs
### gravel

### slope

#####################################

## barriers
### coral points
coral <- sf::st_read(dsn = conmapsg_coral_dir,
                     layer = sf::st_layers(dsn = conmapsg_coral_dir)[[1]][grep(pattern = "Coral",
                                                                      x = sf::st_layers(dsn = conmapsg_coral_dir)[[1]])]) %>%
  # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
  sf::st_transform(x = .,
                   crs = crs)

### sites to avoid
avoided_sites <-sf::st_read(dsn = federal_dir,
                            layer = sf::st_layers(dsn = federal_dir)[[1]][grep(pattern = "Avoid",
                                                                               x = sf::st_layers(dsn = federal_dir)[[1]])]) %>%
  # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
  sf::st_transform(x = .,
                   crs = crs)

### boulder ridges
boulder <- sf::st_read(dsn = federal_dir,
                       layer = sf::st_layers(dsn = federal_dir)[[1]][grep(pattern = "Boulder",
                                                                          x = sf::st_layers(dsn = federal_dir)[[1]])]) %>%
  # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
  sf::st_transform(x = .,
                   crs = crs)

### Cape Cod limit
cape <- sf::st_read(dsn = cape_dir,
                    layer = sf::st_layers(dsn = cape_dir)[[1]][grep(pattern = "CapeoftheCod_line",
                                                                    x = sf::st_layers(dsn = cape_dir)[[1]])]) %>%
  # change to correct coordinate reference system (EPSG:6492 -- NAD83 UTM 19N)
  sf::st_transform(x = .,
                   crs = crs) %>%
  # drop z-dimension
  sf::st_zm(x = .)


#####################################
#####################################

# export data
## state geopackage
# sf::st_write(obj = conmapsg, dsn = state_gpkg, layer = "conmapsg", append = F)
# sf::st_write(obj = disposal_sites, dsn = state_gpkg, layer = "disposal_sites", append = F)
# sf::st_write(obj = sand_patches, dsn = state_gpkg, layer = "sand_patches", append = F)
# sf::st_write(obj = channel_areas, dsn = state_gpkg, layer = "channel_areas", append = F)
# sf::st_write(obj = anchorage_areas, dsn = state_gpkg, layer = "anchorage_areas", append = F)
# sf::st_write(obj = eelgrass, dsn = state_gpkg, layer = "eelgrass_meadows", append = F)
# sf::st_write(obj = cable_pipelines, dsn = state_gpkg, layer = "cable_pipelines", append = F)
# sf::st_write(obj = submarine_cables, dsn = state_gpkg, layer = "submarine_cables", append = F)
# sf::st_write(obj = lng_sites, dsn = state_gpkg, layer = "lng_sites", append = F)
# sf::st_write(obj = lng_pipelines, dsn = state_gpkg, layer = "lng_pipelines", append = F)

## federal geopackage

## barriers geopackage
sf::st_write(obj = coral, dsn = barriers_gpkg, layer = "coral_points", append = F)
sf::st_write(obj = avoided_sites, dsn = barriers_gpkg, layer = "sites_to_avoid", append = F)
sf::st_write(obj = boulder, dsn = barriers_gpkg, layer = "boulder_ridges", append = F)
sf::st_write(obj = cape, dsn = barriers_gpkg, layer = "cape_cod_limit", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
