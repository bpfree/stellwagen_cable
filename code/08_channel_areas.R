#########################
### 08. channel areas ###
#########################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region_name <- "stellwagen"

## layer names
data_name <- "channel_areas"

## coordinate reference system
### set the coordinate reference system that data should become (NAD83 UTM 19N: https://epsg.io/26919)
crs <- "EPSG:26919"

## setback distance
setback <- 600

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
#### channel areas
data_dir <- "data/a_raw_data/National_Channel_Framework"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/a_raw_data/{region_name}.gpkg")

### output directories
#### costs geopackage
output_gpkg <- "data/b_intermediate_data/costs.gpkg"

#####################################

# inspect layers within geodatabases and geopackages
sf::st_layers(dsn = data_dir,
              do_count = T)

sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# read data
## channel areas
data <- sf::st_read(dsn = data_dir) %>%
  sf::st_transform(x = .,
                   crs = crs)

### Check units for data
sf::st_crs(data, parameters = TRUE)$units_gdal
list(unique(sf::st_geometry_type(data)))

any(na.omit(st_is_valid(data)) == FALSE) # check for invalid data
any(is.na(st_is_valid(data))) # check for corrupt data
list(unique(sf::st_is_valid(data, reason = TRUE))) # list reasons for invalid data

# data <- data %>%
#   sf::st_buffer(dist = 0.0) %>%
#   sf::st_cast(x = .,
#               to = "MULTIPOLYGON")

list(unique(sf::st_is_valid(data, reason = TRUE)))

## Stellwagen region
region <- sf::st_read(dsn = study_region_gpkg,
                      layer = stringr::str_glue("{region_name}_region")) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen grid
grid <- sf::st_read(dsn = study_region_gpkg,
                    layer = stringr::str_glue("{region_name}_grid")) %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################
#####################################

# limit data to study region
data_region <- data %>%
  sf::st_buffer(x = .,
                dist = setback) %>%
  rmapshaper::ms_clip(target = .,
                      clip = region) %>%
  dplyr::mutate(layer = stringr::str_glue("{data_name}")) %>%
  dplyr::group_by(layer) %>%
  dplyr::summarise()

#####################################
#####################################

# disposal sites grid
data_region_grid <- grid[data_region, ] %>%
  # spatially join disposal sites to Stellwagen grid
  sf::st_join(x = .,
              y = data_region,
              join = st_intersects) %>%
  # select fields of importance
  dplyr::select(index, layer)

#####################################
#####################################

# export data
## costs geopackage
sf::st_write(obj = data_region_grid, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_grid"), append = FALSE)

## intermediate geopackage


#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
