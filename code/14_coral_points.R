########################
### 14. coral points ###
########################

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
data_name <- "coral_points"

## coordinate reference system
### set the coordinate reference system that data should become (NAD83(2011) / Massachusetts Mainland: https://epsg.io/6492)
#### ***note: units are in feet (not meters)
crs <- "EPSG:6492"

## setback distance
setback <- 675

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
#### coral points
data_dir <- "data/a_raw_data/barriers.gpkg"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/a_raw_data/{region_name}.gpkg")

### output directories
#### barriers geopackage
output_gpkg <- "data/b_intermediate_data/barriers.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = T)

#####################################
#####################################

# read data
## coral points
data <- sf::st_read(dsn = data_dir,
                    layer = stringr::str_glue("{data_name}")) %>%
  sf::st_transform(x = .,
                   crs = crs)

### Check units for data
sf::st_crs(data, parameters = TRUE)$units_gdal

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
  # sf::st_buffer(x = .,
  #               dist = setback) %>%
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
