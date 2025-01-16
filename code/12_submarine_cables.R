#########################################
### 12. submarine cable and pipelines ###
#########################################

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
data_name <- "submarine_cable"
layer_name <- "SubmarineCables"

## coordinate reference system
### set the coordinate reference system that data should become (NAD83 UTM 19N: https://epsg.io/26919)
crs <- "EPSG:26919"

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
#### submarine cables and pipelines
data_dir <- "data/a_raw_data/EnergyandInfrastructure/EnergyandInfrastructure/EnergyandInfrastructure.gdb"

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
## submarine cable
data <- sf::st_read(dsn = data_dir,
                    layer = stringr::str_glue("{layer_name}")) %>%
  sf::st_transform(x = .,
                   crs = crs)

### Check units for data
sf::st_crs(data, parameters = TRUE)$units_gdal

### check feature types (will notice they are not uniform)
list(unique(sf::st_geometry_type(data)))

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
  # make all geometery types the same
  sf::st_cast(x = .,
              # type should get MULTILINESTRING
              to = "MULTILINESTRING") %>%
  # set setback distance (***note: error will occur if not setting all objects to "MULTILINESTRING" first)
  sf::st_buffer(x = .,
                dist = setback) %>%
  # limit data to only area of interest
  rmapshaper::ms_clip(target = .,
                      clip = region) %>%
  # add new field
  dplyr::mutate(layer = stringr::str_glue("{data_name}")) %>%
  # group all objects using new field
  dplyr::group_by(layer) %>%
  # reduce to single object
  dplyr::summarise() %>%
  sf::st_cast(x = .,
              to = "POLYGON")

# ***note: inspect geometries to find cable to remove (per conversation on January 16 2025)
plot(data_region$geometry[3, ])

data_region <- data_region %>%
  # remove cable that is not of interest -- for being too out of date
  filter(!row_number() == 3)

plot(data_region$geometry)

#####################################
#####################################

# submarine cables grid
data_region_grid <- grid[data_region, ] %>%
  # spatially join submarine cables to Stellwagen grid
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
