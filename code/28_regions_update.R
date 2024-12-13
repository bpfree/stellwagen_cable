################################################
### 28. regions -- costs and barriers update ###
################################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region_name <- "stellwagen"

## cell size (in meters)
cell_size <- 50

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
data_dir <- "data/a_raw_data/shippinglanes"
stellwagen_dir <- "data/a_raw_data/sbnms_py2"
grid_dir <- "data/a_raw_data/stellwagen.gpkg"
raster_dir <- "data/d_raster_data"

### output directory
output_gpkg <- "data/c_analysis_data/wind.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

#####################################
#####################################

# load data
## shipping lanes
data <- sf::st_read(dsn = data_dir) %>%
  dplyr::filter(stringr::str_detect(THEMELAYER, "Traffic Separation")) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen boundary
stellwagen <- sf::st_read(dsn = stellwagen_dir) %>%
  sf::st_transform(x = .,
                   crs = crs)

## raster grid
raster <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_study_area_{cell_size}m.grd")))

## costs raster
cost_rm_barriers <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_costs_sediment_updates_rm_barriers_without_coral_boulder_{cell_size}m.grd"))) %>%
  # reclassify the values to have values only between minimum and maximum
  terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))

terra::minmax(cost_rm_barriers)
plot(cost_rm_barriers)

#####################################
#####################################

stellwagen_regions <- stellwagen %>%
  # remove areas that are traffic separators
  rmapshaper::ms_erase(data) %>%
  # convert to polygon
  sf::st_cast(x = .,
              to = "POLYGON") %>%
  # create indices to designate region
  dplyr::mutate(index = row_number())

# north region
stellwagen_north <- stellwagen_regions %>%
  # select region
  dplyr::filter(index == 1)

# south region
stellwagen_south <- stellwagen_regions %>%
  # selection region
  dplyr::filter(index == 2)

plot(stellwagen_regions$geometry)
plot(stellwagen_north$geometry)
plot(stellwagen_south$geometry)

#####################################
#####################################

# rasterize Stellwagen regions
## north region
north_raster <- stellwagen_north %>%
  terra::rasterize(x = .,
                   y = raster)

## set values of 1 to be 99 for later cost analysis
north_raster[north_raster == 1] <- 99
plot(north_raster)

#####################################

## south region
south_raster <- stellwagen_south %>%
  terra::rasterize(x = .,
                   y = raster)

## set values of 1 to be 99 for later cost analysis
south_raster[south_raster == 1] <- 99
plot(south_raster)

#####################################
#####################################

# remove regions from costs raster
## north
costs_rm_north <- c(cost_rm_barriers,
                    north_raster) %>%
  # sum rasters
  terra::app(sum, na.rm = T) %>%
  # remove land from cost layer
  terra::crop(raster,
              mask = TRUE)

#### make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
costs_rm_north[costs_rm_north >= 99] <- NA
costs_rm_north[costs_rm_north == 0.01] <- NA
plot(costs_rm_north)

#####################################

## south
costs_rm_south <- c(cost_rm_barriers,
                    south_raster) %>%
  # sum rasters
  terra::app(sum, na.rm = T) %>%
  # remove land from cost layer
  terra::crop(raster,
              mask = TRUE)

#### make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
costs_rm_south[costs_rm_south >= 99] <- NA
costs_rm_south[costs_rm_south == 0.01] <- NA
plot(costs_rm_south)

#####################################

## TSS
costs_rm_tss <- c(cost_rm_barriers,
                  north_raster,
                  south_raster) %>%
  # sum rasters
  terra::app(sum, na.rm = T) %>%
  # remove land from cost layer
  terra::crop(raster,
              mask = TRUE)

#### make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
costs_rm_tss[costs_rm_tss >= 99] <- NA
costs_rm_tss[costs_rm_tss == 0.01] <- NA
plot(costs_rm_tss)

#####################################
#####################################

# export data
terra::writeRaster(costs_rm_north, filename = file.path(raster_dir, stringr::str_glue("{region_name}_costs_sediment_updates_barriers_coral_boulder_rm_north_{cell_size}m.grd")), overwrite = T)
terra::writeRaster(costs_rm_south, filename = file.path(raster_dir, stringr::str_glue("{region_name}_costs_sediment_updates_barriers_coral_boulder_rm_south_{cell_size}m.grd")), overwrite = T)
terra::writeRaster(costs_rm_tss, filename = file.path(raster_dir, stringr::str_glue("{region_name}_costs_sediment_updates_barriers_coral_boulder_tss_{cell_size}m.grd")), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
