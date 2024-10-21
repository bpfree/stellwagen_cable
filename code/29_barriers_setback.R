###########################################
### 29. barriers raster -- 500m setback ###
###########################################

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

## setback
setback <- 500

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
data_dir <- "data/b_intermediate_data/barriers.gpkg"
raster_dir <- "data/d_raster_data"

### output directory
output_gpkg <- "data/c_analysis_data/barriers.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = T)

#####################################
#####################################

# load raster grid
raster <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_study_area_{cell_size}m.grd")))

#####################################

# load vector data
## barriers
coral_points <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_coral_points_grid"))
sites_avoid <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_sites_to_avoid_grid"))
boulder_ridges <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_boulder_ridges_grid"))
cape_cod <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_cape_cod_limit_grid"))

#####################################
#####################################

# create barriers layers
barriers <- coral_points %>%
  # combine barriers datasets so that each is an unique row
  rbind(sites_avoid,
        boulder_ridges,
        cape_cod) %>%
  # create field called "barrier" and fill with "barrier" for summary; "value" populate with 0
  dplyr::mutate(barrier = "barrier",
                value = 0) %>%
  # group all features by the "barrier" and "value" fields to then have a single feature
  dplyr::group_by(barrier,
                  value) %>%
  # summarise data to obtain single feature that will act as a barrier
  dplyr::summarise() %>%
  # apply setback for consideration of cable
  sf::st_buffer(x = .,
                dist = setback)

barriers_without_coral <- sites_avoid %>%
  # combine barriers datasets so that each is an unique row
  rbind(boulder_ridges,
        cape_cod) %>%
  # create field called "barrier" and fill with "barrier" for summary; "value" populate with 0
  dplyr::mutate(barrier = "barrier",
                value = 0) %>%
  # group all features by the "barrier" and "value" fields to then have a single feature
  dplyr::group_by(barrier,
                  value) %>%
  # summarise data to obtain single feature that will act as a barrier
  dplyr::summarise() %>%
  # apply setback for consideration of cable
  sf::st_buffer(x = .,
                dist = setback)

#####################################
#####################################

# create barriers raster
barriers_raster_500m <- terra::rasterize(x = barriers,
                                         y = raster,
                                         field = "value")

## set values of 0 to be 99 for later cost analysis
barriers_raster_500m[barriers_raster_500m == 0] <- 99


# create barriers raster
barriers_without_coral_500m <- terra::rasterize(x = barriers_without_coral,
                                                y = raster,
                                                field = "value")

## set values of 0 to be 99 for later cost analysis
barriers_without_coral_500m[barriers_without_coral_500m == 0] <- 99

#####################################
#####################################

# export data
## least cost geopackage
sf::st_write(obj = barriers, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_barriers"), append = F)

## raster data
terra::writeRaster(barriers_raster_500m, filename = file.path(raster_dir, stringr::str_glue("{region_name}_barriers_{setback}m_{cell_size}m.grd")), overwrite = T)
terra::writeRaster(barriers_without_coral_500m, filename = file.path(raster_dir, stringr::str_glue("{region_name}_barriers_without_coral_{setback}m_{cell_size}m.grd")), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate