################################################
### 30. costs raster -- 500m barrier setback ###
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

## setback
setback <- 500

## coordinate reference system
### set the coordinate reference system that data should become (NAD83 UTM 19N: https://epsg.io/26919)
crs <- "EPSG:26919"

## designate date
date <- format(Sys.Date(), "%Y%m%d")

#####################################
#####################################

# # cost_function
# cost_function <- function(cost_layer, field_name, cost_value){
#   # add cost value
#   data <- cost_layer %>%
#     # create new field to add cost value
#     dplyr::mutate({{field_name}} := cost_value) %>%
#     # remove geometry so it is simplified data frame
#     sf::st_drop_geometry() %>%
#     # select fields of interest
#     dplyr::select(index, {{field_name}}) %>%
#     # due to different types leading to the same score, need to remove duplicates
#     ## group by unique indexes and values
#     ### using column position (1 = index, 2 = cost value for type)
#     dplyr::group_by_at(1:2) %>%
#     ## summarise to remove duplicates
#     dplyr::summarise()
# }

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
data_dir <- "data/b_intermediate_data/costs.gpkg"
grid_dir <- "data/a_raw_data/stellwagen.gpkg"
raster_dir <- "data/d_raster_data"

### output directory
output_gpkg <- "data/c_analysis_data/costs.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

sf::st_layers(dsn = grid_dir,
              do_count = TRUE)

#####################################
#####################################

# load data
## vector grid
# grid <- sf::st_read(dsn = grid_dir, layer = stringr::str_glue("{region_name}_grid"))

## raster grid
# raster <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_study_area_{cell_size}m.grd")))

## costs grid
### all costs
costs <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_costs_{cell_size}m.grd")))
costs_conmapsg <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_costs_{cell_size}m.grd")))

## barriers grid
### all barriers
barriers_500m <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_barriers_{setback}m_{cell_size}m.grd")))
barriers_without_coral_500m <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_barriers_without_coral_{setback}m_{cell_size}m.grd")))

#####################################

# cost rasters
## cost raster without barriers with 500m setback
cost_rm_barriers_500m <- c(costs,
                      barriers_500m) %>% 
  # sum the two layers while removing any NA values
  terra::app(sum, na.rm = T) %>%
  # add 0.01 so there are no 0 values
  sum(., 0.01)
plot(cost_rm_barriers_500m)

# make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
cost_rm_barriers_500m[cost_rm_barriers_500m >= 99] <- NA
cost_rm_barriers_500m[cost_rm_barriers_500m == 0.01] <- NA
plot(cost_rm_barriers_500m)

#####################################

## cost raster without barriers without coral
cost_rm_barriers_without_coral_500m <- c(costs,
                                    barriers_without_coral_500m) %>% 
  # sum the two layers while removing any NA values
  terra::app(sum, na.rm = T) %>%
  # add 0.01 so there are no 0 values
  sum(., 0.01)
plot(cost_rm_barriers_without_coral_500m)

# make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
cost_rm_barriers_without_coral_500m[cost_rm_barriers_without_coral_500m >= 99] <- NA
cost_rm_barriers_without_coral_500m[cost_rm_barriers_without_coral_500m == 0.01] <- NA
plot(cost_rm_barriers_without_coral_500m)

#####################################

## cost raster with CONMAPSG update and without barriers with 500m setback
cost_conmapsg_update_rm_barriers_500m <- c(costs_conmapsg,
                           barriers_500m) %>% 
  # sum the two layers while removing any NA values
  terra::app(sum, na.rm = T) %>%
  # add 0.01 so there are no 0 values
  sum(., 0.01)
plot(cost_conmapsg_update_rm_barriers_500m)

# make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
cost_conmapsg_update_rm_barriers_500m[cost_conmapsg_update_rm_barriers_500m >= 99] <- NA
cost_conmapsg_update_rm_barriers_500m[cost_conmapsg_update_rm_barriers_500m == 0.01] <- NA
plot(cost_conmapsg_update_rm_barriers_500m)

#####################################

## cost raster with CONMAPSG update and without barriers without coral
cost_conmapsg_update_rm_barriers_without_coral_500m <- c(costs_conmapsg,
                                         barriers_without_coral_500m) %>% 
  # sum the two layers while removing any NA values
  terra::app(sum, na.rm = T) %>%
  # add 0.01 so there are no 0 values
  sum(., 0.01)
plot(cost_conmapsg_update_rm_barriers_without_coral_500m)

# make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
cost_conmapsg_update_rm_barriers_without_coral_500m[cost_conmapsg_update_rm_barriers_without_coral_500m >= 99] <- NA
cost_conmapsg_update_rm_barriers_without_coral_500m[cost_conmapsg_update_rm_barriers_without_coral_500m == 0.01] <- NA
plot(cost_conmapsg_update_rm_barriers_without_coral_500m)

#####################################
#####################################

# export data
## raster data
terra::writeRaster(cost_rm_barriers_500m, filename = file.path(raster_dir, stringr::str_glue("{region_name}_costs_rm_barriers_{setback}m_{cell_size}m.grd")), overwrite = T)
terra::writeRaster(cost_rm_barriers_without_coral_500m, filename = file.path(raster_dir, stringr::str_glue("{region_name}_costs_rm_barriers_without_coral_{setback}m_{cell_size}m.grd")), overwrite = T)

terra::writeRaster(cost_conmapsg_update_rm_barriers_500m, filename = file.path(raster_dir, stringr::str_glue("{region_name}_costs_conmapsg_update_rm_barriers_{setback}m_{cell_size}m.grd")), overwrite = T)
terra::writeRaster(cost_conmapsg_update_rm_barriers_without_coral_500m, filename = file.path(raster_dir, stringr::str_glue("{region_name}_costs_conmapsg_update_rm_barriers_without_coral_{setback}m_{cell_size}m.grd")), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
