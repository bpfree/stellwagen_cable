###########################
### 25. least cost path ###
###########################

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

## neighbors
neighbors <- 48

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
               leastcostpath,
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

#install.packages("devtools")
library(devtools)
install_github("josephlewis/leastcostpath") # https://github.com/josephlewis/leastcostpath
library(leastcostpath)

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
### input directories
points_dir <- "data/c_analysis_data/wind.gpkg"
raster_dir <- "data/d_raster_data"

### output directory
data_dir <- "data/e_least_cost_path_data/least_cost_path.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

#####################################
#####################################

# load data
## starting point
starting_points <- sf::st_read(dsn = points_dir, layer = stringr::str_glue("{region_name}_start_point"))

## ending points
ending_points <- sf::st_read(dsn = points_dir, layer = stringr::str_glue("{region_name}_end_points_1000m"))

## cost raster
costs <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_sediment_update_costs_rm_barriers_without_coral_boulder_{cell_size}m.grd"))) %>%
  # reclassify the values to have values only between minimum and maximum
  terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))

terra::minmax(costs)
plot(costs)

## Swapping values
max <- terra::minmax(costs)[2,]

new_value <- max - costs

terra::minmax(new_value)
plot(new_value)

#####################################
#####################################

# create slope cost surface
cs <- leastcostpath::create_cs(x = new_value,
                               # neighbors = 4, 8, 16, 32, 48
                               neighbours = neighbors)

lcp <- leastcostpath::create_lcp(x = cs,
                                 origin = starting_points,
                                 destination = ending_points,
                                 cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = neighbors)

#####################################
#####################################

tile_raster <- costs %>%
  as.data.frame(xy=T) %>%
  setNames(c("longitude", "latitude", "cost"))

g <- ggplot() +
  geom_tile(data = tile_raster, aes(x = longitude, y = latitude, fill = cost)) +
  geom_sf(data = starting_points, color = "red", size = 2) +
  geom_sf(data = ending_points, size = 2, color = "black") +
  geom_sf(data = lcp, color = "yellow")
g
