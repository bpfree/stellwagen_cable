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

## starting point
lease <- "0567"

## designate section
section <- "full"

## cell size (in meters)
cell_size <- 50

## neighbors
neighbors <- c(4, 8, 16, 32, 48)
# neighbors <- c(4)

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

route_dir <- "data/e_least_cost_path_data/stellwagen_route_options.gpkg"

### output directory
output_gpkg <- "data/e_least_cost_path_data/lcp_r.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = points_dir,
              do_count = TRUE)

sf::st_layers(dsn = output_gpkg,
              do_count = TRUE)

sf::st_delete(dsn = output_gpkg,
              layer = "stellwagen_full_lcp_4")

#####################################
#####################################

# load data
## starting point
start_0564 <- sf::st_read(dsn = route_dir, layer = stringr::str_glue("{region_name}_0564_edge_start"))
start_0567 <- sf::st_read(dsn = route_dir, layer = stringr::str_glue("{region_name}_0567_edge_start"))

snms_east <- sf::st_read(dsn = route_dir, layer = stringr::str_glue("{region_name}_stellwagen_east"))
snms_west <- sf::st_read(dsn = route_dir, layer = stringr::str_glue("{region_name}_stellwagen_east"))

# start_point <- sf::st_read(dsn = points_dir, layer = stringr::str_glue("{region_name}_{lease}_edge_start"))

## ending points
ending_points <- sf::st_read(dsn = points_dir, layer = stringr::str_glue("{region_name}_end_points_1000m"))

## cost raster
costs_full <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_sediment_update_costs_rm_barriers_without_coral_boulder_{cell_size}m.grd"))) %>%
  # reclassify the values to have values only between minimum and maximum
  terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))

# costs_north <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_costs_sediment_updates_barriers_coral_boulder_rm_south_{cell_size}m.grd"))) %>%
#   # reclassify the values to have values only between minimum and maximum
#   terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))
# 
# costs_south <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_costs_sediment_updates_barriers_coral_boulder_rm_north_{cell_size}m.grd"))) %>%
#   # reclassify the values to have values only between minimum and maximum
#   terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))
# 
# costs_tss <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_costs_sediment_updates_barriers_coral_boulder_tss_{cell_size}m.grd"))) %>%
#   # reclassify the values to have values only between minimum and maximum
#   terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))

# terra::minmax(costs_full)
# plot(costs_full)

#####################################
#####################################

lcp_function <- function(costs, neighbors, start_point, ending_points, section, lease){
  # max <- terra::minmax(costs)[2,]
  
  new_value <- 1 / costs
  
  # terra::minmax(new_value)
  # plot(new_value)
  
  for(i in 1:length(neighbors)){
    cs <- leastcostpath::create_cs(x = new_value,
                                   # neighbors = 4, 8, 16, 32, 48
                                   neighbours = neighbors[i])
    
    lcp <- leastcostpath::create_lcp(x = cs,
                                     origin = start_point,
                                     destination = ending_points,
                                     cost_distance = TRUE) %>%
      dplyr::mutate(neighbors = neighbors[i])
    
    # tile_raster <- costs %>%
    #   as.data.frame(xy=T) %>%
    #   setNames(c("longitude", "latitude", "cost"))

    # g <- ggplot() +
    #   geom_tile(data = tile_raster, aes(x = longitude, y = latitude, fill = cost)) +
    #   geom_sf(data = start_point, color = "red", size = 2) +
    #   geom_sf(data = ending_points, size = 2, color = "black") +
    #   geom_sf(data = lcp, color = "yellow")
    # plot(g)
    
    # export data
    sf::st_write(obj = lcp, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{lease}_{section}_lcp_{neighbors[i]}"), append = F)
  }
  
}

# lcp <- lcp_function(costs = costs_south,
#                     neighbors = neighbors,
#                     start_point = start_point,
#                     ending_points = ending_points,
#                     section = section,
#                     lease = lease)

lcp <- lcp_function(costs = get(stringr::str_glue("costs_{section}")),
                    neighbors = neighbors,
                    start_point = start_0567,
                    ending_points = snms_east,
                    section = section,
                    lease = lease)
