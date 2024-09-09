########################
### 19. costs raster ###
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

## cell size
cell_size <- 100

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
study_area <- sf::st_read(dsn = grid_dir, layer = stringr::str_glue("{region_name}_region"))

## constraints grid
### all constraints
constraints <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_barriers_{cell_size}m.grd")))

#####################################

# load data
## costs layers
