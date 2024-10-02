###########################
### 24. least cost path ###
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
data_dir <- "data/e_least_cost_path_data/least_cost_path.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

#####################################
#####################################

# load data
## starting point

## ending points

## least cost paths
### normal barriers
normal_cost_path <- sf::st_read(dsn = data_dir,
                    layer = "stellwagen_model_2_2_corridors")

### north barriers
north_cost_path <- sf::st_read(dsn = data_dir,
                               layer = "stellwagen_model_2_2__north_corridors")

### south barriers
south_cost_path <- sf::st_read(dsn = data_dir,
                               layer = "stellwagen_model_2_2_south_corridors")
