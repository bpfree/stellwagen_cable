###########################
### 32. Top route paths ###
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

## top routes
top_n <- 10

## coordinate reference system
### set the coordinate reference system that data should become (NAD83 UTM 19N: https://epsg.io/26919)
crs <- "EPSG:26919"

## designate date
date <- format(Sys.Date(), "%Y%m%d")

#####################################
#####################################

# load packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(renv,
#                dplyr,
#                ggplot2,
#                janitor,
#                plyr,
#                purrr,
#                rmapshaper,
#                sf,
#                sp,
#                stringr,
#                targets,
#                terra, # is replacing the raster package
#                tidyr)

librarian::shelf(renv,
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
#### Stellwagen boundaries
boundary_dir <- "data/c_analysis_data/stellwagen_boundary.gpkg"

#### points data
points_dir <- "data/c_analysis_data/wind.gpkg"

#####################################

### output directory
output_gpkg <- "data/c_analysis_data/wind.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = boundary_dir,
              do_count = T)

sf::st_layers(dsn = points_dir,
              do_count = T)

#####################################
#####################################

# load data
## Stellwagen north
stellwagen_north <- sf::st_read(dsn = boundary_dir,
                                layer = sf::st_layers(dsn = boundary_dir)[[1]][[grep(pattern = "north",
                                                                          x = sf::st_layers(dsn = boundary_dir)[[1]])]])

## Stellwagen south
stellwagen_south <- sf::st_read(dsn = boundary_dir,
                                layer = sf::st_layers(dsn = boundary_dir)[[1]][[grep(pattern = "south",
                                                                             x = sf::st_layers(dsn = boundary_dir)[[1]])]])

## Stellwagen central
stellwagen_central <- sf::st_read(dsn = boundary_dir,
                                layer = sf::st_layers(dsn = boundary_dir)[[1]][[grep(pattern = "central",
                                                                                     x = sf::st_layers(dsn = boundary_dir)[[1]])]])

#####################################

## points
stell_east_pts <- sf::st_read(dsn = points_dir,
                              layer = sf::st_layers(dsn = points_dir)[[1]][[grep(pattern = "east",
                                                                                 x = sf::st_layers(dsn = points_dir)[[1]])]])

stell_west_pts <- sf::st_read(dsn = points_dir,
                              layer = sf::st_layers(dsn = points_dir)[[1]][[grep(pattern = "west",
                                                                                 x = sf::st_layers(dsn = points_dir)[[1]])]])

#####################################
#####################################

# limit the points depending on the boundary
## north
### starts
north_start <- stell_east_pts %>%
  rmapshaper::ms_clip(target = .,
                      clip = stellwagen_north)

### ends
north_end <- stell_west_pts %>%
  rmapshaper::ms_clip(target = .,
                      clip = stellwagen_north)

#####################################

## central
### starts
central_start <- stell_east_pts %>%
  rmapshaper::ms_clip(target = .,
                      clip = stellwagen_central)

### ends
central_end <- stell_west_pts %>%
  rmapshaper::ms_clip(target = .,
                      clip = stellwagen_central)

#####################################

## south
### starts
south_start <- stell_east_pts %>%
  rmapshaper::ms_clip(target = .,
                      clip = stellwagen_south)

### ends
south_end <- stell_west_pts %>%
  rmapshaper::ms_clip(target = .,
                      clip = stellwagen_south)

#####################################
#####################################

