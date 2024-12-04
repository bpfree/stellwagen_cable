################################################
### 31. Stellwagen routing ###
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
#### National Marine Sanctuary
stellwagen_dir <- "data/a_raw_data/sbnms_py2"

#### cost raster surface
raster_dir <- "data/d_raster_data"

#### start-end lines
lines_dir <- "data/c_analysis_data/wind.gpkg"

#####################################

### output directory
output_gpkg <- "data/c_analysis_data/stellwagen_lines.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = lines_dir,
              do_count = T)

#####################################
#####################################

# load data
## Stellwagen National Marine Sanctuary 
stellwagen <- sf::st_read(dsn = stellwagen_dir) %>%
  # change to matching CRS
  sf::st_transform(x = .,
                   crs = crs) %>%
  # add a 500-meter buffer so all starting and ending points exist 
  sf::st_buffer(x = .,
                dist = setback)

## cost raster
cost_rm_barriers <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_sediment_update_costs_rm_barriers_without_coral_boulder_{cell_size}m.grd"))) %>%
  # reclassify the values to have values only between minimum and maximum
  terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))

## Stellwagen starting-ending lines
start_end_lines <- sf::st_read(dsn = lines_dir,
                               layer = sf::st_layers(dsn = lines_dir)[[1]][[grep(pattern = "lines",
                                                                             x = sf::st_layers(dsn = lines_dir)[[1]])]])

#####################################
#####################################

# Stellwagen cost raster
stellwagen_cost <- cost_rm_barriers %>%
  terra::crop(x = .,
              y = stellwagen,
              mask = T)

plot(stellwagen_cost)

#####################################

# buffered starting-ending lines
lines_buffered <- start_end_lines %>%
  sf::st_buffer(x = .,
                dist = setback)

plot(lines_buffered$geom)

#####################################
#####################################

# calculate costs for each connection avenue
lines_costs <- start_end_lines %>%
  # create a "cost_max" field and populate it with the maximum, mean, and sum cost values of the buffered lines (1000m)
  dplyr::mutate(cost_max = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                                        # for each line
                                                        y = lines_buffered,
                                                        # calculate the maximum cost
                                                        fun = 'max'),
                
                # mean cost
                cost_mean = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                                         # for each line
                                                         y = lines_buffered,
                                                         # calculate the mean cost
                                                         fun = 'mean'),
                
                # summed cost
                cost_sum = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                                        # for each line
                                                        y = lines_buffered,
                                                        # calculate the sum cost
                                                        fun = 'sum'),
                
                # line length (in meters)
                line_length = sf::st_length(.),
                
                # average cost per length
                line_cost_avg = units::drop_units(cost_sum / line_length))

#####################################
#####################################

# histograms
hist(lines_costs$cost_sum)
hist(lines_costs$line_length)
hist(lines_costs$line_cost_avg)

#####################################
#####################################

# export data
## lines data
sf::st_write(obj = lines_costs, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_end_lines_costs"), append = F)
sf::st_write(obj = lines_buffered, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_end_buffered_lines"), append = F)
sf::st_write(obj = stellwagen, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_boundary_{setback}m"), append = F)

## costs surface
terra::writeRaster(x = stellwagen_cost, filename = file.path(raster_dir, stringr::str_glue("{region_name}_nms_boundary_{setback}m_{cell_size}m.grd")), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
