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

#### barriers
barrier_dir <- "data/c_analysis_data/barriers.gpkg"

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

sf::st_layers(dsn = barrier_dir,
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
                                                                                 x = sf::st_layers(dsn = lines_dir)[[1]])]]) %>%
  dplyr::mutate(row = row_number())

## barriers
barriers <- sf::st_read(dsn = barrier_dir,
                        layer = sf::st_layers(dsn = barrier_dir)[[1]][[grep(pattern = "coral_boulder",
                                                                            x = sf::st_layers(dsn = barrier_dir)[[1]])]])

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
  # give lines a buffer of 500 meters on either side for a 1000m corridor
  sf::st_buffer(x = .,
                # distance equals setback distance
                dist = setback) %>%
  # add a column that gets populated with row number
  dplyr::mutate(row = row_number())

plot(lines_buffered$geom)

#####################################
#####################################

# create lines that intersect with barriers
corridors_no_go <- as.vector(as.data.frame(sf::st_intersects(x = barriers,
                                                         y = lines_buffered))$col.id)

# create lines fully contained within Stellwagen
corridors_stellwagen <- as.vector(as.data.frame(sf::st_contains(x = stellwagen,
                                                            y = start_end_lines))$col.id)

#####################################

# generate lines that do not go through a barrier zone nor go outside the Stellwagen National Marine Sanctuary
corridors_fine <- lines_buffered %>%
  dplyr::filter(!row %in% corridors_no_go & row %in% corridors_stellwagen)

plot(lines_fine$geom)

#####################################
#####################################

lines_fine <- start_end_lines %>%
  dplyr::filter(row %in% corridors_fine$row)

#####################################
#####################################

# calculate costs for each connection avenue
lines_costs <- lines_test %>%
  # create a "cost_max" field and populate it with the maximum, mean, and sum cost values of the buffered lines (1000m)
  dplyr::mutate(cost_max = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                                        # for each line
                                                        y = lines_fine,
                                                        # calculate the maximum cost
                                                        fun = 'max'),
                
                # mean cost
                cost_mean = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                                         # for each line
                                                         y = lines_fine,
                                                         # calculate the mean cost
                                                         fun = 'mean'),
                
                # summed cost
                cost_sum = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                                        # for each line
                                                        y = lines_fine,
                                                        # calculate the sum cost
                                                        fun = 'sum'),
                
                # quantiles
                cost_quantile = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                                             # for each line
                                                             y = lines_fine,
                                                             # calculate the quantiles
                                                             fun = "quantile",
                                                             quantiles = c(0.25, 0.75)),
                
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
