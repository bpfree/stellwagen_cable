##############################
### 31. Stellwagen routing ###
##############################

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

## circle area 
area_limit <- pi * setback ** 2

## area factor (multiply area limit by 1.5 to estimate that each polygon might have an additional 25% -- so will be 150% of area limit)
area_factor <- 1.5

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
                   crs = crs)

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

## sanctuary with a 500-m buffer
stellwagen_buffer <- stellwagen %>%
  # add a 500-meter buffer so all starting and ending points exist 
  sf::st_buffer(x = .,
                dist = setback)

# Stellwagen cost raster
stellwagen_cost <- cost_rm_barriers %>%
  terra::crop(x = .,
              y = stellwagen_buffer,
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

#####################################
#####################################

# create lines that intersect with barriers
corridors_no_go <- as.vector(as.data.frame(sf::st_intersects(x = barriers,
                                                             y = lines_buffered))$col.id)

# # create lines fully contained within Stellwagen
# corridors_stellwagen <- as.vector(as.data.frame(sf::st_contains(x = stellwagen,
#                                                                 y = start_end_lines))$col.id)

#####################################
#####################################

# get lines that are fully contained within Stellwagen
## given that all the lines extend beyond the national marine sanctuary boundary, have to get creative
## on how to determine corridors that leave and re-enter (in south-east section and north-west section)

### in an effort to do this, the sections of the corridors were eliminated if they comprised of more than two parts outside the boundary;
### thus the first step was to get only the sections outside the boundary
### second, calculate the number of parts for a particular corridor were outside the boundary (anything with more than 3 got eliminated)
### third, based on the area calculations, remove the corridors with the largest sections -- this got tricky as the math is not perfect nor as expected
### due to that the boundary is not perfectly straight lines, so at times more than half a circle gets generated on the sides

areas_outside <- lines_buffered %>%
  # erase any areas that fall within the national marine sanctuary
  sf::st_difference(x = lines_buffered,
                    # use the national marine sanctuary 
                    y = stellwagen) %>%
  # calculate the area outside the sanctuary, total area, and percentage of area outside the sanctuary
  dplyr::mutate(outside_area = units::drop_units(sf::st_area(.)), # total area of the outside area
                # total area -- with units (m) dropped
                total_area = units::drop_units(sf::st_area(lines_buffered)),
                # outside percentage
                outside_pct = outside_area / total_area * 100)

## inspect the types of geometry for the newly created areas outside the sanctuary
list(unique(sf::st_geometry_type(areas_outside)))

#####################################

# calculate the number of polygons for each corridor outside the sanctuary
areas_outside_many <- areas_outside %>%
  # since not all geometries are MULTIPOLYGON, need to cast to that type
  sf::st_cast(to = "MULTIPOLYGON") %>%
  # now can cast to POLYGON to get each unique polygon
  sf::st_cast(to = "POLYGON") %>%
  # group by the corridor (row)
  dplyr::group_by(row) %>%
  # and count the total number of instances of the row
  dplyr::summarise(count = n())

## check the geometry types to ensure they are only POLYGON
list(unique(sf::st_geometry_type(areas_outside_many)))

#####################################

# corridors with 2 polygons
areas_with2 <- areas_outside_many %>%
  # limit to only corridors with two polygons
  dplyr::filter(count <= 2)

# corridors that have polygons outside that have a maximum of 150% of circle area limit
areas2_inside <- areas_with2 %>%
  # calculate total area of the corridor outside the sanctuary
  dplyr::mutate(total_area = units::drop_units(sf::st_area(.))) %>%
  # area of two halves = full circle (area of circle = pi * r^2 = pi * 500^2 = pi * 250000 = ~ 785,398)
  ## multiply by area factor
  dplyr::filter(total_area <= area_limit * area_factor) %>%
  # drop geometry convert back to dataframe
  sf::st_drop_geometry()

# create vector of the corridors inside the sanctuary
corridor_stell_vect <- areas2_inside %>%
  # convert to dataframe
  as.data.frame() %>%
  # make a vector
  dplyr::pull(row)

#####################################

# corridors that meet the two criteria: avoids barriers areas and remains within the sanctuary
corridors_fine <- lines_buffered %>%
  # filter for corridors not in corridors no-go areas and corridors in the Stellwagen sanctuary
  dplyr::filter(!row %in% corridors_no_go & row %in% corridor_stell_vect)

## plot the corridors that meet criteria
plot(corridors_fine$geom)

#####################################
#####################################

# get the lines that match the corridors
lines_fine <- start_end_lines %>%
  # filter for lines that match corridors
  dplyr::filter(row %in% corridors_fine$row)

#####################################

# calculate costs for each connection avenue
lines_costs <- lines_fine %>%
  # create a fields and populate it with the maximum, mean, 25th and 75th quantiles, and sum cost values of the buffered lines (1000m)
  dplyr::mutate(
    # 25th quantile
    cost_q25 = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                            # for each line
                                            y = lines_fine,
                                            # calculate the quantiles
                                            fun = "quantile",
                                            # set quantiles for 25th and 75th percentiles
                                            quantiles = c(0.25)),
    
    # mean cost
    cost_mean = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                             # for each line
                                             y = lines_fine,
                                             # calculate the mean cost
                                             fun = 'mean'),
    
    # 75th quantile
    cost_q75 = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                            # for each line
                                            y = lines_fine,
                                            # calculate the quantiles
                                            fun = "quantile",
                                            # set quantiles for 25th and 75th percentiles
                                            quantiles = c(0.75)),
    
    # maximum cost value         
    cost_max = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                            # for each line
                                            y = lines_fine,
                                            # calculate the maximum cost
                                            fun = 'max'),
    # summed cost
    cost_sum = exactextractr::exact_extract(x = stellwagen_cost[[1]],
                                            # for each line
                                            y = lines_fine,
                                            # calculate the sum cost
                                            fun = 'sum'),
    
    # line length (in meters)
    line_length = sf::st_length(.),
    
    # average cost per length
    line_cost_avg = units::drop_units(cost_sum / line_length)) %>%
  
  # join the 
  dplyr::left_join(x = .,
                   y = areas2_inside,
                   by = "row")

#####################################
#####################################

plot(lines_costs$geom)

# histograms
hist(lines_costs$cost_sum)
hist(lines_costs$line_length)
hist(lines_costs$line_cost_avg)

#####################################
#####################################

# export data
## lines data
sf::st_write(obj = lines_costs, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_end_lines_costs"), append = F)

# sf::st_write(obj = corridors_no_go, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_end_corridors_no_go"), append = F)
# sf::st_write(obj = corridors_stellwagen, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_end_corridors_nms"), append = F)
sf::st_write(obj = lines_fine, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_end_lines_fine"), append = F)
sf::st_write(obj = corridors_fine, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_corridors_fine"), append = F)

sf::st_write(obj = lines_buffered, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_end_buffered_lines"), append = F)
sf::st_write(obj = stellwagen, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_boundary_{setback}m"), append = F)

## costs surface
terra::writeRaster(x = stellwagen_cost, filename = file.path(raster_dir, stringr::str_glue("{region_name}_nms_boundary_{setback}m_{cell_size}m.grd")), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
