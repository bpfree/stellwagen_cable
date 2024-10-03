################################
### 22. start and end points ###
################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region_name <- "stellwagen"

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
data_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/03_CableRouteModel/model_2/model_2.gdb"
lease_dir <- "data/a_raw_data/gulfofmainefsnareasgeodatabase/Gulf_of_Maine_FSN_areas_09_10_2024.gdb"

### output directory
output_gpkg <- "data/c_analysis_data/wind.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

sf::st_layers(dsn = lease_dir,
              do_count = TRUE)

#####################################
#####################################

# start point
## centralized
start_point <- sf::st_read(dsn = data_dir,
                    layer = sf::st_layers(dsn = data_dir)[[1]][grep(pattern = "2_2_corridors_start",
                                                                    x = sf::st_layers(dsn = data_dir)[[1]])]) %>%
  sf::st_zm() %>%
  sf::st_cast("POINT") %>%
  sf::st_transform(x = .,
                   crs = crs)

## edge
leases <- sf::st_read(dsn = lease_dir,
                      layer = sf::st_layers(dsn = lease_dir)[[1]][grep(pattern = "all_outlines",
                                                                          x = sf::st_layers(dsn = lease_dir)[[1]])]) %>%
  # filter for only the leases of interest (0564 and 0567)
  dplyr::filter(grepl(pattern = "0564|0567",
                      # find the pattern within the "ADDITIONAL_INFORMATION" field
                      ADDITIONAL_INFORMATION))

### lease 0564
lease_0564 <- leases %>%
  # only lease 0564
  dplyr::filter(grepl(pattern = "0564",
                      # find the pattern within the "ADDITIONAL_INFORMATION" field
                      ADDITIONAL_INFORMATION)) %>%
  # make it first a multilinestring object
  sf::st_cast(x = .,
              to = "MULTILINESTRING") %>%
  # then make it a collection of points
  sf::st_cast(x = .,
              to = "POINT") %>%
  # create fields for longitude and latitude
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

lease_0564_edge_point <- lease_0564 %>%
  # limit it to the furthest west point (xmin)
  dplyr::filter(lon <= sf::st_bbox(.)$xmin)

#####################################

### lease 0567
lease_0567 <- leases %>%
  # limit to lease 0567
  dplyr::filter(grepl(pattern = "0567",
                      # find the pattern within the "ADDITIONAL_INFORMATION" field
                      ADDITIONAL_INFORMATION)) %>%
  # make it first a multilinestring object
  sf::st_cast(x = .,
              to = "MULTILINESTRING") %>%
  # then make it a collection of points
  sf::st_cast(x = .,
              to = "POINT") %>%
  # create fields for longitude and latitude
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

lease_0567_edge_point <- lease_0567 %>%
  # limit it to the furthest west point (xmin)
  dplyr::filter(lon <= sf::st_bbox(.)$xmin)

#####################################

# combine edge points
start_edge_points <- rbind(lease_0564_edge_point,
                           lease_0567_edge_point) %>%
  sf::st_zm() %>%
  sf::st_cast("POINT") %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################
#####################################

# end point
end_points <- sf::st_read(dsn = data_dir,
                           layer = sf::st_layers(dsn = data_dir)[[1]][grep(pattern = "2_2_corridors_end",
                                                                           x = sf::st_layers(dsn = data_dir)[[1]])]) %>%
  sf::st_zm() %>%
  sf::st_cast("POINT") %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################
#####################################

# export data
sf::st_write(start_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_point"), append = FALSE)
sf::st_write(lease_0564_edge_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_0564_edge_start"), append = FALSE)
sf::st_write(lease_0567_edge_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_0567_edge_start"), append = FALSE)
sf::st_write(start_edge_points, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_edge_start_points"), append = FALSE)

sf::st_write(end_points, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_end_points"), append = FALSE)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate