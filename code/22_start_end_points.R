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

### output directory
output_gpkg <- "data/c_analysis_data/wind.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

#####################################
#####################################

# start point
start_point <- sf::st_read(dsn = data_dir,
                    layer = sf::st_layers(dsn = data_dir)[[1]][grep(pattern = "2_2_corridors_start",
                                                                    x = sf::st_layers(dsn = data_dir)[[1]])]) %>%
  sf::st_zm() %>%
  sf::st_cast("POINT") %>%
  sf::st_transform(x = .,
                   crs = crs)

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
sf::st_write(start_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_point"))
sf::st_write(end_points, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_end_points"))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate