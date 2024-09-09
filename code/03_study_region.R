########################
### 03. study region ###
########################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

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

# Commentary on R and code formulation:
## ***Note: If not familiar with dplyr notation
## dplyr is within the tidyverse and can use %>%
## to "pipe" a process, allowing for fluidity
## Can learn more here: https://style.tidyverse.org/pipes.html

## another common coding notation used is "::"
## For instance, you may encounter it as dplyr::filter()
## This means "use the filter function from the dplyr package"
## Notation is used given sometimes different packages have
## the same function name, so it helps code to tell which
## package to use for that particular function.
## The notation is continued even when a function name is
## unique to a particular package so it is obvious which
## package is used

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/03_CableRouteModel/model_2/model_2.gdb"

## export directory
export_dir <- "data/a_raw_data/stellwagen.gpkg"
raster_dir <- "data/d_raster_data"

#####################################

# inspect layers within geodatabases and geopackages
sf::st_layers(dsn = data_dir,
              do_count = T)

#####################################
#####################################

# create simplified grid
grid <- sf::st_read(dsn = data_dir, layer = "SW_NMS_AOI_model2_Selected") %>%
  # create an index based on row number
  dplyr::mutate(index = row_number()) %>%
  # simplify to the index
  dplyr::select(index)

# create a dissolved grid
blank_grid <- grid %>%
  # create new field to designate region
  dplyr::mutate(region = "stellwagen",
                value = 0) %>%
  # group by region
  dplyr::group_by(region,
                  value) %>%
  # summarise by region to dissolve to one polygon
  dplyr::summarise()

#####################################
#####################################

# create grid
### grid with 100 meter cell size
#### create a template raster that has the extent of the study area
rast_temp <- terra::rast(blank_grid,
                         # use the extent of the marine study area
                         extent = blank_grid,
                         # give raster to have resolution of 100 meters
                         resolution = 100,
                         # have coordinate reference system as the study area (NAD83 UTM 19N: https://epsg.io/26919)
                         crs = crs(blank_grid))

#### Create raster filed with the data from the study area
rast_100m <- terra::rasterize(x = blank_grid,
                              y = rast_temp,
                              field = "value")

#####################################
#####################################

# export data
## vector grids
sf::st_write(obj = grid, dsn = export_dir, layer = "stellwagen_grid", append = F)
sf::st_write(obj = blank_grid, dsn = export_dir, layer = "stellwagen_region", append = F)

## raster grid
terra::writeRaster(rast_100m, filename = file.path(raster_dir, "gom_study_area_marine_100m_raster.grd"), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
