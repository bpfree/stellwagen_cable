##################################
### 14. NMS sediment -- gravel ###
##################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region_name <- "stellwagen"

## layer names
data_name <- "nms_sediment"

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
               exactextractr,
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
#### sediment
data_dir <- "data/a_raw_data/sediment_grid/sediment"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/a_raw_data/{region_name}.gpkg")

#### National Marine Sanctuary
stellwagen_dir <- "data/a_raw_data/sbnms_py2"

### output directories
#### RDS directory
dir.create(file.path("data/b_intermediate_data", "gravel_tables"))
rds_dir <- "data/b_intermediate_data/gravel_tables"

#### costs geopackage
output_gpkg <- "data/b_intermediate_data/costs.gpkg"

#####################################

# inspect layers within directory
sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################


# read data
## Stellwagen grid
grid <- sf::st_read(dsn = study_region_gpkg,
                    layer = stringr::str_glue("{region_name}_grid")) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen boundary
stellwagen <- sf::st_read(dsn = stellwagen_dir) %>%
  sf::st_transform(x = .,
                   crs = crs)

### get grid cells that overlap with gravel dataset
grid_stellwagen <- grid[stellwagen, ] %>%
  # spatially join sediment data extent to Stellwagen grid
  sf::st_join(x = .,
              y = stellwagen,
              join = st_intersects) %>%
  # select only index field
  dplyr::select(index)

## sediment
data <-  terra::rast(file.path(data_dir, "w001001.adf")) %>%
  # match the coordinate reference system
  terra::project(x = .,
                 y = crs)

### inspect the data
levels(data)
is.factor(data)
plot(data)

#####################################
#####################################

# limit sediment data to Stellwagen Banks National Marine Sanctuary
data_region <- data %>%
  terra::crop(x = .,
              y = stellwagen,
              mask = T)

plot(data_region)

# calculate the sediment that is the majority of grid cell and populate with that sediment value
data_region_grid <- exactextractr::exact_extract(x = data_region,
                                            # grid for Stellwagen Bank National Marine Sanctuary
                                            y = grid_stellwagen,
                                            # populate with the sediment value that covers the majority of the grid
                                            fun = 'majority') %>%
  # convert to a dataframe
  as.data.frame() %>%
  # rename the field to be sediment
  dplyr::rename("sediment" = ".") %>%
  # combine sediment data with the grid for Stellwagen Bank National Marine Sanctuary
  cbind(.,
        grid_stellwagen) %>%
  # convert back to sf
  sf::st_as_sf() %>%
  # reorder the fields so that it is index then sediment
  dplyr::select(index, sediment)

g <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data = data_region_grid, aes(fill = sediment))
g

#####################################
#####################################

# export data
## costs geopackage
sf::st_write(obj = data_region_grid, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_grid"), append = FALSE)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
