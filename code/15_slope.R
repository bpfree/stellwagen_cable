#################
### 15. slope ###
#################

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
data_name <- "slope"

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
#### slope
data_dir <- "data/a_raw_data/stellwagen_slope/nbs_slope"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/a_raw_data/{region_name}.gpkg")

### output directories
#### costs geopackage
output_gpkg <- "data/b_intermediate_data/costs.gpkg"

#####################################

# inspect layers within directory
sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# Create normalization functions
## Linear function
linear_function <- function(raster, crs, region){
  
  # project the raster to match the correct coordinate reference system
  raster_proj <- raster %>%
    # project the raster
    terra::project(x = .,
                   # use the parameterized CRS ("EPSG:26919")
                   y = crs)
  
  # crop raster to study region
  raster_crop <- raster_proj %>%
    # crop data to study region
    terra::crop(x = .,
                # study region acts as crop boundary
                y = region)
  
  # mask raster to only the study region
  raster_mask <- raster_crop %>%
    # mask data
    terra::mask(x = .,
                # study region acts as mask area
                mask = region)
  
  # calculate minimum value
  min <- terra::minmax(raster_mask)[1,]
  
  # recalculate maximum value
  max <- terra::minmax(raster_mask)[2,]
  
  # create linear function
  normalize <- (raster_mask[] - min) / (max - min)
  
  # set values back to the newly projected raster
  raster_normalize <- terra::setValues(raster_mask, normalize)
  
  # return the raster
  return(raster_normalize)
}

#####################################
#####################################

# read data
## Stellwagen grid
grid <- sf::st_read(dsn = study_region_gpkg,
                    layer = stringr::str_glue("{region_name}_grid")) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen study area
region <- sf::st_read(dsn = study_region_gpkg,
                      layer = stringr::str_glue("{region_name}_polygon"))

## slope data
data <-  terra::rast(file.path(data_dir, "w001000.adf"))

### inspect the data
levels(data)
is.factor(data)
plot(data)

#####################################
#####################################

# normalize slope data in study region
data_normalized <- data %>%
  linear_function(raster = ., crs = crs, region = region)

res(data_normalized) # 8 x 8
hist(data_normalized) # show histogram of values (though mostly values near 1)
freq(data_normalized) # show frequency of values (though will round to 0 and 1)
ncol(data_normalized)
nrow(data_normalized)
ncell(data_normalized)

#####################################
#####################################

#####################################
#####################################

# calculate the maximum reclassified slope value
data_region_grid <- grid %>%
  # create a "slope_max" field and populate it with the maximum reclassified slope value
  dplyr::mutate(slope_max = exactextractr::exact_extract(x = data_normalized[[1]],
                                                         # for each cell
                                                         y = grid,
                                                         # calculate the maximum reclassified slope
                                                         fun = 'max')) %>%
  # keep only index cells with values
  dplyr::filter(!is.na(slope_max)) %>%
  # move "slope_max" so it is after the "index" field
  dplyr::relocate(slope_max, .after = index)

min(data_region_grid$slope_max, na.rm = T) # inspect minimum value
max(data_region_grid$slope_max, na.rm = T) # inspect maximum value
hist(data_region_grid$slope_max, na.rm = T) # inspect histogram

#####################################
#####################################

# export data
## costs geopackage
sf::st_write(obj = data_region_grid, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_grid"), append = FALSE)

## intermediate geopackage


#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
