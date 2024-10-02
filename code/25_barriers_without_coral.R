###############################################
### 25. model 3 barriers -- no coral points ###
###############################################

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

# cost_function
cost_function <- function(cost_layer, field_name, cost_value){
  # add cost value
  data <- cost_layer %>%
    # create new field to add cost value
    dplyr::mutate({{field_name}} := cost_value) %>%
    # remove geometry so it is simplified data frame
    sf::st_drop_geometry() %>%
    # select fields of interest
    dplyr::select(index, {{field_name}}) %>%
    # due to different types leading to the same score, need to remove duplicates
    ## group by unique indexes and values
    ### using column position (1 = index, 2 = cost value for type)
    dplyr::group_by_at(1:2) %>%
    ## summarise to remove duplicates
    dplyr::summarise()
}

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
costs_dir <- "data/b_intermediate_data/costs.gpkg"
grid_dir <- "data/a_raw_data/stellwagen.gpkg"
barriers_dir <- "data/b_intermediate_data/barriers.gpkg"
raster_dir <- "data/d_raster_data"

### output directory
barriers_gpkg <- "data/c_analysis_data/barriers.gpkg"
model_gpkg <- "data/c_analysis_data/model3.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = T)

#####################################
#####################################

# load raster grid
raster <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_study_area_{cell_size}m.grd")))

#####################################

# load vector data
## barriers
sites_avoid <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_sites_to_avoid_grid"))
boulder_ridges <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_boulder_ridges_grid"))
cape_cod <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_cape_cod_limit_grid"))

#####################################
#####################################

# create barriers layers
barriers_without_coral <- sites_avoid %>%
  # combine barriers datasets so that each is an unique row
  rbind(boulder_ridges,
        cape_cod) %>%
  # create field called "barrier" and fill with "barrier" for summary; "value" populate with 0
  dplyr::mutate(barrier = "barrier",
                value = 0) %>%
  # group all features by the "barrier" and "value" fields to then have a single feature
  dplyr::group_by(barrier,
                  value) %>%
  # summarise data to obtain single feature that will act as a barrier
  dplyr::summarise()

#####################################
#####################################

# convert to rasters
## barriers
sites_avoid_raster <- terra::rasterize(x = sites_avoid,
                                       y = raster,
                                       field = "value")

boulder_ridges_raster <- terra::rasterize(x = boulder_ridges,
                                          y = raster,
                                          field = "value")

cape_cod_raster <- terra::rasterize(x = cape_cod,
                                    y = raster,
                                    field = "value")

#####################################
#####################################

# create barriers raster
## cover any NA values of another raster with values from any other raster (all barrier cells)
barriers_without_coral_raster <- terra::cover(x = sites_avoid_raster,
                                              y = boulder_ridges_raster) %>%
  # cover with Cape Cod limit raster
  terra::cover(y = cape_cod_raster)

## set values of 0 to be 99 for later cost analysis
barriers_without_coral_raster[barriers_without_coral_raster == 0] <- 99

#####################################
#####################################

# export data
## least cost geopackage
sf::st_write(obj = barriers_without_coral, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_barriers", append = F))

## raster data
terra::writeRaster(barriers_without_coral_raster, filename = file.path(raster_dir, stringr::str_glue("{region_name}_barriers_without_coral_{cell_size}m.grd")), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate