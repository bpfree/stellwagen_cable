####################
### 04. CONMAPSG ###
####################

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
data_name <- "conmapsg"

## CONMAPSG types
sand_types <- c("M", "Ms", "S", "Sm")
mix_types <- c("Mg", "Mr", "Sg", "Sr")
gravel_types <- c("G", "G*", "Gm", "Gr", "Gs", "Gs*", "Sg or Gs")
rock_types <- c("G or R", "R", "R**", "Rf", "Rg", "Rm", "Rs")

sand <- "sand"
mix <- "mix"
gravel <- "gravel"
rock <- "rock"

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
#### CONMAPSG (data shared by Brooke Hodge -- brooke.hodge@mass.gov)
##### report: https://www.mass.gov/doc/sediment-and-geology-work-group-report/download
data_dir <- "data/a_raw_data/Final_merged_data"

#### National Marine Sanctuary
stellwagen_dir <- "data/a_raw_data/sbnms_py2"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/a_raw_data/{region_name}.gpkg")

### output directories
#### costs geopackage
output_gpkg <- stringr::str_glue("data/b_intermediate_data/costs.gpkg")

#####################################

# inspect layers within geodatabases and geopackages
sf::st_layers(dsn = data_dir,
              do_count = T)

sf::st_layers(dsn = study_region_gpkg,
              do_count = T)

#####################################
#####################################

# read data
## CONMAPSG
data <- sf::st_read(dsn = data_dir) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen region
region <- sf::st_read(dsn = study_region_gpkg,
                      layer = stringr::str_glue("{region_name}_region")) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen grid
grid <- sf::st_read(dsn = study_region_gpkg,
                    layer = stringr::str_glue("{region}_grid")) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen boundary
stellwagen <- sf::st_read(dsn = stellwagen_dir) %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################
#####################################

# limit data to study region
data_region <- data %>%
  rmapshaper::ms_clip(target = .,
                      clip = region) %>%
  rmapshaper::ms_erase(target = .,
                      erase = stellwagen) %>%
  dplyr::mutate(layer = stringr::str_glue("{data_name}"))

#####################################

# filter to specific types

## sand
data_region_sand <- data_region %>%
  # filter for matching types
  dplyr::filter(BARNHARDT %in% sand_types) %>%
  # create new field that is filled with new type
  dplyr::mutate(type = dplyr::case_when(BARNHARDT %in% sand_types ~ sand)) %>%
  # select layers of interest
  dplyr::select(BARNHARDT, layer, type)

## mix
data_region_mix <- data_region %>%
  # filter for matching types
  dplyr::filter(BARNHARDT %in% mix_types) %>%
  # create new field that is filled with new type
  dplyr::mutate(type = dplyr::case_when(BARNHARDT %in% mix_types ~ mix)) %>%
  # select layers of interest
  dplyr::select(BARNHARDT, layer, type)

## gravel
data_region_gravel <- data_region %>%
  # filter for matching types
  dplyr::filter(BARNHARDT %in% gravel_types) %>%
  # create new field that is filled with new type
  dplyr::mutate(type = dplyr::case_when(BARNHARDT %in% gravel_types ~ gravel)) %>%
  # select layers of interest
  dplyr::select(BARNHARDT, layer, type)

## rock
data_region_rock <- data_region %>%
  dplyr::filter(BARNHARDT %in% rock_types) %>%
  dplyr::mutate(type = dplyr::case_when(BARNHARDT %in% rock_types ~ rock)) %>%
  dplyr::select(BARNHARDT, layer, type)

#####################################
#####################################

# CONMAPSG grid
## sand
data_region_grid_sand <- grid[data_region_sand, ] %>%
  # spatially join CONMAPSG to Stellwagen grid
  sf::st_join(x = .,
              y = data_region_sand,
              join = st_intersects) %>%
  # due to overlapping areas there are a few duplicated indexes
  ## group by unique indexes
  dplyr::group_by(index, BARNHARDT, layer, type) %>%
  ## summarise to remove duplicates
  dplyr::summarise()

## mix
data_region_grid_mix <- grid[data_region_mix, ] %>%
  # spatially join CONMAPSG to Stellwagen grid
  sf::st_join(x = .,
              y = data_region_mix,
              join = st_intersects) %>%
  # due to overlapping areas there are a few duplicated indexes
  ## group by unique indexes
  dplyr::group_by(index, BARNHARDT, layer, type) %>%
  ## summarise to remove duplicates
  dplyr::summarise()

## gravel
data_region_grid_gravel <- grid[data_region_gravel, ] %>%
  # spatially join CONMAPSG to Stellwagen grid
  sf::st_join(x = .,
              y = data_region_gravel,
              join = st_intersects) %>%
  # due to overlapping areas there are a few duplicated indexes
  ## group by unique indexes
  dplyr::group_by(index, BARNHARDT, layer, type) %>%
  ## summarise to remove duplicates
  dplyr::summarise()

## rock
data_region_grid_rock <- grid[data_region_rock, ] %>%
  # spatially join CONMAPSG to Stellwagen grid
  sf::st_join(x = .,
              y = data_region_rock,
              join = st_intersects) %>%
  # due to overlapping areas there are a few duplicated indexes
  ## group by unique indexes
  dplyr::group_by(index, BARNHARDT, layer, type) %>%
  ## summarise to remove duplicates
  dplyr::summarise()

#####################################

# check for duplicates in the data that would impact cost layer
duplicates <- data_region_grid_rock %>%
  # create frequency field based on index
  dplyr::add_count(index) %>%
  # see which ones are duplicates
  dplyr::filter(n>1) %>%
  # show distinct options
  dplyr::distinct()

#####################################
#####################################

# export data
## costs geopackage
sf::st_write(obj = data_region_grid_sand, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_{sand}_grid"), append = FALSE)
sf::st_write(obj = data_region_grid_mix, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_{mix}_grid"), append = FALSE)
sf::st_write(obj = data_region_grid_gravel, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_{gravel}_grid"), append = FALSE)
sf::st_write(obj = data_region_grid_rock, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_{rock}_grid"), append = FALSE)

## intermediate geopackage


#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
