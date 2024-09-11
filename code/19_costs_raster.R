########################
### 19. costs raster ###
########################

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
data_dir <- "data/b_intermediate_data/costs.gpkg"
grid_dir <- "data/a_raw_data/stellwagen.gpkg"
raster_dir <- "data/d_raster_data"

### output directory
output_gpkg <- "data/c_analysis_data/costs.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

sf::st_layers(dsn = grid_dir,
              do_count = TRUE)

#####################################
#####################################

# load data
grid <- sf::st_read(dsn = grid_dir, layer = stringr::str_glue("{region_name}_grid"))

## constraints grid
### all constraints
constraints <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_barriers_{cell_size}m.grd")))

#####################################

# load data
## costs layers
### CONMAPSG
conmapsg_sand <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_sand_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_sand_value", cost_value = 0.2)

conmapsg_mix <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_mix_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_mix_value", cost_value = 0.4)

conmapsg_gravel <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_gravel_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_gravel_value", cost_value = 0.6)

conmapsg_rock <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_rock_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_rock_value", cost_value = 1.0)

#####################################

# check for duplicates in the data that would impact cost layer
duplicates <- conmapsg_rock %>%
  # create frequency field based on index
  dplyr::add_count(index) %>%
  # see which ones are duplicates
  dplyr::filter(n>1) %>%
  # show distinct options
  dplyr::distinct()

#####################################
#####################################

conmapsg <- grid %>%
  dplyr::left_join(x = .,
                   y = conmapsg_sand,
                   by = "index") %>%
  dplyr::left_join(x = .,
                   y = conmapsg_mix,
                   by = "index") %>%
  dplyr::left_join(x = .,
                   y = conmapsg_gravel,
                   by = "index") %>%
  dplyr::left_join(x = .,
                   y = conmapsg_rock,
                   by = "index") %>%
  # create final cost that takes maximum of all the values
  dplyr::mutate(cost = pmax(conmapsg_sand_value,
                            conmapsg_mix_value,
                            conmapsg_gravel_value,
                            conmapsg_rock_value,
                            # remove any values that are NA when new field
                            na.rm = T)) %>%
  dplyr::relocate(cost, .after = conmapsg_rock_value) %>%
  # give a value of 1 to the cost field in a cell if value is NA
  dplyr::mutate(across(6, ~replace(x = .,
                                     list = is.na(.),
                                     # replacement values
                                     values = 1)))

#####################################
#####################################

### disposal sites
disposal_sites <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_disposal_sites_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "disposal_sites_value", cost_value = 0.8)

### intertidal flats
disposal_sites <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_intertidal_flats_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "intertidal_flats_value", cost_value = 0.2)

### sand patches
sand_patches <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_sand_patches_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "sand_patches_value", cost_value = 1.0)

### channel areas
channel_areas <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_channel_areas_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "channel_areas_value", cost_value = 1.0)

### anchorage areas
anchorage_areas <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_anchorage_areas_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "anchorage_areas_value", cost_value = 0.8)

### eelgrass meadows
eelgrass_meadows <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_eelgrass_meadows_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "eelgrass_meadows_value", cost_value = 0.4)

### cable and pipeline areas
eelgrass_meadows <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_eelgrass_meadows_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "eelgrass_meadows_value", cost_value = 0.4)

### submarine cables
eelgrass_meadows <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_eelgrass_meadows_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "eelgrass_meadows_value", cost_value = 0.4)

### LNG pipelines
eelgrass_meadows <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_eelgrass_meadows_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "eelgrass_meadows_value", cost_value = 0.4)