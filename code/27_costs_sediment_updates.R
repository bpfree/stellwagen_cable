############################################
### 27. costs raster -- sediment updates ###
############################################

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
## vector grid
grid <- sf::st_read(dsn = grid_dir, layer = stringr::str_glue("{region_name}_grid"))

## raster grid
raster <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_study_area_{cell_size}m.grd")))

## barriers grid
barriers <- terra::rast(file.path(raster_dir, stringr::str_glue("{region_name}_barriers_without_coral_boulder_{cell_size}m.grd")))

#####################################

# load data
## costs layers
### CONMAPSG
conmapsg_sand <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_sand_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_sand_value", cost_value = 0.1)

conmapsg_mix <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_mix_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_mix_value", cost_value = 0.1)

conmapsg_gravel <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_gravel_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_gravel_value", cost_value = 0.1)

conmapsg_rock <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_conmapsg_rock_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "conmapsg_rock_value", cost_value = 0.6)

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

conmapsg_grid <- grid %>%
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
  ## this will create the most conservative (costly) values
  dplyr::mutate(cost = pmax(conmapsg_sand_value,
                            conmapsg_mix_value,
                            conmapsg_gravel_value,
                            conmapsg_rock_value,
                            # remove any values that are NA when new field
                            na.rm = T)) %>%
  # move the new field after all the other values
  dplyr::relocate(cost, .after = conmapsg_rock_value) %>%
  # give a value of 0 to the cost field in a cell if value is NA
  dplyr::mutate(across(6, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "cost")

#####################################
#####################################

### disposal sites
disposal_sites <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_disposal_sites_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "disposal_sites_value", cost_value = 0.8)

disposal_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = disposal_sites,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "disposal_sites_value",
                   x = .)

#####################################

### intertidal flats
intertidal_flats <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_intertidal_flats_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "intertidal_flats_value", cost_value = 0.2)

intertidal_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = intertidal_flats,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "intertidal_flats_value",
                   x = .)

#####################################

### sand patches
sand_patches <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_sand_patches_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "sand_patches_value", cost_value = 1.0)

patches_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = sand_patches,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  
  # rasterize data
  terra::rasterize(y = raster,
                   field = "sand_patches_value",
                   x = .)

#####################################

### channel areas
channel_areas <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_channel_areas_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "channel_areas_value", cost_value = 1.0)

channel_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = channel_areas,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "channel_areas_value",
                   x = .)

#####################################

### anchorage areas
anchorage_areas <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_anchorage_areas_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "anchorage_areas_value", cost_value = 0.8)

anchorage_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = anchorage_areas,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "anchorage_areas_value",
                   x = .)

#####################################

### eelgrass meadows
eelgrass_meadows <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_eelgrass_meadow_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "eelgrass_meadows_value", cost_value = 0.4)

eelgrass_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = eelgrass_meadows,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "eelgrass_meadows_value",
                   x = .)

#####################################

### cable and pipeline areas
cable_pipelines <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_cable_pipeline_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "cable_pipelines_value", cost_value = 0.4)

cable_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = cable_pipelines,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "cable_pipelines_value",
                   x = .)

#####################################

### submarine cables
submarine_cables <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_submarine_cable_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "submarine_cables_value", cost_value = 0.4)

submarine_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = submarine_cables,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "submarine_cables_value",
                   x = .)

#####################################

### LNG pipelines
lng_pipelines <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_lng_pipeline_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "lng_pipelines_value", cost_value = 0.4)

lng_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = lng_pipelines,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "lng_pipelines_value",
                   x = .)

#####################################

### sediment mud
mud <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_nms_sediment_grid")) %>%
  # filter for sediment type
  dplyr::filter(sediment == 1) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "mud_value", cost_value = 0.4)

mud_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = mud,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "mud_value",
                   x = .)

#####################################

### sediment sand
sand <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_nms_sediment_grid")) %>%
  # filter for sediment type
  dplyr::filter(sediment == 2) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "sand_value", cost_value = 0.6)

sand_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = sand,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "sand_value",
                   x = .)

#####################################

### sediment gravel
gravel <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_nms_sediment_grid")) %>%
  # filter for sediment type
  dplyr::filter(sediment == 3) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "gravel_value", cost_value = 0.5)

gravel_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = gravel,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "gravel_value",
                   x = .)

#####################################

### sediment boulder
boulder <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_nms_sediment_grid")) %>%
  # filter for sediment type
  dplyr::filter(sediment == 4) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "boulder_value", cost_value = 0.6)

boulder_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = boulder,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "boulder_value",
                   x = .)

#####################################

### sand lance
sand_lance <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_sand_lance_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "sand_lance_value", cost_value = 0.9)

lance_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = sand_lance,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "sand_lance_value",
                   x = .)

#####################################

### boulder ridge
boulder_ridge <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_boulder_ridges_grid")) %>%
  # add cost value and remove geometry
  cost_function(cost_layer = ., field_name = "boulder_ridge_value", cost_value = 0.6)

ridge_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = boulder_ridge,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "boulder_ridge_value",
                   x = .)

#####################################

### slope
slope <- sf::st_read(dsn = data_dir, layer = stringr::str_glue("{region_name}_slope_grid")) %>%
  # remove geometry so it is simplified data frame
  sf::st_drop_geometry()

slope_grid <- grid %>%
  dplyr::left_join(x = .,
                   y = slope,
                   by = "index") %>%
  dplyr::mutate(across(2, ~replace(x = .,
                                   list = is.na(.),
                                   # replacement values
                                   values = 0))) %>%
  # rasterize data
  terra::rasterize(y = raster,
                   field = "slope_max",
                   x = .)

#####################################
#####################################

# create costs layer
## cover any NA values of another raster with values from any other raster (all barrier cells)
cost_raster <- c(conmapsg_grid,
                 disposal_grid,
                 intertidal_grid,
                 patches_grid,
                 channel_grid,
                 anchorage_grid,
                 eelgrass_grid,
                 cable_grid,
                 submarine_grid,
                 lng_grid,
                 mud_grid,
                 sand_grid,
                 gravel_grid,
                 boulder_grid,
                 lance_grid,
                 ridge_grid,
                 slope_grid) %>%
  terra::app(sum, na.rm = T) %>%
  # remove land from cost layer
  terra::crop(raster,
              mask = TRUE)

## cost raster without barriers
cost_rm_barriers <- c(cost_raster,
                      barriers) %>% 
  # sum the two layers while removing any NA values
  terra::app(sum, na.rm = T) %>%
  # add 0.01 so there are no 0 values
  sum(., 0.01)
plot(cost_rm_barriers)

# make any values above 99 (where a constraint would be) to be set as NA to remove from analysis
cost_rm_barriers[cost_rm_barriers >= 99] <- NA
cost_rm_barriers[cost_rm_barriers == 0.01] <- NA
plot(cost_rm_barriers)

#####################################
#####################################

# export data
## least cost geopackage
sf::st_write(obj = conmapsg_sand, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_conmapsg_rock_cost", append = F))
sf::st_write(obj = conmapsg_mix, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_conmapsg_mix_cost", append = F))
sf::st_write(obj = conmapsg_gravel, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_conmapsg_gravel_cost", append = F))
sf::st_write(obj = conmapsg_sand, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_conmapsg_sand_cost", append = F))

sf::st_write(obj = disposal_sites, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_disposal_sites_cost", append = F))
sf::st_write(obj = intertidal_flats, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_intertidal_flats_cost", append = F))
sf::st_write(obj = sand_patches, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_sand_patches_cost", append = F))
sf::st_write(obj = channel_areas, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_channel_areas_cost", append = F))
sf::st_write(obj = anchorage_areas, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_anchorage_areas_cost", append = F))
sf::st_write(obj = eelgrass_meadows, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_eelgrass_meadows_cost", append = F))
sf::st_write(obj = cable_pipelines, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_cable_pipelines_cost", append = F))
sf::st_write(obj = submarine_cables, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_submarine_cables_cost", append = F))
sf::st_write(obj = lng_pipelines, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_lng_pipelines_cost", append = F))
sf::st_write(obj = mud, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_nms_mud_cost", append = F))
sf::st_write(obj = sand, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_nms_sand_cost", append = F))
sf::st_write(obj = gravel, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_nms_gravel_cost", append = F))
sf::st_write(obj = boulder, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_nms_boulder_cost", append = F))

sf::st_write(obj = sand_lance, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_sand_lance_cost", append = F))
sf::st_write(obj = boulder_ridge, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_boulder_ridge_cost", append = F))

## raster data
terra::writeRaster(cost_rm_barriers, filename = file.path(raster_dir, stringr::str_glue("{region_name}_sediment_update_costs_rm_barriers_without_coral_{cell_size}m.grd")), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
