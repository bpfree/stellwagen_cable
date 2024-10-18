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
data_name <- "sediment_gravel"

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

# transform raster data to WGS84
data_points <-  data %>%
  # reproject the coordinate reference system to WGS84
  terra::project(x = .,
                 y = "EPSG:4326")

# create polygon points based off the extent
aoi_points <- rbind(c("point", terra::xmax(data_points), terra::ymin(data_points)), # southeastern point
                    c("point", terra::xmax(data_points), terra::ymax(data_points)), # northeastern point
                    c("point", terra::xmin(data_points), terra::ymax(data_points)), # northwestern point
                    c("point", terra::xmin(data_points), terra::ymin(data_points))) %>% # southwestern point
  # convert to data frame
  as.data.frame() %>%
  # rename column names
  dplyr::rename("point" = "V1",
                "lon" = "V2",
                "lat" = "V3") %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               crs = "EPSG:4326") %>%
  # change projection to NAD83 UTM 19N (https://epsg.io/26919)
  sf::st_transform(x = .,
                   crs = crs) # EPSG 26919 (NAD83 UTM 19N: https://epsg.io/26919)

# create polygon
aoi_poly <- aoi_points %>%
  # group by the points field
  dplyr::group_by(point) %>%
  # combine geometries without resolving borders to create multipoint feature
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  # convert back to sf
  sf::st_as_sf() %>%
  # convert to polygon simple feature
  sf::st_cast("POLYGON") %>%
  # convert back to sf
  sf::st_as_sf()

### get grid cells that overlap with gravel dataset
grid_gravel <- grid[aoi_poly, ] %>%
  # spatially join sediment data extent to Stellwagen grid
  sf::st_join(x = .,
              y = aoi_poly,
              join = st_intersects) %>%
  # select only index field
  dplyr::select(index)

#####################################

# remove data no longer needed
rm(data_points)
rm(aoi_points)
rm(aoi_poly)

#####################################
#####################################

# reclassify values to have only gravel data
data_rcl <- terra::classify(x = data,
                            # reclass the values 1, 2, and 4 as 0
                            rcl = cbind(c(1,2,4), 0),
                            # reclass others (in this case 3) as 1
                            others = 1) %>%
  # reclass data so that any 0s become NA
  terra::classify(x = .,
                  cbind(0, NA))

#####################################

# remove data no longer needed
plot(data_rcl)
rm(data)
rm(grid)

#####################################

# create divisions for making the grid of gravel data more manageable
div <- round(dim(grid_gravel)[1] / 5)
div

# second division
div2_start <- div + 1
div2_end <- div * 2
div2_end - div2_start

# third division
div3_start <- div2_end + 1
div3_end <- div * 3
div3_end - div3_start

# fourth division
div4_start <- div3_end + 1
div4_end <- div * 4
div4_end - div4_start

# fifth division
div5_start <- div4_end + 1
div5_end <- dim(grid_gravel)[1]
div5_end - div5_start

# grids for gravel data based on division splits
gravel_grid1 <- grid_gravel[1:div, ]
gravel_grid2 <- grid_gravel[div2_start:div2_end, ]
gravel_grid3 <- grid_gravel[div3_start:div3_end, ]
gravel_grid4 <- grid_gravel[div4_start:div4_end, ]
gravel_grid5 <- grid_gravel[div5_start:div5_end, ]

#####################################
#####################################

# calculate gravel percentage in grid cell
## first division calculation
gravel_fractions1 <- exactextractr::exact_extract(x = data_rcl,
                                                  y = gravel_grid1,
                                                  function(df) {
                                                    df %>%
                                                      dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
                                                      dplyr::group_by(index, value) %>%
                                                      dplyr::summarize(freq = sum(frac_total))
                                                  },
                                                  summarize_df = TRUE,
                                                  include_cols = "index")

## gravel with 50% threshold
gravel_fractions1_grid <- gravel_fractions1 %>%
  # filter for areas that are gravel (value = 1) and have greater than 50% within the cell
  dplyr::filter(value == 1 & freq >= 0.50)

#####################################

## second division
gravel_fractions2 <- exactextractr::exact_extract(x = data_rcl,
                                                  y = gravel_grid2,
                                                  function(df) {
                                                    df %>%
                                                      dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
                                                      dplyr::group_by(index, value) %>%
                                                      dplyr::summarize(freq = sum(frac_total))
                                                  },
                                                  summarize_df = TRUE,
                                                  include_cols = "index")

## gravel with 50% threshold
gravel_fractions2_grid <- gravel_fractions2 %>%
  # filter for areas that are gravel (value = 1) and have greater than 50% within the cell
  dplyr::filter(value == 1 & freq >= 0.50)

#####################################

## third division
gravel_fractions3 <- exactextractr::exact_extract(x = data_rcl,
                                                  y = gravel_grid3,
                                                  function(df) {
                                                    df %>%
                                                      dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
                                                      dplyr::group_by(index, value) %>%
                                                      dplyr::summarize(freq = sum(frac_total))
                                                  },
                                                  summarize_df = TRUE,
                                                  include_cols = "index")

## gravel with 50% threshold
gravel_fractions3_grid <- gravel_fractions3 %>%
  # filter for areas that are gravel (value = 1) and have greater than 50% within the cell
  dplyr::filter(value == 1 & freq >= 0.50)

#####################################

## fourth division
gravel_fractions4 <- exactextractr::exact_extract(x = data_rcl,
                                                  y = gravel_grid4,
                                                  function(df) {
                                                    df %>%
                                                      dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
                                                      dplyr::group_by(index, value) %>%
                                                      dplyr::summarize(freq = sum(frac_total))
                                                  },
                                                  summarize_df = TRUE,
                                                  include_cols = "index")

## gravel with 50% threshold
gravel_fractions4_grid <- gravel_fractions4 %>%
  # filter for areas that are gravel (value = 1) and have greater than 50% within the cell
  dplyr::filter(value == 1 & freq >= 0.50)

#####################################

## fifth division
gravel_fractions5 <- exactextractr::exact_extract(x = data_rcl,
                                                  y = gravel_grid5,
                                                  function(df) {
                                                    df %>%
                                                      dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
                                                      dplyr::group_by(index, value) %>%
                                                      dplyr::summarize(freq = sum(frac_total))
                                                  },
                                                  summarize_df = TRUE,
                                                  include_cols = "index")

## gravel with 50% threshold
gravel_fractions5_grid <- gravel_fractions5 %>%
  # filter for areas that are gravel (value = 1) and have greater than 50% within the cell
  dplyr::filter(value == 1 & freq >= 0.50)

#####################################

# combine all threshold grids to get full extent
gravel_total_grid <- rbind(gravel_fractions1_grid,
                           gravel_fractions2_grid,
                           gravel_fractions3_grid,
                           gravel_fractions4_grid,
                           gravel_fractions5_grid)

#####################################

## Stellwagen grid
grid <- sf::st_read(dsn = study_region_gpkg,
                    layer = stringr::str_glue("{region_name}_grid")) %>%
  sf::st_transform(x = .,
                   crs = crs)

data_region_grid <- grid %>%
  dplyr::inner_join(x = .,
                    y = gravel_total_grid,
                    by = "index") %>%
  dplyr::mutate(layer = "gravel") %>%
  dplyr::select(index, layer)

#####################################
#####################################

# export data
## costs geopackage
sf::st_write(obj = data_region_grid, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_grid"), append = FALSE)

## intermediate geopackage
sf::st_write(obj = aoi_points, dsn = study_region_gpkg, layer = stringr::str_glue("{region_name}_points"), append = F)
sf::st_write(obj = aoi_poly, dsn = study_region_gpkg, layer = stringr::str_glue("{region_name}_polygon"), append = F)
sf::st_write(obj = grid_gravel, dsn = study_region_gpkg, layer = stringr::str_glue("{region_name}_gravel_grid"), append = F)

## RDS tables
saveRDS(object = gravel_fractions1_grid, file = file.path(rds_dir, stringr::str_glue("{region_name}_{data_name}_grid_threshold1.rds")))
saveRDS(object = gravel_fractions2_grid, file = file.path(rds_dir, stringr::str_glue("{region_name}_{data_name}_grid_threshold2.rds")))
saveRDS(object = gravel_fractions3_grid, file = file.path(rds_dir, stringr::str_glue("{region_name}_{data_name}_grid_threshold3.rds")))
saveRDS(object = gravel_fractions4_grid, file = file.path(rds_dir, stringr::str_glue("{region_name}_{data_name}_grid_threshold4.rds")))
saveRDS(object = gravel_fractions5_grid, file = file.path(rds_dir, stringr::str_glue("{region_name}_{data_name}_grid_threshold5.rds")))

saveRDS(object = gravel_total_grid, file = file.path(rds_dir, stringr::str_glue("{region_name}_{data_name}_grid_threshold.rds")))

## rasters
terra::writeRaster(x = data_rcl, filename = "data/b_intermediate_data/sediment_gravel.grd", overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
