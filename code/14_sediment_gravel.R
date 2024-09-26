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
#### liquid natural gas pipelines
data_dir <- "data/a_raw_data/sediment_grid/sediment"

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

levels(data)
is.factor(data)
plot(data)

data_points <-  data %>%
  # match the coordinate reference system
  terra::project(x = .,
                 y = "EPSG:4326")

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

# Create polygon
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
  # spatially join sediment data to Stellwagen grid
  sf::st_join(x = .,
              y = aoi_poly,
              join = st_intersects) %>%
  dplyr::select(index)

rm(data_points)
rm(aoi_points)
rm(aoi_poly)

#####################################
#####################################

# reclassify values to have only gravel data
data_rcl <- terra::classify(x = data,
                  rcl = cbind(c(1,2,4), 0),
                  others = 1) %>%
  terra::classify(x = .,
                  cbind(0, NA))

plot(data_rcl)
rm(data)
rm(grid)

gravel_fractions <- exactextractr::exact_extract(x = data_rcl,
                                                 y = grid_gravel,
                                                 function(df) {
                                                   df %>%
                                                     dplyr::mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
                                                     dplyr::group_by(index, value) %>%
                                                     dplyr::summarize(freq = sum(frac_total))
                                                 },
                                                 summarize_df = TRUE,
                                                 include_cols = "index")

stellwagen_gravel_area <- gravel_fractions %>%
  dplyr::inner_join(x = .,
                    y = grid,
                    by = "index") %>%
  dplyr::filter(value = 1,
                freq >= 0.5)



#####################################
#####################################

# convert raster to vector data (as polygons)
# convert to polygon
westport_vms_polygon <- terra::as.polygons(x = fishery_z_scale,
                                           # do not aggregate all similar values together as single feature
                                           aggregate = F,
                                           # use the values from original raster
                                           values = T) %>%
  # change to simple feature (sf)
  sf::st_as_sf() %>%
  # simplify column name to "vms" (this is the first column of the object, thus the colnames(.)[1] means take the first column name from the vms object)
  dplyr::rename(vms = colnames(.)[1]) %>%
  # add field "layer" and populate with "vms"
  dplyr::mutate(layer = "vms") %>%
  # limit to the study region
  rmapshaper::ms_clip(clip = westport_region) %>%
  # reproject data into a coordinate system (NAD 1983 UTM Zone 18N) that will convert units from degrees to meters
  sf::st_transform(crs = crs)

#####################################
#####################################

# export data
## costs geopackage
sf::st_write(obj = data_region_grid, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_{data_name}_grid"), append = FALSE)

## intermediate geopackage
sf::st_write(obj = aoi_points, dsn = study_region_gpkg, layer = stringr::str_glue("{region_name}_points"), append = F)
sf::st_write(obj = aoi_poly, dsn = study_region_gpkg, layer = stringr::str_glue("{region_name}_polygon"), append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
