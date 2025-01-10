###########################
### 33. Top route paths ###
###########################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region_name <- "stellwagen"

## top routes (by overall least cost)
top_n <- 25

## coordinate reference system
### set the coordinate reference system that data should become (NAD83 UTM 19N: https://epsg.io/26919)
crs <- "EPSG:26919"

## designate date
date <- format(Sys.Date(), "%Y%m%d")

#####################################
#####################################

# load packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(renv,
#                dplyr,
#                ggplot2,
#                janitor,
#                plyr,
#                purrr,
#                rmapshaper,
#                sf,
#                sp,
#                stringr,
#                targets,
#                terra, # is replacing the raster package
#                tidyr)

librarian::shelf(renv,
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
#### Stellwagen routes
routes_dir <- "data/e_least_cost_path_data/stellwagen_lcp_options.gpkg"

#### lines data
lines_dir <- "data/c_analysis_data/stellwagen_lines.gpkg"

### output directory
output_gpkg <- "data/e_least_cost_path_data/stellwagen_route_options.gpkg"

#####################################

# inspect layers within directories
sf::st_layers(dsn = routes_dir,
              do_count = T)

sf::st_layers(dsn = lines_dir,
              do_count = T)

#####################################
#####################################

# load data
## 0564 to east
route_0564_east <- sf::st_read(dsn = routes_dir,
                               layer = sf::st_layers(dsn = routes_dir)[[1]][[grep(pattern = "0564_east$",
                                                                                  x = sf::st_layers(dsn = routes_dir)[[1]])]]) %>%
  # arrange ascending by lowest path cost
  dplyr::arrange(PathCost) %>%
  # only return top ("least costly") paths
  dplyr::slice_head(n = top_n)

## 0567 to east
route_0567_east <- sf::st_read(dsn = routes_dir,
                                layer = sf::st_layers(dsn = routes_dir)[[1]][[grep(pattern = "0567_east$",
                                                                                     x = sf::st_layers(dsn = routes_dir)[[1]])]]) %>%
  # arrange ascending by lowest path cost
  dplyr::arrange(PathCost) %>%
  # only return top ("least costly") paths
  dplyr::slice_head(n = top_n)

## Boston to west
boston_west <- sf::st_read(dsn = routes_dir,
                                  layer = sf::st_layers(dsn = routes_dir)[[1]][[grep(pattern = "boston_west$",
                                                                                       x = sf::st_layers(dsn = routes_dir)[[1]])]]) %>%
  # arrange ascending by lowest path cost
  dplyr::arrange(PathCost) %>%
  # only return top ("least costly") paths
  dplyr::slice_head(n = top_n)

## Plymouth to west
plymouth_west <- sf::st_read(dsn = routes_dir,
                                  layer = sf::st_layers(dsn = routes_dir)[[1]][[grep(pattern = "plymouth_west$",
                                                                                     x = sf::st_layers(dsn = routes_dir)[[1]])]]) %>%
  # arrange ascending by lowest path cost
  dplyr::arrange(PathCost) %>%
  # only return top ("least costly") paths
  dplyr::slice_head(n = top_n)

#####################################

## lines
stellwagen_lines <- sf::st_read(dsn = lines_dir,
                                layer = sf::st_layers(dsn = lines_dir)[[1]][[grep(pattern = "costs",
                                                                                   x = sf::st_layers(dsn = lines_dir)[[1]])]])

#####################################
#####################################

## east points
points_0564_east <- c(route_0564_east$DestID)
points_0567_east <- c(route_0567_east$DestID)

points_boston_west <- c(boston_west$DestID) + 77
points_plymouth_west <- c(plymouth_west$DestID) + 77

#####################################

route_0564_boston <- stellwagen_lines %>%
  dplyr::filter(index_start %in% points_0564_east,
                index_end %in% points_boston_west)

route_0564_plymouth <- stellwagen_lines %>%
  dplyr::filter(index_start %in% points_0564_east,
                index_end %in% points_plymouth_west)

route_0567_boston <- stellwagen_lines %>%
  dplyr::filter(index_start %in% points_0567_east,
                index_end %in% points_boston_west)

route_0567_plymouth <- stellwagen_lines %>%
  dplyr::filter(index_start %in% points_0567_east,
                index_end %in% points_plymouth_west)

#####################################
#####################################

plot(route_0564_boston$geom)
plot(route_0564_plymouth$geom)
plot(route_0567_boston$geom)
plot(route_0567_plymouth$geom)

#####################################
#####################################

# export data
## routes through sanctuary
sf::st_write(obj = route_0564_boston, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_0564_boston", sep = "_"), append = F)
sf::st_write(obj = route_0567_boston, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_0567_boston", sep = "_"), append = F)
sf::st_write(obj = route_0564_plymouth, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_0564_plymouth", sep = "_"), append = F)
sf::st_write(obj = route_0567_plymouth, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_0567_plymouth", sep = "_"), append = F)

## routes to sanctuary
sf::st_write(obj = route_0564_east, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_0564_east", sep = "_"), append = F)
sf::st_write(obj = route_0567_east, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_0567_east", sep = "_"), append = F)
sf::st_write(obj = boston_west, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_boston_west", sep = "_"), append = F)
sf::st_write(obj = plymouth_west, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_top{top_n}_routes_plymouth_west", sep = "_"), append = F)
