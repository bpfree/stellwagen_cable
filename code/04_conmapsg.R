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

## coordinate reference system
### set the coordinate reference system that data should become (NAD83(2011) / Massachusetts Mainland: https://epsg.io/6492)
#### ***note: units are in feet (not meters)
crs <- "EPSG:6492"

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
#### CONMAPSG
data_dir <- "data/a_raw_data/state_costs.gpkg"

#### study area grid
study_region_gpkg <- stringr::str_glue("data/a_raw_data/{region}.gpkg")

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
data <- sf::st_read(dsn = data_dir,
                    layer = stringr::str_glue("{data_name}")) %>%
  sf::st_transform(x = .,
                   crs = crs)

## Stellwagen region
region <- sf::st_read(dsn = study_region_gpkg,
                      layer = stringr::str_glue("{region}_region")) %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################
#####################################

# limit data to study region
data_region <- data %>%
  rmapshaper::ms_clip(target = .,
                      clip = region)

data_region <- data_region %>%
  dplyr::mutate(type = dplyr::case_when(BARNHARDT == "M" ~ "sand_mud",
                                        BARNHARDT == "Ms" ~ "sand_mud",
                                        BARNHARDT == "S" ~ "sand_mud",
                                        BARNHARDT == "Sm" ~ "sand_mud",
                                        BARNHARDT == "Mg" ~ "mix",
                                        BARNHARDT == "Mr" ~ "mix",
                                        BARNHARDT == "Sg" ~ "mix",
                                        BARNHARDT == "Sr" ~ "mix",
                                        BARNHARDT == "G" ~ "gravel",
                                        BARNHARDT == "G" ~ "gravel",
                                        BARNHARDT == "G*" ~ "gravel",
                                        BARNHARDT == "Gm" ~ "gravel",
                                        BARNHARDT == "Gr" ~ "gravel",
                                        BARNHARDT == "Gs" ~ "gravel",
                                        BARNHARDT == "Gs*" ~ "gravel",
                                        BARNHARDT == "Sg or Gs" ~ "gravel",
                                        BARNHARDT == "R**" ~ "rock",
                                        BARNHARDT == "Rf" ~ "rock",
                                        BARNHARDT == "Rg" ~ "rock",
                                        BARNHARDT == "Rm" ~ "rock",
                                        BARNHARDT == "Rs" ~ "rock"))
  

#####################################
#####################################

