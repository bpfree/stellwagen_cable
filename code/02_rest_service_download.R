#################################################
### 02. Download Data -- REST server download ###
#################################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

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
               RSelenium,
               sf,
               sp,
               stringr,
               targets,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

data_dir <- "data/a_raw_data"

#####################################
#####################################

rest_services_function <- function(url_list, base_url, data_dir){
  # define base URL (the service path)
  base_url <- base_url
  
  # define the unique dataset URL ending
  full_url <- url_list
  
  # combine the base with the dataset URL to create the entire data URL
  data_url <- file.path(base_url, full_url)
  
  # pull the spatial layer from the REST server
  data <- arcpullr::get_spatial_layer(data_url)
  
  # get the unique data name (when applicable)
  dir_name <- stringr::str_split(url_list, pattern = "/")[[1]][1]
  
  # create new directory for data
  dir_create <- dir.create(file.path(data_dir, dir_name))
  
  # set the new pathname to export the data
  new_dir <- file.path(data_dir, dir_name)
  
  # export the dataset
  sf::st_write(obj = data, dsn = file.path(new_dir, paste0(dir_name, ".shp")), delete_layer = F)
}

#####################################

# Massachusetts CZM datasets (https://czm-moris-mass-eoeea.hub.arcgis.com/)

## intertidal flats (https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Intertidal_Flats/FeatureServer)
### MORIS: https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::intertidal-flats
### metadata: https://www.arcgis.com/sharing/rest/content/items/cc7505bab5d44e5d9c9984a887e6bb3e/info/metadata/metadata.xml?format=default&output=html
### data source: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/ArcGIS/rest/services/Intertidal_Flats/FeatureServer/0

## sand patches (https://services1.arcgis.com/7iJyYTjCtKsZS1LR/ArcGIS/rest/services/Preliminary_offshore_sand_resources_APTIM_Technical_Report_No_631226219_2018/FeatureServer)
### MORIS: https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::preliminary-offshore-sand-resources-aptim-technical-report-no-631226219-2018
### metadata: https://www.arcgis.com/sharing/rest/content/items/be12362142734c6d8e50392dd219eec5/info/metadata/metadata.xml?format=default&output=html
### RESTService: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Preliminary_offshore_sand_resources_APTIM_Technical_Report_No_631226219_2018/FeatureServer/0

## liquid natural gas
### Neptune: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Neptune_LNG_Pipeline/FeatureServer
#### MORIS: https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::neptune-lng-pipeline
#### metadata: https://www.arcgis.com/sharing/rest/content/items/0df6b39f58444cc9a236755b75d1c92b/info/metadata/metadata.xml?format=default&output=html
#### RESTService: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Neptune_LNG_Pipeline/FeatureServer/1

### Northeast: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Northeast_Gateway_LNG_Pipeline/FeatureServer
#### MORIS: https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::northeast-gateway-lng-pipeline
#### metadata: https://www.arcgis.com/sharing/rest/content/items/7826f80140934c869571dfb2b46f0313/info/metadata/metadata.xml?format=default&output=html
#### RESTService: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Northeast_Gateway_LNG_Pipeline/FeatureServer/1

### Algonquin: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Algonquin_Hubline_LNC_Pipeline/FeatureServer
#### MORIS: https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::algonquin-hubline-lnc-pipeline
#### metadata: https://www.arcgis.com/sharing/rest/content/items/8cbe1bdd72a443a5bf04c2d50c78df10/info/metadata/metadata.xml?format=default&output=html
#### RESTService: https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Algonquin_Hubline_LNC_Pipeline/FeatureServer/1

url_list <- c(
  "Intertidal_Flats/FeatureServer/0",
  "Preliminary_offshore_sand_resources_APTIM_Technical_Report_No_631226219_2018/FeatureServer/0",
  "Neptune_LNG_Pipeline/FeatureServer/1",
  "Northeast_Gateway_LNG_Pipeline/FeatureServer/1",
  "Algonquin_Hubline_LNC_Pipeline/FeatureServer/1"
)

parallel::detectCores()[1]
cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = url_list, fun = rest_services_function,
                            base_url = "https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services", data_dir = data_dir)

parallel::stopCluster(cl = cl)

#####################################

# channel areas dataset
## https://services7.arcgis.com/n1YM8pTrFmm7L4hs/ArcGIS/rest/services/National_Channel_Framework/FeatureServer/1
### data source: https://czm-moris-mass-eoeea.hub.arcgis.com/datasets/Mass-EOEEA::national-channel-framework-channel-area-acoe

url_list <- c(
  "National_Channel_Framework/FeatureServer/1"
)

parallel::detectCores()[1]
cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = url_list, fun = rest_services_function,
                            base_url = "https://services7.arcgis.com/n1YM8pTrFmm7L4hs/ArcGIS/rest/services", data_dir = data_dir)

parallel::stopCluster(cl = cl)

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

rest_services_function <- function(url_list, base_url, data_dir){
  # define base URL (the service path)
  base_url <- base_url
  
  # define the unique dataset URL ending
  full_url <- url_list
  
  # combine the base with the dataset URL to create the entire data URL
  data_url <- file.path(base_url, full_url)
  
  # pull the spatial layer from the REST server
  data <- arcpullr::get_spatial_layer(data_url)
  
  # get the unique data name (when applicable)
  dir_name <- stringr::str_split(url_list, pattern = "/")[[1]][4]
  
  # create new directory for data
  dir_create <- dir.create(file.path(data_dir, dir_name))
  
  # set the new pathname to export the data
  new_dir <- file.path(data_dir, dir_name)
  
  # export the dataset
  sf::st_write(obj = data, dsn = file.path(new_dir, paste0(dir_name, ".shp")), delete_layer = F)
}

#####################################

# submarine cable and pipeline data
## https://nauticalcharts.noaa.gov/data/gis-data-and-services.html
## https://nauticalcharts.noaa.gov/learn/encdirect/#map-services

url_list <- c(
  "encdirect/enc_overview/MapServer/61",
  "encdirect/enc_general/MapServer/69",
  "encdirect/enc_coastal/MapServer/88",
  "encdirect/enc_approach/MapServer/118",
  "encdirect/enc_approach/MapServer/119",
  "encdirect/enc_harbour/MapServer/114",
  "encdirect/enc_berthing/MapServer/54"
)

parallel::detectCores()[1]
cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = url_list, fun = rest_services_function,
                            base_url = "https://encdirect.noaa.gov/arcgis/rest/services", data_dir = data_dir)

parallel::stopCluster(cl = cl)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate