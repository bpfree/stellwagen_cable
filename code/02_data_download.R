#########################
### 02. download data ###
#########################

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
               sf,
               sp,
               stringr,
               targets,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# Commentary on R and code formulation:
## ***Note: If not familiar with dplyr notation
## dplyr is within the tidyverse and can use %>%
## to "pipe" a process, allowing for fluidity
## Can learn more here: https://style.tidyverse.org/pipes.html

## another common coding notation used is "::"
## For instance, you may encounter it as dplyr::filter()
## This means "use the filter function from the dplyr package"
## Notation is used given sometimes different packages have
## the same function name, so it helps code to tell which
## package to use for that particular function.
## The notation is continued even when a function name is
## unique to a particular package so it is obvious which
## package is used

#####################################
#####################################

# Create function that will pull data from publicly available websites
## This allows for the analyis to have the most current data; for some
## of the datasets are updated with periodical frequency (e.g., every 
## month) or when needed. Additionally, this keeps consistency with
## naming of files and datasets.
### The function downloads the desired data from the URL provided and
### then unzips the data for use

data_download_function <- function(download_list, data_dir){
  
  # designate the URL that the data are hosted on
  url <- download_list
  
  # file will become last part of the URL, so will be the data for download
  file <- basename(url)
  
  # Download the data
  if (!file.exists(file)) {
    options(timeout=100000)
    # download the file from the URL
    download.file(url = url,
                  # place the downloaded file in the data directory
                  destfile = file.path(data_dir, file),
                  mode="wb")
  }
  
  # Unzip the file if the data are compressed as .zip
  ## Examine if the filename contains the pattern ".zip"
  ### grepl returns a logic statement when pattern ".zip" is met in the file
  if (grepl(".zip", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- sub(".zip", "", file)
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
}

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data"

#####################################
#####################################

# Download list
download_list <- c(

  ## active and inactive disposal sites (source: https://marinecadastre.gov/downloads/data/mc/OceanDisposalSite.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::ocean-disposal-sites
  ### metadata: https://www.fisheries.noaa.gov/inport/item/54193
  
  "https://marinecadastre.gov/downloads/data/mc/OceanDisposalSite.zip",
  
  ## anchorage areas (source: https://marinecadastre.gov/downloads/data/mc/Anchorage.zip)
  ### MarineCadastre: https://marinecadastre-noaa.hub.arcgis.com/datasets/noaa::anchorages
  ### metadata: https://www.fisheries.noaa.gov/inport/item/48849
  
  "https://marinecadastre.gov/downloads/data/mc/Anchorage.zip",
  
  ## CONMAPSG (source: https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg.zip)
  ### metadata: https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg.htm
  ### metadata (text): https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg-metadata.txt
  ### FAQ: https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg-faq.htm
  
  "https://pubs.usgs.gov/of/2005/1001/data/conmapsg/conmapsg.zip"
)

#####################################
#####################################

parallel::detectCores()

cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = download_list, fun = data_download_function, data_dir = data_dir)

parallel::stopCluster(cl = cl)

#####################################
#####################################

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
