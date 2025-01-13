################################
### 23. start and end points ###
################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# set parameters
## designate region name
region_name <- "stellwagen"

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
data_dir <- "11_Stellwagen_Cable_Routing/Model Runs/StellwagenCableRoute/03_CableRouteModel/model_2/model_2.gdb"
lease_dir <- "data/a_raw_data/gulfofmainefsnareasgeodatabase/Gulf_of_Maine_FSN_areas_09_10_2024.gdb"
stellwagen_dir <- "data/a_raw_data/sbnms_py2"

### output directory
output_gpkg <- "data/c_analysis_data/wind.gpkg"
output_dir <- "data/e_least_cost_path_data/stellwagen_route_options.gpkg"
csv_dir <- "data/c_analysis_data"

#####################################

# inspect layers within directories
sf::st_layers(dsn = data_dir,
              do_count = TRUE)

sf::st_layers(dsn = lease_dir,
              do_count = TRUE)

#####################################
#####################################

# start point
## centralized
# start_point <- sf::st_read(dsn = data_dir,
#                     layer = sf::st_layers(dsn = data_dir)[[1]][grep(pattern = "2_2_corridors_start",
#                                                                     x = sf::st_layers(dsn = data_dir)[[1]])]) %>%
#   sf::st_zm() %>%
#   sf::st_cast("POINT") %>%
#   sf::st_transform(x = .,
#                    crs = crs)

#####################################

## edge
leases <- sf::st_read(dsn = lease_dir,
                      layer = sf::st_layers(dsn = lease_dir)[[1]][grep(pattern = "all_outlines",
                                                                          x = sf::st_layers(dsn = lease_dir)[[1]])]) %>%
  # filter for only the leases of interest (0564 and 0567)
  dplyr::filter(grepl(pattern = "0564|0567",
                      # find the pattern within the "ADDITIONAL_INFORMATION" field
                      ADDITIONAL_INFORMATION))

### lease 0564
lease_0564 <- leases %>%
  # only lease 0564
  dplyr::filter(grepl(pattern = "0564",
                      # find the pattern within the "ADDITIONAL_INFORMATION" field
                      ADDITIONAL_INFORMATION)) %>%
  # make it first a multilinestring object
  sf::st_cast(x = .,
              to = "MULTILINESTRING") %>%
  # then make it a collection of points
  sf::st_cast(x = .,
              to = "POINT") %>%
  # create fields for longitude and latitude
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

lease_0564_edge_point <- lease_0564 %>%
  # limit it to the furthest west point (xmin)
  dplyr::filter(lon <= sf::st_bbox(.)$xmin) %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################

### lease 0567
lease_0567 <- leases %>%
  # limit to lease 0567
  dplyr::filter(grepl(pattern = "0567",
                      # find the pattern within the "ADDITIONAL_INFORMATION" field
                      ADDITIONAL_INFORMATION)) %>%
  # make it first a multilinestring object
  sf::st_cast(x = .,
              to = "MULTILINESTRING") %>%
  # then make it a collection of points
  sf::st_cast(x = .,
              to = "POINT") %>%
  # create fields for longitude and latitude
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

lease_0567_edge_point <- lease_0567 %>%
  # limit it to the furthest west point (xmin)
  dplyr::filter(lon <= sf::st_bbox(.)$xmin) %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################

# combine edge points
start_edge_points <- rbind(lease_0564_edge_point,
                           lease_0567_edge_point) %>%
  sf::st_zm() %>%
  sf::st_cast("POINT") %>%
  sf::st_transform(x = .,
                   crs = crs)

#####################################
#####################################

## Stellwagen boundary
stellwagen <- sf::st_read(dsn = stellwagen_dir) %>%
  sf::st_transform(x = .,
                   crs = crs) %>%
  # shrink the boundary
  ## this will allow points to be inside the boundary
  sf::st_buffer(x = .,
                # compress by 10 meters
                dist = -10) %>%
  # change to linestring
  sf::st_cast("LINESTRING")

stellwagen_start <- stellwagen %>%
  # create points along the linestring
  sf::st_line_sample(x = .,
                     # a point every 1 km
                     density = units::set_units(1, 1/km)) %>%
  sf::st_as_sf() %>%
  sf::st_cast("POINT") %>%
  # create fields for longitude and latitude
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2],) %>%
  dplyr::filter(lon >= sf::st_bbox(.)$xmax - (sf::st_bbox(.)$xmax - sf::st_bbox(.)$xmin) / 3) %>%
  # get the points along the eastern boundary
  dplyr::slice_head(n = 77) %>%
  # create start index
  dplyr::mutate(index_start = row_number())

plot(stellwagen_start$x)

stellwagen_end <- stellwagen %>%
  # create points along the linestring
  sf::st_line_sample(x = .,
                     # a point every 1 km
                     density = units::set_units(1, 1/km)) %>%
  sf::st_as_sf() %>%
  sf::st_cast("POINT") %>%
  # create fields for longitude and latitude
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  dplyr::filter(lon <= sf::st_bbox(.)$xmax - (sf::st_bbox(.)$xmax - sf::st_bbox(.)$xmin) / 2) %>%
  # create start index
  dplyr::mutate(index_start = 77 + row_number())

plot(stellwagen_end$x)

#####################################

a <- stellwagen_start %>%
  sf::st_drop_geometry() %>%
  dplyr::select(lon, lat) %>%
  dplyr::rename(starts_x = lon, starts_y = lat) %>%
  # create start index
  dplyr::mutate(index_start = row_number())

b <- stellwagen_end %>%
  sf::st_drop_geometry() %>%
  dplyr::select(lon, lat) %>%
  dplyr::rename(ends_x = lon, ends_y = lat) %>%
  # create start index
  dplyr::mutate(index_end = 77 + row_number())

pairs <- tidyr::crossing(a, b) %>%
  dplyr::mutate(index = row_number()) %>%
  dplyr::relocate(index, .before = starts_x)

View(pairs)

for(i in seq(nrow(a))){
  for(j in seq(nrow(b))){
    
    row <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(as.numeric(c(a[i, 1:2], b[j, 1:2])), ncol = 2, byrow = TRUE)), crs = crs))
    
    row <- sf::st_set_geometry(row, "geometry")
    
    row <- cbind(row, a[i,], b[j,])
    
    if(i==1 & j==1){out_df = row}
    else{out_df = rbind(out_df, row)}
  }
}

row

out_df$geometry[1,]

View(out_df)
plot(out_df$geometry)

g <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = stellwagen_start, color = "lightblue", size = 2) +
  ggplot2::geom_sf(data = stellwagen_end, color = "darkred", size = 2) +
  ggplot2::geom_sf(data = out_df, color = "black", linetype = "dashed")

g

write.csv(x = pairs, file = file.path(csv_dir, stringr::str_glue("{region_name}_points.csv")))
sf::st_write(obj = out_df, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_lines", append = F))

#####################################
#####################################

# end points
end_point <- rbind(c("point",-70.995, 42.341), # Boston
                   c("point",-70.623, 41.980)) %>% # Plymouth
  # convert to data frame
  as.data.frame() %>%
  # rename column names
  dplyr::rename("point" = "V1",
                "lon" = "V2",
                "lat" = "V3") %>%
  # new field
  dplyr::mutate("city" = row_number()) %>%
  # recode city names
  dplyr::mutate(city = recode(city,
                              "1" = "Boston",
                              "2" = "Plymouth")) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject the coordinate reference system
  sf::st_transform(crs) # "EPSG:26919" (NAD83 UTM 19N: https://epsg.io/26919)

boston <- end_point %>%
  dplyr::filter(city == "Boston") %>%
  dplyr::select(geometry) %>%
  dplyr::rename("Shape" = "geometry")

plymouth <- end_point %>%
  dplyr::filter(city == "Plymouth") %>%
  dplyr::select(geometry) %>%
  dplyr::rename("Shape" = "geometry")

#####################################
#####################################

# route combinations
boston_0564 <- lease_0564_edge_point %>%
  select(Shape) %>%
  rbind(boston)

boston_0567 <- lease_0567_edge_point %>%
  select(Shape) %>%
  rbind(boston)

plymouth_0564 <- lease_0564_edge_point %>%
  select(Shape) %>%
  rbind(plymouth)

plymouth_0567 <- lease_0567_edge_point %>%
  select(Shape) %>%
  rbind(plymouth)

#####################################
#####################################

# export data
# sf::st_write(start_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_start_point"), append = FALSE)
sf::st_write(lease_0564_edge_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_0564_edge_start"), append = FALSE)
sf::st_write(lease_0567_edge_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_0567_edge_start"), append = FALSE)
# sf::st_write(start_edge_points, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_edge_start_points"), append = FALSE)

sf::st_write(stellwagen_start, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_stellwagen_starts"), append = FALSE)
sf::st_write(stellwagen_end, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_stellwagen_ends"), append = FALSE)

sf::st_write(end_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_end_points"), append = FALSE)
sf::st_write(plymouth, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_plymouth_end_point"), append = FALSE)
sf::st_write(boston, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_boston_end_point"), append = FALSE)

# sf::st_write(end_point, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_end_points_1000m"), append = FALSE)

sf::st_write(plymouth_0564, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_plymouth_0564"), append = FALSE)
sf::st_write(plymouth_0567, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_plymouth_0567"), append = FALSE)
sf::st_write(boston_0564, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_boston_0564"), append = FALSE)
sf::st_write(boston_0567, dsn = output_gpkg, layer = stringr::str_glue("{region_name}_boston_0567"), append = FALSE)

## final geopackage
sf::st_write(lease_0564_edge_point, dsn = output_dir, layer = stringr::str_glue("{region_name}_0564_edge_start"), append = FALSE)
sf::st_write(lease_0567_edge_point, dsn = output_dir, layer = stringr::str_glue("{region_name}_0567_edge_start"), append = FALSE)

sf::st_write(stellwagen_start, dsn = output_dir, layer = stringr::str_glue("{region_name}_stellwagen_east"), append = FALSE)
sf::st_write(stellwagen_end, dsn = output_dir, layer = stringr::str_glue("{region_name}_stellwagen_west"), append = FALSE)
sf::st_write(obj = out_df, dsn = output_dir, layer = stringr::str_glue("{region_name}_lines"), append = FALSE)

sf::st_write(end_point, dsn = output_dir, layer = stringr::str_glue("{region_name}_landing_sites"), append = FALSE)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
