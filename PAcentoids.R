# R language

# Calculating the centroid of all polygons in the WDPA 
#       Note: there are multiple shapefiles with batches of PAs, and similarly we will export batchs of centroid data

# Set up working environment
library(sf)
library(dplyr)
library(purrr)

# Establish filepath to WDPA shapefiles of PAs
directory_path <- "G:/.shortcut-targets-by-id/1zvJrRR38XEN9N1yR69JmJj_84BhVKKec/AW_ConservationWG/WDPA Shapefiles with Country Name/"

# Establish function to import a single shapefile, then harvest its WDPA and centroid lat/lon
process_shapefile <- function(file_path) {
  shape_data <- st_read(file_path)
  # Establish datatable output to store harvested data in
  output<-data.frame(Name = rep(NA, nrow(shape_data)),
  AvgLat = rep(NA, nrow(shape_data)))
  # Read the shapefile
  for (POL in 1:nrow(shape_data)) {
    name<-shape_data$WDPA_PID[POL]    
    # Extract the geometry of the first polygon
    first_polygon <- st_geometry(shape_data)[POL]
    # Extract the latitude (Y) coordinates for each vertex
    mean_lat <- mean(st_coordinates(first_polygon)[, "Y"])
    print(paste0("WDPA ", name, " is at approx. ", mean_lat))
    output[POL,1:2]<-c(name, mean_lat)
    }
  csv_name <- paste0(tools::file_path_sans_ext(basename(file_path)), "_centroid_data.csv")
  write.csv(output, file = csv_name, row.names = FALSE, quote = FALSE)
  }

# Get a list of all shapefiles in the directory
shapefiles <- list.files(directory_path, pattern = "\\.shp$", full.names = TRUE)

# Process each shapefile
for (FILE in 1:length(shapefiles)) {
  f <- shapefiles[FILE]
  process_shapefile(f)
}
