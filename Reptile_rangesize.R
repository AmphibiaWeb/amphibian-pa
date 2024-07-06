# R language

# Harvest reptile range sizes from GARD shapefiles
# I provide you with the harvested values as GARD_extraReptiles.csv

# Set up the working environment
library(sf)

# Read in the shapefile from the Global Assessment of Reptile Distributions group
# This raw data must be obtained from GARD as linked in Dryad
rep <- st_read("Your/path/modeled_reptiles.shp")
st_crs(rep)  

# Create a dataframe to hold reptile data
repDF <- data.frame(Species = rep$Binomial, Area = numeric(length(rep$Binomial)))

# Fill dataframe with area data per species
for (row in seq_len(nrow(repDF))) {
  species_name <- repDF$Species[row]
  matching_index <- which(rep$Binomial == species_name)
  if (length(matching_index) > 0) {
    matching_polygon <- rep[matching_index, ]
    matching_polygon <- st_make_valid(matching_polygon)    #fix broken geometries  -- very few (10) remain with invalid geometries after this
    if (st_is_valid(matching_polygon)) {
      repDF$Area[row] <- st_area(matching_polygon)
    } else {
      warning(paste("Invalid geometry after attempted fix for species:", species_name))
      repDF$Area[row] <- NA
    }
  } else {
    warning(paste("No match found for species:", species_name))
    repDF$Area[row] <- NA
  }
}

rm(rep)

# Prep data for joining with other taxa
repDF$label<-"reptiles"
repDF$category<-"NA"

# Now, read in data from IUCN. 
ranges <- read.csv("G:/.shortcut-targets-by-id/1zvJrRR38XEN9N1yR69JmJj_84BhVKKec/AW_ConservationWG/ConservationWG_githubrepo/SmallIsBig_data/OtherTaxaRanges/All_taxa_ranges.csv")
ranges <- ranges[ranges$label=="reptiles",c("binomial", "label", "category", "Area")] 

# Merge with GARD data
all_reps<-merge(repDF, ranges, by.x="Species", by.y="binomial", all=T)

# Remove species that were already in IUCN dataset
repDF$Species[!(repDF$Species %in% ranges$binomial)]

# Export only the data for species not already present in IUCN dataset
write.csv(repDF[!(repDF$Species %in% ranges$binomial),], "Your/path/OtherTaxaRanges/GARD_extraReptiles.csv")
