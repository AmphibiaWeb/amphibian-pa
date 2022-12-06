#WDPA Analysis              #
#Author: Emma Steigerwald   #
#Date: 2 Jan 2020           #
#############################

#Set up working environment
setwd("C:/Users/Lenovo/Google Drive/Outreach/AmphibiaWeb/WDPA/")
library(ggplot2)

#Read in WDPA data from (https://www.protectedplanet.net/),
#filtered for only terrestrial reserves
wdpa <- read.csv("WDPA_analysis_TerrestrialOnly.csv")


#Make a histogram of IUCN_CAT
#First, check class of this data column. We want it to be expressed as a factor. 
class(wdpa$IUCN_CAT)
wdpa$IUCN_CAT <- as.factor(wdpa$IUCN_CAT)
wdpa$IUCN_CAT <- factor(wdpa$IUCN_CAT,levels = c("Ia", "Ib", "II", "III", "IV", 
                                                 "V", "VI", "Not Applicable", 
                                                 "Not Assigned", "Not Reported"))
#Now, make the histogram in ggplot of IUCN_CAT
ggplot(wdpa, aes(x=IUCN_CAT))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title="Histogram of PA IUCN Category", x ="IUCN protection category", y = "Count")


#Make histogram of protected area size, represented as GIS rather than reported area
class(wdpa$GIS_M_AREA) #numeric-- excellent
class(wdpa$GIS_AREA ) #numeric-- excellent
#Calculate terrestrial areas, free from marine portion of coastal preserves
wdpa$GIS_T_AREA <- wdpa$GIS_AREA - wdpa$GIS_M_AREA
wdpa$GIS_T_logAREA <- log(wdpa$GIS_T_AREA)
#Function to express axis labels with 2 decimal points only
scaleFUN <- function(x) sprintf("%.2f", exp(x))
ggplot(wdpa, aes(GIS_T_logAREA)) +
  geom_histogram()+
  scale_x_continuous(breaks=seq(-12, 14, by=1), labels=scaleFUN, limits=c(-12,14)) +
  scale_y_continuous(trans='log') +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title="Histogram of PA Area", x ="Log scaled axis of PA area \n (expressed as untransformed value [km^2])", y = "Count")
