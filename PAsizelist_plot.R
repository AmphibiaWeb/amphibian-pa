#I have to run this on the server...
#first, in bash, 
cd /space/s2/emma/ConsWG                   
R
                   
#Then, in R, ... 
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(SciViews)
library(tidyr)
library(ggrepel)
library(forcats)
library(ggridges)
library(lessR)
library(data.table)                   
library(vegan)  
                   
#The other thing I'm going to do here is see comparative accumulation curves for PA areas of different size brackets
SpPA <- read.csv("All_Amphibia with WDPA one to many.csv")
SpPA$WDPA_PID[SpPA$WDPA_PID == ""] <- "NotProtected"
wdpa <- read.csv("WDPA_WDOECM_Jul2021_Public_all_csv.csv")
#We need to take out marine area from what we're considering as protected area size!
wdpa$GIS_T_AREA <-wdpa$GIS_AREA - wdpa$GIS_M_AREA
#Make status year numeric for following analysis
wdpa$STATUS_YR <- as.numeric(wdpa$STATUS_YR)
#Remove blank rows. 
wdpa <- wdpa[rowSums(is.na(wdpa)) != ncol(wdpa),]                   
Species_Area <- left_join(SpPA, wdpa, by=c("WDPA_PID"="WDPA_PID"))                  
                
#0-10km^2                   
tiny_list<-wdpa$WDPA_PID[wdpa$GIS_T_AREA>0 & wdpa$GIS_T_AREA<=10]    
tiny<-input[rownames(input) %in% tiny_list, ]   #subset only this size class of PAs                                
                   
#10-100km^2                   
small_list<-wdpa$WDPA_PID[wdpa$GIS_T_AREA>10 & wdpa$GIS_T_AREA<=100,]                   
small<-input[small_list,]  #subset only this size class of PAs  

#100-1000km^2                   
med_list<-wdpa$WDPA_PID[wdpa$GIS_T_AREA>100 & wdpa$GIS_T_AREA<=1000,] 
med<-input[med_list,]  #subset only this size class of PAs  

#1000-10000km^2                   
large_list<-wdpa$WDPA_PID[wdpa$GIS_T_AREA>1000 & wdpa$GIS_T_AREA<=10000,] 
large<-input[large_list,]  #subset only this size class of PAs  
                   
#10000-100000km^2                   
huge_list<-wdpa$WDPA_PID[wdpa$GIS_T_AREA>10000 & wdpa$GIS_T_AREA<=100000] #grab the PAs that meet that size criterion  
huge_list <- huge_list[!is.na(huge_list)] #remove na values from this vector just in case
#How many amphibians in all of these reserves?            
#length(SpPA$binomial[SpPA$WDPA_PID %in% tiny_list])                   
                 
jpeg("../Figures/PA_size/SAC_0-10.jpg", width = 2000, height = 2000)               
ggplot(tiny_growth)+
    geom_point(aes(x=idu, y=tiny_growth[1]), cex=0.5) +
    geom_smooth()+               
    labs(y="Cumulative species count", x="PA size (km2)") +        
    theme(axis.text.x = element_text(angle=45, size=45, vjust=0.5), axis.text.y = element_text(size=45), axis.title=element_text(size=45), 
        plot.title=element_blank(), legend.text=element_text(size=40), legend.title=element_text(size=45), legend.key.height = unit(3, "cm"), 
        legend.key.width = unit(1.5,"cm"), panel.background = element_rect(fill = "white", colour = "grey50"), strip.text = element_text(size=45)) +
  scale_x_continuous(limits = c(0,nrow(tiny_growth)), expand = c(0,0)) + #, breaks=c(log(10), log(100), log(1000), log(10000), log(100000)), labels=c(10, 100, 1000, 10000, 100000)) + 
  scale_y_continuous(limits = c(0,7000), expand = c(0, 0))
dev.off()                     
                   
