# R language

# Plot of amphibian species richness based on PA size and analysis of how species area is impacted by PA characteristics

#Set up working environment
library(dplyr)
library(ggplot2) 
library(plyr) 
library(hexbin)
library(RColorBrewer)
library(stringr)
library(cowplot)
library(SciViews)
library(tidyr) 
library(ggpattern)
library(forcats)
library(khroma)
muted <- colour("muted")
library(lessR)
library(data.table)
library(scales)
library(extrafont)
font_import()
library(car)
library(MASS)
library(lmtest)
library(pscl)

#####################

#Load all species found in all PAs
SpPA <- read.csv("Data using Amphibian Ranges for Species Richness/All_Amphibia with WDPA one to many.csv")

# Associate a threat level with these amphibians
Amph <- read.csv("Data using Amphibian Ranges for Species Richness/Amphibians_w_ranges.csv")
length(unique(Amph$binomial))   
Amph$code[Amph$code==""] <- "In assessment"
Amph$code[Amph$code=="V" | Amph$code=="VU-provisional"] <- "VU"
Amph$code[Amph$code=="NT-provisional" | Amph$code=="NT-Provisional"] <- "NT"
Amph$code[Amph$code=="EN-provisional"] <- "EN"
Amph$code[Amph$code=="DD-provisional" | Amph$code=="DD- provisional"] <- "DD"
Amph$code[Amph$code=="CR-provisional" | Amph$code=="CR-Provisional" | Amph$code=="CE-provisional"] <- "CR"
Amph$code[Amph$code=="LC-provisional"] <- "LC"
Amph$code<-as.factor(Amph$code)
levels(Amph$code)
keep<-c("binomial", "code")
Amph<-Amph[,names(Amph)%in%keep]
names(Amph)<-c("binomial", "AWiucn")

SpPA <- left_join(Amph, SpPA, by="binomial")

#Label amphibians not on PAs as "not protected"
SpPA$WDPA_PID[SpPA$WDPA_PID == ""] <- "NotProtected"

# Count amphibian species per PA
SpPaSum <- SpPA %>%
  dplyr::group_by(WDPA_PID) %>%
  dplyr::summarise(totRichness = length(unique(binomial))) 

# Count threatened amphibians per PA per IUCN threatened category
ThreatSum <- SpPA %>%
  dplyr::group_by(WDPA_PID, AWiucn) %>%
  dplyr::summarise(count = length(unique(binomial)))    
#Label the catch all for amphibians not on PAs
ThreatSum$WDPA_PID[ThreatSum$WDPA_PID == ""] <- "NotProtected"

#Make this horizontal format
ThreatSum <- ThreatSum %>% spread(AWiucn, count)
SpPaSum <- left_join(SpPaSum, ThreatSum, by=c("WDPA_PID"="WDPA_PID"))

#Calculate total number threatened species
SpPaSum$AllThreat <- rowSums(SpPaSum[, c('CR', 'EN', 'EW', 'EX', 'NT', 'VU')], na.rm=TRUE) 
#Total species assessed (not DD or in assessment)
SpPaSum$AllAssessed <- rowSums(SpPaSum[, c('CR', 'EN', 'EW', 'EX', 'NT', 'VU', 'LC')], na.rm=TRUE) 
#Total proportion threatened species
SpPaSum$ThreatProp <- SpPaSum$AllThreat/SpPaSum$AllAssessed              

# Import the country, continent, hemisphere info
xtra <- read.csv("World_Database_Protected_Areas/WDPA_all.csv")
names(xtra)[names(xtra) == "NAME_EN"] <- "Country"
names(xtra)[names(xtra) == "ï..WDPA_PID"] <- "WDPA_PID"
xtra <- xtra[ , which(names(xtra) %in% c("WDPA_PID", "CONTINENT", "Country"))]
xtra <- unique(xtra)

# Bring in the WDPA data
wdpa <- read.csv("World_Database_Protected_Areas/WDPA_WDOECM_Jul2021_Public_all_csv/WDPA_WDOECM_Jul2021_Public_all_csv.csv")
#We need to take out marine area from what we're considering as protected area size!
wdpa$GIS_T_AREA <-wdpa$GIS_AREA - wdpa$GIS_M_AREA
#Make status year numeric for following analysis
wdpa$STATUS_YR <- as.numeric(wdpa$STATUS_YR)
#Remove blank rows. 
wdpa <- wdpa[rowSums(is.na(wdpa)) != ncol(wdpa),]

#Now, merge...
SpPaSum <- left_join(SpPaSum, wdpa, by=c("WDPA_PID"="WDPA_PID"))
SpPaSum <- left_join(SpPaSum, xtra, by=c("WDPA_PID"="WDPA_PID"))
#Remove extra columns
torem <- c("ï..TYPE", "WDPAID", "PA_DEF", "DESIG", "DESIG_ENG", "DESIG_TYPE", "INT_CRIT", "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TAKE", "NO_TK_AREA", "STATUS", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "VERIF", "METADATAID", "SUB_LOC", "PARENT_ISO3", "ISO3", "SUPP_INFO", "CONS_OBJ", "all")           
SpPaSum <- SpPaSum[ , -which(names(SpPaSum) %in% torem)]
           
#We decided to remove PAs where amphibians don't occur from our analyses
RHT_WDPA <- SpPaSum[SpPaSum$totRichness > 0,]
#Remove blank rows
RHT_WDPA <- RHT_WDPA[rowSums(is.na(RHT_WDPA)) != ncol(RHT_WDPA),]
#replace continent names in all to clarify
RHT_WDPA <- as.data.frame(lapply(RHT_WDPA ,function(x) if(is.character(x)|is.factor(x)) gsub("Oceania","Australia and Oceania", x) else x))
RHT_WDPA <- as.data.frame(lapply(RHT_WDPA ,function(x) if(is.character(x)|is.factor(x)) gsub("North America","North and Central America", x) else x))
#Remove records that do not seem to correspond to established protected areas, including those with their area name 
#recorded as “area not protected” (2599 polygons), or their status recorded as “degazetted,”“proposed,”“recommended,”
#“in preparation,” or “unset” (1024 polygons and 1751 points). Check for them by manually revising the search results of
#the following
RHT_WDPA %>% filter_all(any_vars(grepl("not protected|degazetted|proposed|recommended|in preparation|unset",.)))
#Filter out PID 555564362 and 305921, which are both "proposed" rather than created it looks like  
RHT_WDPA <- RHT_WDPA[!(RHT_WDPA$WDPA_PID=="555564362" | RHT_WDPA$WDPA_PID=="305921"),]

#Get rid of everything we don't need, because it burdens the system
remove(SpPA, xtra, wdpa, torem, SpPaSum, threatdata, ThreatSum, gdp)
                                
#Get rid of any NA values in columns of interest
AR <- RHT_WDPA[RHT_WDPA$GIS_T_AREA != 0, ]
AR <- AR[!is.na(AR$GIS_T_AREA), ]

# Hexbin of PA area by species richness
jpeg("../Figures/PAsize-SpeciesRichness_HexBin(ln).jpg", width = 2000, height = 2000, res=100, quality = 100)
ggplot(data=AR, aes(x=GIS_T_AREA, y=totRichness, width=1)) +
  stat_binhex(aes(fill=..count..), bins=75, colour="black", size=0.01) + 
  scale_fill_gradientn(trans = log10_trans(),
                      colours = c("#117733", "#DDCC77", "#332288"), 
                      values= c(0, 0.45, 0.65, 1), 
                      breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x))) +  
  geom_smooth(method="lm", formula=y~x, color="black", size=2, linetype=4)+                                                  
  labs(title="", y = "Species richness", fill = "Count") +
  theme(text=element_text(family="Calibri"), 
        axis.text.x = element_text(angle=45, size=45, vjust=0.5), 
        axis.text.y = element_text(size=45), axis.title=element_text(size=45), 
        plot.title=element_text(size=45), legend.text=element_text(size=40), 
        legend.title=element_text(size=45), legend.key.height = unit(3, "cm"), 
        legend.key.width = unit(1.5,"cm"), 
        panel.background = element_rect(fill = "white", colour = "grey50")) +
  scale_x_continuous(trans='log10', 
                     name=expression("Protected area size ("~km^2~", log scale)"),                               
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)), 
                     expand=c(0.008,0.008),
                     limits=c(1,10^5))  + 
  scale_y_continuous(expand = c(0.008,0.008))
dev.off()   
                                  
######################################################################

# Regression analyses modelling impact of PA characteristics on amphibian species richness                     
                     
# For regressions and tests, I want to include latitude as a factor:
directory_path <- "your/path/WDPA Shapefiles with Country Name/"
csvs <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE)                     
# Initialize an empty data frame to store the combined data
combined_data <- data.frame()
# Read and rbind each CSV file
for (csv_file in csvs) {
  current_data <- read.csv(csv_file)
  combined_data <- rbind(combined_data, current_data)  
  # Remove the original object to save memory
  rm(current_data)
}                    
combined_data$AvgLat<-  abs(round(combined_data$AvgLat, 2))  
AR <- na.omit(AR, cols = "WDPA_PID")
combined_data <- na.omit(combined_data)
# merge as data.tables,  as R is better able to deal with very large ones.                      
setDT(AR)
setDT(combined_data)                    
AR <- merge(AR, combined_data, by.y = "Name", by.x = "WDPA_PID", all.x = TRUE)
                     
# Clean up unneeded data and columns                     
rm(list=c("Amph", "RHT_WDPA", "csvs", "csv_file", "directory_path", "keep", "muted"))                    
AR<-AR[,c("WDPA_PID", "totRichness", "GIS_T_AREA", "IUCN_CAT", "STATUS_YR")]  
set.seed(123)  # Set seed for reproducibility
subset_AR <- AR[sample(nrow(AR), 10000), ]# Randomly sample 10,000 rows
                     
#Test assumptions of linear regression (using a subset of the data):
model <- lm(totRichness ~ log(GIS_T_AREA), data=subset_AR)   
model_1 <- lm(totRichness ~ log(GIS_T_AREA)*STATUS_YR + AvgLat, data=subset_AR)                   
plot(model)
plot(model_1)                    
                  
# Try Poisson family GLM (since species richness is count data)                    
model <- glm(totRichness ~ log(GIS_T_AREA), data = subset_AR, family = poisson)
model_1 <- glm(totRichness ~ log(GIS_T_AREA)*STATUS_YR + AvgLat, data = subset_AR, family = poisson)                                       
# explore deviance residulas, which should follow roughly normal distribution:
plot(model, which = 1)    # No, does not look normal
# ratio of the deviance to its degrees of freedom can be compared to a chi-square distribution. In a well-specified model, this ratio should be close to 1.
pchisq(model$deviance, df = model$df.residual, lower.tail = FALSE)     # No, this in fact is close to 0  
plot(model_1, which = 1)
pchisq(model_1$deviance, df = model_1$df.residual, lower.tail = FALSE)                       
                     
# Since we've got overdispersion, let's do negative binomial instead                     
model <- glm.nb(totRichness ~ log(GIS_T_AREA), data = subset_AR)
model_1 <- glm.nb(totRichness ~ log(GIS_T_AREA)*STATUS_YR + AvgLat, data = subset_AR)                     
plot(model, which = 1)    
pchisq(model$deviance, df = model$df.residual, lower.tail = FALSE)     # 
plot(model_1, which = 1)    
pchisq(model_1$deviance, df = model_1$df.residual, lower.tail = FALSE)                        
# Definitely the closest to a fit that we've gotten.  
                     
# Let's compare full models' AIC
AR <- AR[!(AR$IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported")), ]

# Full models of interest
# We imagine area and year might interact, in that pehaps the differences between big and small PAs only emerge over time                     
null <-  glm.nb(totRichness ~ 1, data = AR)                    
model <- glm.nb(totRichness ~ AvgLat, data = AR)                     
model_0 <- glm.nb(totRichness ~ log(GIS_T_AREA), data = AR)
model_1 <- glm.nb(totRichness ~ log(GIS_T_AREA) + AvgLat, data = AR)   
model_2 <- glm.nb(totRichness ~ log(GIS_T_AREA) + STATUS_YR, data = AR)   
model_3 <- glm.nb(totRichness ~ log(GIS_T_AREA):STATUS_YR, data = AR) 
model_4 <- glm.nb(totRichness ~ log(GIS_T_AREA):STATUS_YR + STATUS_YR, data = AR) 
model_5 <- glm.nb(totRichness ~ log(GIS_T_AREA):STATUS_YR + log(GIS_T_AREA), data = AR) 
model_6 <- glm.nb(totRichness ~ log(GIS_T_AREA)*STATUS_YR, data = AR)                      
model_7 <- glm.nb(totRichness ~ log(GIS_T_AREA):STATUS_YR + AvgLat, data = AR)        
model_8 <- glm.nb(totRichness ~ log(GIS_T_AREA):STATUS_YR + AvgLat + IUCN_CAT, data = AR) 
model_9 <- glm.nb(totRichness ~ log(GIS_T_AREA)*STATUS_YR + AvgLat + IUCN_CAT, data = AR)                      

# model comparison by AIC                     
AIC_values <- c(AIC(null), AIC(model), AIC(model_0), AIC(model_1), AIC(model_2), AIC(model_3), AIC(model_4), AIC(model_5), AIC(model_6), AIC(model_7), AIC(model_8), AIC(model_9))
print(AIC_values)                

# Examine top 3 models by AIC                     
plot(model_9, which = 1)    
pchisq(model_9$deviance, df = model_9$df.residual, lower.tail = FALSE)    
summary(model_9)
pR2(model_9)
                                                         
plot(model_8, which = 1)    
pchisq(model_8$deviance, df = model_8$df.residual, lower.tail = FALSE)    
summary(model_8)
pR2(model_8)            
                                                         
plot(model_7, which = 1)    
pchisq(model_8$deviance, df = model_8$df.residual, lower.tail = FALSE)    
summary(model_8)
pR2(model_8)                     
