# R language

# Analyses of how proportion threatendness of PA is associated with its characteristics, and plots examining 
#             the real accumulation of amphibians on existing PA network over time as well as relationship
#             between PA age and its proportion of threatened species

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
library(forcats)
library(khroma)
muted <- colour("muted")
library(lessR)
library(data.table) #yes
library(scales)
library(pscl)

#Load all species found in all PAs
SpPA <- read.csv("Data using Amphibian Ranges for Species Richness/All_Amphibia with WDPA one to many.csv")
#total count of amphibians for which we have range information
Amph <- read.csv("Data using Amphibian Ranges for Species Richness/Amphibians_w_ranges.csv")
length(unique(Amph$binomial))   #7094 species

#Here's the threat data. let's clean itup.
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

# Join the data together
SpPA <- left_join(Amph, SpPA, by="binomial")

#Label amphibians not on PAs as "not protected"
SpPA$WDPA_PID[SpPA$WDPA_PID == ""] <- "NotProtected"

#Calculate the total number of amphibians associated with each PA. 
SpPaSum <- SpPA %>%
  dplyr::group_by(WDPA_PID) %>%
  dplyr::summarise(totRichness = length(unique(binomial))) 

# Summarize counts of threatened species per PA
ThreatSum <- SpPA %>%
  dplyr::group_by(WDPA_PID, AWiucn) %>%
  dplyr::summarise(count = length(unique(binomial)))  
ThreatSum<-as.data.frame(ThreatSum)
#Label the catch all for amphibians not on PAs
ThreatSum$WDPA_PID[ThreatSum$WDPA_PID == ""] <- "NotProtected"
#Make this horizontal format
ThreatSum <- ThreatSum %>% spread(AWiucn, count)
SpPaSum <- left_join(SpPaSum, ThreatSum, by=c("WDPA_PID"="WDPA_PID"))

#Calculate total number threatened species
SpPaSum$AllThreat <- rowSums(SpPaSum[, c('CR', 'EN', 'VU','EW', 'EX')], na.rm=TRUE) 
#Total species assessed (not DD or in assessment
SpPaSum$AllAssessed <- rowSums(SpPaSum[, c('CR', 'EN', 'NT', 'VU', 'LC')], na.rm=TRUE) 
#Total proportion threatened species
SpPaSum$ThreatProp <- SpPaSum$AllThreat/SpPaSum$AllAssessed

# Import the country and region info per PA
xtra <- read.csv("World_Database_Protected_Areas/WDPA_all.csv")
names(xtra)[names(xtra) == "NAME_EN"] <- "Country"
names(xtra)[names(xtra) == "ï..WDPA_PID"] <- "WDPA_PID"
xtra <- xtra[ , which(names(xtra) %in% c("WDPA_PID", "CONTINENT", "Country"))]
xtra <- unique(xtra)

# Bring in PA data for area and establishment year
wdpa <- read.csv("World_Database_Protected_Areas/WDPA_WDOECM_Jul2021_Public_all_csv/WDPA_WDOECM_Jul2021_Public_all_csv.csv")
#We need to take out marine area from what we're considering as protected area size!
wdpa$GIS_T_AREA <-wdpa$GIS_AREA - wdpa$GIS_M_AREA
#Make status year numeric for following analysis
wdpa$STATUS_YR <- as.numeric(wdpa$STATUS_YR)
#Remove blank rows. 
wdpa <- wdpa[rowSums(is.na(wdpa)) != ncol(wdpa),]

# merge data
#THIS IS ALL WDPAs WHETHER THEY HAVE AMPHIBIANS OR NOT! 
SpPaSum <- left_join(SpPaSum, wdpa, by=c("WDPA_PID"="WDPA_PID"))
SpPaSum <- left_join(SpPaSum, xtra, by=c("WDPA_PID"="WDPA_PID"))
#Remove extra columns
torem <- c("ï..TYPE", "WDPAID", "PA_DEF", "DESIG", "DESIG_ENG", "DESIG_TYPE", "INT_CRIT", "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TAKE", "NO_TK_AREA", "STATUS", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "VERIF", "METADATAID", "SUB_LOC", "PARENT_ISO3", "ISO3", "SUPP_INFO", "CONS_OBJ", "all")           
SpPaSum <- SpPaSum[ , -which(names(SpPaSum) %in% torem)]
           
# We decided to remove PAs where amphibians don't occur from our analyses
RHT_WDPA <- SpPaSum[SpPaSum$totRichness > 0,]
#Remove blank rows
RHT_WDPA <- RHT_WDPA[rowSums(is.na(RHT_WDPA)) != ncol(RHT_WDPA),]

#Get rid of everything we don't need, because it burdens the system
remove(xtra, wdpa, torem, SpPaSum, threatdata, ThreatSum)

# replace ambiguous region names we had established
RHT_WDPA <- as.data.frame(lapply(RHT_WDPA ,function(x) if(is.character(x)|is.factor(x)) gsub("Oceania","Australia and Oceania", x) else x))
RHT_WDPA <- as.data.frame(lapply(RHT_WDPA ,function(x) if(is.character(x)|is.factor(x)) gsub("North America","North and Central America", x) else x))

# Remove records that do not seem to correspond to established protected areas, including those with their area name 
# recorded as “area not protected” (2599 polygons), or their status recorded as “degazetted,”“proposed,”“recommended,”
# “in preparation,” or “unset” (1024 polygons and 1751 points). Check for them by manually revising the search results of
# the following
RHT_WDPA %>% filter_all(any_vars(grepl("not protected|degazetted|proposed|recommended|in preparation|unset",.)))
# Filter out PID 555564362 and 305921, which are both "proposed" rather than created it looks like  
RHT_WDPA <- RHT_WDPA[!(RHT_WDPA$WDPA_PID=="555564362" | RHT_WDPA$WDPA_PID=="305921"),]
# Dataframe shrinks from 137,660 to 137,658;   so this worked as expected!!                                 

# Create a dataset for plotting our existing network's accumulation of amphibians over time                                 
#Get rid of any NA values in columns of interest
yr <- RHT_WDPA[!is.na(RHT_WDPA$STATUS_YR), ]
yr <- yr[yr$STATUS_YR != 0, ]
yr <- yr[!is.na(yr$STATUS_YR),]  
yr <- yr[!is.na(yr$CONTINENT),]  
yr <- yr[!duplicated(yr$WDPA_PID),]

# Create a dataset for only PAs where we can also calculate the proportion threatenedess of amphibians
completeFun <- function(data, desiredCols) {
   completeVec <- complete.cases(data[, desiredCols])
   return(data[completeVec, ])}                   
yrrr <- completeFun(yr, c("AllThreat", "totRichness"))  

#############################################################
# Test of how PA characteristics are associated with PA proportion threatenedness   

# Bring in the centroid data                                 
directory_path <- "G:/.shortcut-targets-by-id/1zvJrRR38XEN9N1yR69JmJj_84BhVKKec/AW_ConservationWG/WDPA Shapefiles with Country Name/"
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
combined_data$AvgLat<-abs(round(combined_data$AvgLat, 2))  
combined_data <- na.omit(combined_data)
# merge as data.tables                     
setDT(yrrr)
setDT(combined_data)                    
yrrr <- merge(yrrr, combined_data, by.y = "Name", by.x = "WDPA_PID", all.x = TRUE)

# Remove ambiguous IUCN PA categories                                 
yrrr <- yrrr[!(yrrr$IUCN_CAT %in% c("Not Applicable", "Not Assigned", "Not Reported")), ]
                                 
set.seed(123)  # Set seed for reproducibility
subset_yrrr <- yrrr[sample(nrow(yrrr), 10000), ]# Randomly sample 10,000 rows
                                 
#Test assumptions of linear regression (using a subset of the data):
model <- lm(propThreat ~ log(GIS_T_AREA), data=subset_yrrr)   
model_1 <- lm(propThreat ~ log(GIS_T_AREA)*STATUS_YR + AvgLat, data=subset_yrrr)                   
plot(model)
plot(model_1)                    
# assumptions violated

# Investigate appropriateness of a binomial GLM                                 
model <- glm(propThreat ~ log(GIS_T_AREA), family = binomial, data = subset_yrrr)
model_1 <- glm(propThreat ~ log(GIS_T_AREA)*STATUS_YR, family = binomial, data = subset_yrrr)
plot(model, which = 1)    
pchisq(model$deviance, df = model$df.residual, lower.tail = FALSE)     
plot(model_1, which = 1)    
pchisq(model_1$deviance, df = model_1$df.residual, lower.tail = FALSE)    

# Now build full models 
null <- glm(propThreat ~ 1, family = binomial, data = yrrr)
model <- glm(propThreat ~ STATUS_YR, family = binomial, data = yrrr)    
model_0 <- glm(propThreat ~ IUCN_CAT, family = binomial, data = yrrr)  
model_1 <- glm(propThreat ~ AvgLat, family = binomial, data = yrrr)                                     
model_2 <- glm(propThreat ~ log(GIS_T_AREA), family = binomial, data = yrrr)                                  
model_3 <- glm(propThreat ~ log(GIS_T_AREA)*STATUS_YR, family = binomial, data = yrrr) 
model_4 <- glm(propThreat ~ AvgLat + log(GIS_T_AREA), family = binomial, data = yrrr)                                 
model_5 <- glm(propThreat ~ AvgLat*log(GIS_T_AREA), family = binomial, data = yrrr)   
model_6 <- glm(propThreat ~ AvgLat:log(GIS_T_AREA) + AvgLat, family = binomial, data = yrrr) 
model_7 <- glm(propThreat ~ AvgLat:log(GIS_T_AREA) + log(GIS_T_AREA), family = binomial, data = yrrr) 
model_8 <- glm(propThreat ~ AvgLat:log(GIS_T_AREA) + IUCN_CAT, family = binomial, data = yrrr)                                 
model_9 <- glm(propThreat ~ AvgLat:log(GIS_T_AREA), family = binomial, data = yrrr)   
model_10 <- glm(propThreat ~ AvgLat*log(GIS_T_AREA) + IUCN_CAT, family = binomial, data = yrrr)  
model_11 <- glm(propThreat ~ AvgLat*log(GIS_T_AREA) + STATUS_YR, family = binomial, data = yrrr)                                   

# Compare model AICs                                 
AIC_values <- c(AIC(null), AIC(model), AIC(model_0), AIC(model_1), AIC(model_2), AIC(model_3), AIC(model_4), AIC(model_5), AIC(model_6), AIC(model_7), AIC(model_8), AIC(model_9), AIC(model_10), AIC(model_11))
print(AIC_values) 

# Gather statistics for top three models by AIC                                 
summary(model_6) 
pR2(model_6)        
summary(model_1) 
pR2(model_1)                                 
summary(model_4) 
pR2(model_4)                                  
 
#############################################################
# Plot species accumulation in existing PA network and relationship between PA age and proportion threatenedness 
                                
# Because tiny species richness end up creating strange horizontal lines on following plot with small numbers, 
#      use cut-off of 3 amphibian species for richness. 
yrrr <- yrrr[yrrr$totRichness > 3, ]
yrrr$propThreat <- yrrr$AllThreat/yrrr$totRichness
yrrr <- yrrr[yrrr$propThreat < 1.01, ]

# Add on cumulative species protected until a particular date...                                  
# 5 yr bins, 1875-2020
# So I need to add on status year to SpPA
spyr<-merge(SpPA[SpPA$WDPA_PID!="NotProtected",], RHT_WDPA, by="WDPA_PID")                                 
cumulative<-data.frame(matrix(ncol=2, nrow=161))
colnames(cumulative) <- c('Year', 'Tally')
cumulative$Year<-seq(1860, 2020, by=1) 
for (ROW in 1:nrow(cumulative)){
  year<-cumulative$Year[ROW]
  total<-length(unique(spyr$binomial[spyr$STATUS_YR<=year]))
  cumulative$Tally[ROW]<-total
  micro_total<-length(unique(spyr$binomial[spyr$STATUS_YR<=year & spyr$GIS_T_AREA<10]))
  cumulative$cum_micro[ROW]<-micro_total
  }       
                                 
# scientific format for axes                      
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}
                                 
# Make sure to filter by only tiny PAs (<10km2) !
B<- ggplot(data=yrrr[yrrr$GIS_T_AREA<10,], aes(x=STATUS_YR, y=propThreat)) +
    stat_binhex(aes(fill=..count..), bins=50, colour="black", size=0.01) + 
    scale_fill_gradientn(trans = log10_trans(),
                      colours = c("#117733", "#DDCC77", "#CC6677"), 
                      values= c(0, 0.45, 0.55, 1) #,
                     ) +                                 
 labs(y = "Proportion amphibian threatenedness", x = "") + #, fill="Count of\nProtected\nAreas"
    scale_x_continuous(breaks=seq(1860, 2020, by = 20), labels=seq(1860, 2020, by = 20), limits=c(1860,2020), expand = c(0, 0)) +                          
    scale_y_continuous(expand = c(0.008, 0.008)) +      
  guides(fill = FALSE) +                                 
theme(
  text = element_text(size = 40, family = "Calibri"),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  panel.border = element_rect(fill = NA, color = "white"),
  panel.background = element_rect(fill = "white", colour = "grey50"),
  plot.margin = margin(r = .8, b = 0.1, l = .5, t=1, unit = "cm"),
  axis.text.y = element_text(angle = 45, vjust = 1, hjust = -0.2))
                      
A <-  ggplot()+
  geom_bar(data=yr, aes(x=STATUS_YR), stat="bin", binwidth = 5, color = "black", fill = "#117733", alpha=0.5)+
  geom_bar(data=yr[yr$GIS_T_AREA<10,], aes(x=STATUS_YR), stat="bin", binwidth = 5, color = "black", fill = "black")+
  geom_point(aes(x=c(2015, 2015), y=c(7094*5, 8489*5)), pch=8, stroke=2, col="#7c5295", size=2)+                    
  geom_line(data=cumulative, aes(x=Year, y=Tally*5), lwd=2, color="#7c5295")+   
  geom_line(data=cumulative, aes(x=Year, y=cum_micro*5), lwd=2, color="#7c5295", linetype=3)+                                
  labs(x = "Date of protected area declaration") +
  scale_x_continuous(breaks=seq(1860, 2020, by = 20), labels=seq(1860, 2020, by = 20), limits=c(1860,2020), expand = c(0.05, 0.005)) +
  scale_y_continuous(labels = scientific_10, name="Number of protected areas", sec.axis = sec_axis(~.*0.15, labels = scientific_10, name="Number of amphibian species"), expand=c(0.008,0.008)) +                                
  theme(text=element_text(size=40, family="Calibri"),      
        axis.text.x = element_text(angle=45, vjust=1.1, hjust=1.1), 
        axis.text.y = element_text(angle=45, color="#117733"), 
        axis.text.y.right = element_text(color="#7c5295"), 
        axis.title.y = element_text(color="#117733"), 
        axis.title.y.right = element_text(color="#7c5295"),
        plot.margin = margin(t=1.7, unit = "cm"),
        panel.border = element_rect(fill = NA, color = "white"),
        panel.background = element_rect(fill = "white", colour = "grey50"))                                                                               
                      
jpeg("../Figures/PAyear_test_V3.jpg", width = 2200, height = 1000, res=100, quality = 100)   
plot_grid(A,B, rel_widths = c(1.2, 1))                                 
dev.off()                                                                                     
 
