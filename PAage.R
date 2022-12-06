#Exploratory Analyses of AWeb Conservation Working Group Data                                                                                                               #
#author: Emma Steigerwald                                                                                                                                                   #
#############################################################################################################################################################################

#Set up working environment
setwd("G:/Shared drives/AW-Data/ConservationWG/ConservationWG_GithubRepo/Data")
#Load libraries
library(dplyr) #yes
library(ggplot2) #yes
library(plyr) #yes
library(hexbin)
library(RColorBrewer)
library(stringr) #yes
library(cowplot) #NO
library(SciViews)
library(tidyr) #yes
library(forcats)
library(khroma) #Maybe use https://www.rdocumentation.org/packages/ggsci/versions/2.9 instead?
muted <- colour("muted")
library(lessR)
library(data.table) #yes
library(scales)
library(extrafont)
font_import()

#Plotting params:
#   PA stats (area or count): #117733
#   Threatened: #CC6677
#   Richness: #332288

#####################
#Load all species found in all PAs
SpPA <- read.csv("Data using Amphibian Ranges for Species Richness/All_Amphibia with WDPA one to many.csv")
#total count of amphibians for which we have range information
Amph <- read.csv("Data using Amphibian Ranges for Species Richness/Amphibians_w_ranges.csv")
range_total<-length(unique(Amph$binomial)) 

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

SpPA <- left_join(Amph, SpPA, by="binomial")


#Label amphibians not on PAs as "not protected"
SpPA$WDPA_PID[SpPA$WDPA_PID == ""] <- "NotProtected"
#Calculate the total number of amphibians associated with each PA. 
SpPaSum <- SpPA %>%
  dplyr::group_by(WDPA_PID) %>%
  dplyr::summarise(totRichness = length(unique(binomial))) 

xtra <- read.csv("All_Amphibia_Country_Hemi2.csv")
xtra <- xtra[ , which(names(xtra) %in% c("binomial", "CONTINENT"))]

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
            

#####################
#Finally, import the country, continent, hemisphere info
xtra <- read.csv("World_Database_Protected_Areas/WDPA_all.csv")
names(xtra)[names(xtra) == "NAME_EN"] <- "Country"
names(xtra)[names(xtra) == "ï..WDPA_PID"] <- "WDPA_PID"
xtra <- xtra[ , which(names(xtra) %in% c("WDPA_PID", "CONTINENT", "Country"))]
xtra <- unique(xtra)

#Now bring in the WDPA data from 
###WE STILL NEED TO FIX ENCODING ISSUES HERE!!!
wdpa <- read.csv("World_Database_Protected_Areas/WDPA_WDOECM_Jul2021_Public_all_csv/WDPA_WDOECM_Jul2021_Public_all_csv.csv")
#We need to take out marine area from what we're considering as protected area size!
wdpa$GIS_T_AREA <-wdpa$GIS_AREA - wdpa$GIS_M_AREA
#Make status year numeric for following analysis
wdpa$STATUS_YR <- as.numeric(wdpa$STATUS_YR)
#Remove blank rows. 
wdpa <- wdpa[rowSums(is.na(wdpa)) != ncol(wdpa),]


#Now, merge...
#THIS IS ALL WDPAs WHETHER THEY HAVE AMPHIBIANS OR NOT! 
SpPaSum <- left_join(SpPaSum, wdpa, by=c("WDPA_PID"="WDPA_PID"))
SpPaSum <- left_join(SpPaSum, xtra, by=c("WDPA_PID"="WDPA_PID"))
#Remove extra columns
torem <- c("ï..TYPE", "WDPAID", "PA_DEF", "DESIG", "DESIG_ENG", "DESIG_TYPE", "INT_CRIT", "MARINE", "REP_M_AREA", "GIS_M_AREA", "REP_AREA", "GIS_AREA", "NO_TAKE", "NO_TK_AREA", "STATUS", "GOV_TYPE", "OWN_TYPE", "MANG_AUTH", "MANG_PLAN", "VERIF", "METADATAID", "SUB_LOC", "PARENT_ISO3", "ISO3", "SUPP_INFO", "CONS_OBJ", "all")           
SpPaSum <- SpPaSum[ , -which(names(SpPaSum) %in% torem)]
           
#We decided to remove PAs where amphibians don't occur from our analyses
RHT_WDPA <- SpPaSum[SpPaSum$totRichness > 0,]
#Remove blank rows
RHT_WDPA <- RHT_WDPA[rowSums(is.na(RHT_WDPA)) != ncol(RHT_WDPA),]

#Get rid of everything we don't need, because it burdens the system
remove(xtra, wdpa, torem, SpPaSum, threatdata, ThreatSum)

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
#Dataframe shrinks from 137,660 to 137,658;   so this worked as expected!!  

                   
#####################################################################################
#First, create data for declaraction year plot                                
                                 
#Get rid of any NA values in columns of interest
yr <- RHT_WDPA[!is.na(RHT_WDPA$STATUS_YR), ]
yr <- yr[yr$STATUS_YR != 0, ]
yr <- yr[!is.na(yr$STATUS_YR),]  
yr <- yr[!is.na(yr$CONTINENT),]  
yr <- yr[!duplicated(yr$WDPA_PID),]

#Next, how PA age relates to proportion threatened species in these PAs
completeFun <- function(data, desiredCols) {
   completeVec <- complete.cases(data[, desiredCols])
   return(data[completeVec, ])}                   
yrrr <- completeFun(yr, c("AllThreat", "totRichness")) 
#Because the proportions will result in strangle horizontal lines on following plot with small numbers, use cut-off of 3 for richness. 
yrrr <- yrrr[yrrr$totRichness > 3, ]
yrrr$propThreat <- yrrr$AllThreat/yrrr$totRichness
yrrr <- yrrr[yrrr$propThreat < 1.01, ]
                                 
#If I wanted to add on cumulative species protected until a particular date...                                  
#5 yr bins, 1875-2020
#So I need to add on status year to SpPA
spyr<-merge(SpPA[SpPA$WDPA_PID!="NotProtected",], RHT_WDPA, by="WDPA_PID")                                 
cumulative<-data.frame(matrix(ncol=2, nrow=161))
colnames(cumulative) <- c('Year', 'Tally')
cumulative$Year<-seq(1860, 2020, by=1) 
for (ROW in 1:nrow(cumulative)){
  year<-cumulative$Year[ROW]
  total<-length(unique(spyr$binomial[spyr$STATUS_YR<=year]))
  cumulative$Tally[ROW]<-total
  }
                                 

#Here, I want to create a line representing median PA threatenedness per decade                                 
yr_avg <- yrrr[yrrr$GIS_T_AREA<10,] %>%
  dplyr::group_by(group = STATUS_YR, CONTINENT) %>%
  dplyr::summarise(Avg = mean(propThreat), Med = median(propThreat), count=n())                                 
                                 
                      
#Here, I'm going to look at the same plot but filtering by only tiny PAs (<1km2)
A<- ggplot(data=yrrr[yrrr$GIS_T_AREA<10,], aes(x=STATUS_YR, y=propThreat)) +
    #stat_binhex(aes(fill=log(..count..)), bins=50, colour="black", size=0.01) +
    stat_binhex(aes(fill=..count..), bins=50, colour="black", size=0.01) + 
    #geom_line(data=yr_avg, aes(x=group, y=count), lwd=2, color="black")+           #, linetype=4    
    #geom_smooth(method="lm", formula=y~x, color="black", size=2, linetype=4)+                             
    #scale_fill_gradientn(colours = c("#117733", "#DDCC77", "#CC6677"), values= c(0, 0.45, 0.55, 1), breaks = c(log(5), log(20), log(100), log(1000), log(5000), log(20000)), labels = c(5, 20, 100, 1000, 5000, 20000)) +  
 scale_fill_gradientn(trans = log10_trans(),
                      colours = c("#117733", "#DDCC77", "#CC6677"), 
                      values= c(0, 0.45, 0.55, 1), 
                      breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x))) +                                 
 labs(title="A.", y = "Proportion amphibian threatenedness", x = "", fill="Count of\nProtected\nAreas") +
    scale_x_continuous(breaks=seq(1860, 2020, by = 20), labels=seq(1860, 2020, by = 20), limits=c(1860,2020), expand = c(0.008, 0.008)) +                          
  theme(text=element_text(size=30, family="Calibri"),
        legend.title=element_text(size=25, family="Calibri"),
        axis.text.x = element_text(angle=45, vjust=0.5), 
        legend.key.height = unit(2, "cm"), 
        legend.key.width = unit(1,"cm"), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.margin=margin(r=2.6, b=1.2, unit="cm"))                          
                      
                      

#Here's the code I use to pull things out to annotate! (modified each time)                     
yrrr[yrrr$STATUS_YR>1880 & yrrr$STATUS_YR<1900 & yrrr$propThreat<.05 & yrrr$GIS_T_AREA<10,c(2, 14, 15, 18, 19, 21, 22)]                      
                      
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

                      
yrrr$bin <- findInterval(yrrr$STATUS_YR, seq(1800, 2020, 5))    
yrrr$yr_bin <- 1800+(yrrr$bin-1)*5     
                      
#Add a categorical column classifying into different size bins
yrrrr<-yrrr[yrrr$GIS_T_AREA>=1 & yrrr$GIS_T_AREA<100000,]
yrrrr$size_bin[yrrrr$GIS_T_AREA>=1 & yrrrr$GIS_T_AREA<10] <-   "tiny"                    
yrrrr$size_bin[yrrrr$GIS_T_AREA>=10 & yrrrr$GIS_T_AREA<100] <-   "small"                          
yrrrr$size_bin[yrrrr$GIS_T_AREA>=100 & yrrrr$GIS_T_AREA<1000] <-   "medium"                          
yrrrr$size_bin[yrrrr$GIS_T_AREA>=1000 & yrrrr$GIS_T_AREA<10000] <-   "large"                          
yrrrr$size_bin[yrrrr$GIS_T_AREA>=10000 & yrrrr$GIS_T_AREA<100000] <-   "huge"                          

line <- yrrrr %>%
  dplyr::group_by(yr_bin, size_bin) %>%
  dplyr::summarise(Avg = mean(propThreat), Med = median(propThreat), count=n())  

lbs = setNames(c("'1 - 10 '*km^2*''", "'10 - 100 '*km^2*''", "'100 - 1,000 '*km^2*''", "'1,000 - 10,000 '*km^2*''","'10,000 - 100,000 '*km^2*''"),
               c("tiny", "small", "medium", "large","huge"))[levels(factor(line$size_bin))]                      
                      
cols<-c("tiny"="deeppink3", "small"="#CC6677", "medium"="#999933", "large"="#AA4499","huge"="#44AA99")      
 
#set the order!!
line$size_bin <- factor(line$size_bin, levels = c("tiny", "small", "medium", "Asia", "large", "huge"))  
                      
                      
B <-  ggplot(yr, aes(x=STATUS_YR))+
  geom_bar(stat="bin", binwidth = 1, color = "black", fill = "white", size=0.1)+
  geom_density(stat="bin", binwidth = 5, color = "black", fill = "#117733", alpha=0.5)+
  geom_line(data=cumulative, aes(x=Year, y=Tally*6.67), lwd=2, color="#332288", alpha=0.8)+   
  geom_hline(yintercept=range_total*6.67, linetype=2, lwd=2, color="#332288")+ 
  #geom_text(label="Total amphibians for which range information exists", aes(x=1940, y=range_total*6.67+900), size=7, color="#332288")+                               
  labs(title="B.", x = "Date of protected area declaration") +
  scale_x_continuous(breaks=seq(1860, 2020, by = 20), labels=seq(1860, 2020, by = 20), limits=c(1860,2020), expand = c(0.008, 0.008)) +
  #scale_y_continuous(breaks=breaks(function(x) 10^x), labels=format(math_format(10^.x)), sec.axis = sec_axis(~.*0.15))+   #, breaks = seq(0, 6000, by=1000)
  scale_y_continuous(labels = scientific_10, name="Number of protected areas", sec.axis = sec_axis(~.*0.15, labels = scientific_10, name="Number of amphibian species")) +                                
  theme(text=element_text(size=30, family="Calibri"),      
        axis.text.x = element_text(angle=45, vjust=1.1, hjust=1.1), 
        axis.text.y = element_text(angle=45, color="#117733"), 
        axis.text.y.right = element_text(color="#332288"), 
        axis.title.y = element_text(color="#117733"), 
        axis.title.y.right = element_text(color="#332288"), 
        panel.background = element_rect(fill = "white", colour = "grey50"))                                                              
                 
                 
                      
jpeg("../Figures/PAyear_test.jpg", width = 2200, height = 1000, res=100, quality = 100)   
plot_grid(A, B)                                 
dev.off()                       
                                      
 #set the order!!
yrrrr$size_bin <- factor(yrrrr$size_bin, levels = c("tiny", "small", "medium", "Asia", "large", "huge"))                       
jpeg("../Figures/PAyear_point.jpg", width = 1000, height = 1000, res=100, quality = 100)                        
ggplot(data=yrrrr, aes(x=STATUS_YR, y=propThreat)) +
    geom_jitter(aes(col=size_bin), cex=1) + 
    facet_wrap(~size_bin, nrow=5, ncol=1, labeller=as_labeller(lbs, label_parsed)) +  
    scale_colour_manual(values=cols) +               
 labs(title="", y = "Proportion amphibian threatenedness", x = "") +
    scale_x_continuous(breaks=seq(1860, 2020, by = 20), labels=seq(1860, 2020, by = 20), limits=c(1865,2020), expand = c(0.008, 0.008)) +                          
    scale_y_continuous(limits=c(0,0.21), breaks=seq(0, 0.20, 0.10), expand = c(0.008, 0.008)) +  
  guides(color="none")+                    
  theme(text=element_text(size=30, family="Calibri"),      
        axis.text.x = element_text(angle=45, vjust=0.5), 
        strip.text = element_text(size=20),
        strip.text.x = element_text(margin = margin(0, 0, 0, 0, "cm")),
        legend.key.height = unit(3, "cm"), 
        legend.key.width = unit(1.5,"cm"), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.margin=margin(r=2.6, b=1.2, unit="cm"))                                                                                   
dev.off()                        
                                     
jpeg("../Figures/PAyear_DEBT_1.jpg", width = 1000, height = 1000, res=100, quality = 100)                        
ggplot(data=line, aes(x=yr_bin, y=Avg, fill=factor(size_bin)), alpha=0.5) +
    geom_bar(stat="identity", col="black", binwidth=5)+              
    facet_wrap(~size_bin, nrow=5, ncol=1, labeller=as_labeller(lbs, label_parsed)) +  
    scale_colour_manual(values=cols) +               
 labs(title="", y = "", x = "") +
    scale_x_continuous(breaks=seq(1860, 2020, by = 20), labels=seq(1860, 2020, by = 20), limits=c(1865,2020), expand = c(0.008, 0.008)) +                          
    scale_y_continuous(limits=c(0,0.21), breaks=seq(0, 0.20, 0.10), expand = c(0.008, 0.008)) +  
  guides(fill="none")+                    
  theme(text=element_text(size=30, family="Calibri"),      
        axis.text.x = element_text(angle=45, vjust=0.5), 
        strip.text = element_text(size=20),
        strip.text.x = element_text(margin = margin(0, 0, 0, 0, "cm")),
        legend.key.height = unit(3, "cm"), 
        legend.key.width = unit(1.5,"cm"), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.margin=margin(r=2.6, b=1.2, unit="cm"))                                                                                   
dev.off()                       
 
