#Size_Richness.R                                                                                                                                                            #
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
library(ggpattern)
library(forcats)
library(khroma) #Maybe use https://www.rdocumentation.org/packages/ggsci/versions/2.9 instead?
muted <- colour("muted")
library(lessR)
library(data.table) #yes
library(scales)
library(extrafont)
font_import()

#####################
#Load all species found in all PAs
SpPA <- read.csv("Data using Amphibian Ranges for Species Richness/All_Amphibia with WDPA one to many.csv")

#Associate a threat level with these amphibians
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
SpPaSum <- SpPA %>%
  dplyr::group_by(WDPA_PID) %>%
  dplyr::summarise(totRichness = length(unique(binomial))) 

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
#Total species assessed (not DD or in assessment
SpPaSum$AllAssessed <- rowSums(SpPaSum[, c('CR', 'EN', 'EW', 'EX', 'NT', 'VU', 'LC')], na.rm=TRUE) 
#Total proportion threatened species
SpPaSum$ThreatProp <- SpPaSum$AllThreat/SpPaSum$AllAssessed

#SAVE SpPA for the cumulative amphibian species covered, below!!!
#Now we've gotten what we want fromt his crazy large table, remove it.
remove(SpPA)                 

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
remove(xtra, wdpa, torem, SpPaSum, threatdata, ThreatSum, gdp)

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
#ALL QUESTIONS HAVING TO DO WITH PA AREA
#####################################################################################

#Get rid of any NA values in columns of interest
AR <- RHT_WDPA[RHT_WDPA$GIS_T_AREA != 0, ]
AR <- AR[!is.na(AR$GIS_T_AREA), ]

################
# Histogram of PA area

#Hexbin of the same image
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
                   
#Display this as a regression?
jpeg("../Figures/PAsize-SpeciesRichness_Regression.jpg", width = 2000, height = 2000, res=100, quality = 100)
ggplot(data=AR, aes(x=GIS_T_AREA, y=totRichness, width=1)) +
  geom_point(size=0.00000000000001)+
  geom_smooth(method='lm', color="black", lw=2, fill="grey") +                 
  labs(title="How PA size relates to species richness", y = "Species richness", x = "PA size (km^2)") +
  theme(axis.text.x = element_text(angle=45, size=45, vjust=0.5), axis.text.y = element_text(size=45), axis.title=element_text(size=45), 
        plot.title=element_blank(), legend.text=element_text(size=40), legend.title=element_text(size=45), legend.key.height = unit(3, "cm"), 
        legend.key.width = unit(1.5,"cm"), panel.background = element_rect(fill = "white", colour = "grey50"), strip.text = element_text(size=45)) +
  scale_x_continuous(limits = c(0, 10000), expand = c(0.008, 0.008), breaks=seq(0, 10000, by=1000), labels=seq(0, 10000, by=1000)) + 
  scale_y_continuous(limits = c(0,180), expand = c(0.008, 0.008))
dev.off()  

                     
#Test assumptions of linear regression:

model <- lm(totRichness ~ log(GIS_T_AREA), data=AR)   
model_1 <- lm(totRichness ~ log(GIS_T_AREA) + CONTINENT, data=AR)
                     
#   Coefficients:
# (Intercept)   GIS_T_AREA  
# 11.9817774    0.0009729     

library(broom)                     
model.diag.metrics <- augment(model)
jpeg("../Figures/model.jpg")
par(mfrow = c(2, 2))
plot(model)
dev.off()
                     
jpeg("../Figures/model_1.jpg")
par(mfrow = c(2, 2))
plot(model_1)
dev.off()
                     
#The diagnostic plots show residuals in four different ways:

#(1) Residuals vs Fitted. Used to check the linear relationship assumptions. A horizontal line, without distinct patterns is an indication for a linear relationship, what is good.
#violated
#(2) Normal Q-Q. Used to examine whether the residuals are normally distributed. It’s good if residuals points follow the straight dashed line.
#violated
#(3) Scale-Location (or Spread-Location). Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity. This is not the case in our example, where we have a heteroscedasticity problem.
#violated
#(4) Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis. This plot will be described further in the next sections.                     
#violated 
                     
summary(model)
#Call:
#lm(formula = totRichness ~ log(GIS_T_AREA), data = AR)
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-21.979  -6.228  -2.373   3.435 187.603 
#
#Coefficients:
#                 Estimate Std. Error t value            Pr(>|t|)    
#(Intercept)     12.616364   0.022415   562.8 <0.0000000000000002 ***
#log(GIS_T_AREA)  0.917902   0.006875   133.5 <0.0000000000000002 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                     
#Residual standard error: 10.94 on 245356 degrees of freedom
#Multiple R-squared:  0.06773,	Adjusted R-squared:  0.06772 
#F-statistic: 1.782e+04 on 1 and 245356 DF,  p-value: < 0.00000000000000022                     
                     
summary(model_1)
#Call:
#lm(formula = totRichness ~ log(GIS_T_AREA) + CONTINENT, data = AR)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-48.075  -4.426  -1.249   4.217 176.463 

#Coefficients:
                                                      Estimate Std. Error  t value            Pr(>|t|)    
#(Intercept)                                          27.998320   0.103555  270.371 <0.0000000000000002 ***
#log(GIS_T_AREA)                                       0.407576   0.006335   64.341 <0.0000000000000002 ***
#CONTINENTAsia                                       -14.970802   0.133063 -112.509 <0.0000000000000002 ***
#CONTINENTAustralia and New Zealand                  -20.188033   0.122152 -165.270 <0.0000000000000002 ***
#CONTINENTCanada and U.S.A.                          -14.865769   0.118704 -125.234 <0.0000000000000002 ***
#CONTINENTCentral America, Mexico, and the Caribbean -11.276719   0.192181  -58.678 <0.0000000000000002 ***
#CONTINENTEurope                                     -18.491242   0.107817 -171.506 <0.0000000000000002 ***
#CONTINENTMadagascar                                   0.083484   0.766111    0.109               0.913    
#CONTINENTMelanesia, Micronesia, and Polynesia       -19.099019   0.844930  -22.604 <0.0000000000000002 ***
#CONTINENTSouth America                               17.050890   0.156352  109.055 <0.0000000000000002 ***
#CONTINENTSoutheast Asia                              -6.517757   0.270310  -24.112 <0.0000000000000002 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 8.758 on 209646 degrees of freedom
#  (35701 observations deleted due to missingness)
#Multiple R-squared:  0.4021,	Adjusted R-squared:  0.4021 
#F-statistic: 1.41e+04 on 10 and 209646 DF,  p-value: < 0.00000000000000022
                     

#Residual standard error: 10.94 on 245356 degrees of freedom
#Multiple R-squared:  0.06773,	Adjusted R-squared:  0.06772 
#F-statistic: 1.782e+04 on 1 and 245356 DF,  p-value: < 0.00000000000000022
                                        
