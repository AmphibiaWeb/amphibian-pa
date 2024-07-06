# R language

# Create supplementary figures with barplots of species richness, threatened species, and unprotected species per country

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
library(data.table) 

# Establish color palette for plotting
IUCNColors <- c("DD" = "Grey", "In assessment"="white", "LC" = "Chartreuse4", "NT"="Greenyellow", "VU"="yellow", "EN"="Orange", "CR"="Red", "EW"="Darkmagenta", "EX"="black")

#Load in all species with range data
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

# Load all species found in all PAs
SpPA <- read.csv("Data using Amphibian Ranges for Species Richness/All_Amphibia with WDPA one to many.csv")
length(unique(SpPA$binomial)) #6932                               
                   
#Import country-richness data
CountryRich <- read.csv("Data using Amphibian Ranges for Species Richness/Species+Continent+Hemisphere.csv")
keep<-c("binomial", "CONTINENT", "NAME_EN")
CountryRich<-CountryRich[,names(CountryRich)%in%keep]    
country <- join(CountryRich, Amph, by="binomial") 
country <- join(country, SpPA, by="binomial") 
#Any species without a WDPAPID listed is considered unprotected
country$WDPA_PID<-country$WDPA_PID %>% replace_na('NotProtected')
country$NAME_EN <- gsub("People's Republic of China","China",as.character(country$NAME_EN))
country$NAME_EN <- gsub("Democratic Republic of the Congo","the Congo",as.character(country$NAME_EN))
                   
#summarize by each threatened category per country
countsum <- country %>%
  dplyr::group_by(NAME_EN, AWiucn) %>%                   
  dplyr::summarise(CategoryCount = length(unique(binomial)))      

#total country threatened richness
totsum <- country %>%
  dplyr::group_by(NAME_EN) %>%                   
  dplyr::summarise(Total = length(unique(binomial)), 
                   TotalDD=length(unique(binomial[AWiucn=="DD"])),
                   TotalUnprotected = length(unique(binomial[WDPA_PID=="NotProtected"])), 
                   TotalAssessed=length(unique(binomial[AWiucn=="EX" | AWiucn== "EW" | AWiucn=="CR" | AWiucn=="EN" | AWiucn=="VU" | AWiucn=="NT" | AWiucn=="LC"])), 
                   TotalThreatened=length(unique(binomial[AWiucn=="EX" | AWiucn== "EW" | AWiucn=="CR" | AWiucn=="EN" | AWiucn=="VU"])), 
                  TotalThreatUnprotect = length(unique(binomial[WDPA_PID=="NotProtected" & (AWiucn=="EX" | AWiucn== "EW" | AWiucn=="CR" | AWiucn=="EN" | AWiucn=="VU")])), 
                   TotEndangeredPlus=length(unique(binomial[AWiucn=="EX" | AWiucn== "EW" | AWiucn=="CR" | AWiucn=="EN"])))                   
totsum$TotalProtected <- totsum$Total - totsum$TotalUnprotected
totsum$PropThreat <- totsum$TotalThreatened/totsum$TotalAssessed   
totsum$PropUnprotected <- totsum$TotalUnprotected/totsum$Total  
totsum$PropEndangeredPlus <- totsum$TotEndangeredPlus/totsum$TotalAssessed                       
countsum <- join(countsum, totsum, by="NAME_EN")

# Join data on threatened and total richness
countsum$AWiucn <- factor(countsum$AWiucn, levels = rev(c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "In assessment")))
countsum <- filter(countsum, !is.na(AWiucn))   

# For unprotected species only, summarize by each category per country
unprosum <- country[country$WDPA_PID=="NotProtected",] %>%
  dplyr::group_by(NAME_EN, AWiucn) %>%                   
  dplyr::summarise(PAstatus="NotProtected", PACatCount = length(unique(binomial)))                   
#For protected species only, summarize by each category per country
prosum <- country[country$WDPA_PID!="NotProtected",] %>%
  dplyr::group_by(NAME_EN, AWiucn) %>%                   
  dplyr::summarise(PAstatus="Protected", PACatCount = length(unique(binomial))) 
totes <- rbind(unprosum, prosum) 
totes <- join(totes, totsum, by="NAME_EN")
#I want levels to be ordered by endangerement
totes$AWiucn <- factor(totes$AWiucn, levels = rev(c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "In assessment")))
totes <- filter(totes, !is.na(AWiucn))

#Top 10 most species-rich countries                   
length(unique(countsum$NAME_EN[countsum$Total>315]))
plotA <- countsum[countsum$Total>315,]                  
a<- ggplot(data=plotA, aes(x=reorder(NAME_EN, Total), y=CategoryCount, fill=AWiucn)) +
  geom_bar(stat="identity", color="black", size=0.4, width=0.8)+
  labs(title="A.") +
  scale_fill_manual(values=IUCNColors)+
  scale_y_continuous(name="Number of species")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+ 
  theme_cowplot(font_size=24) + 
  guides(fill="none")+                 
  coord_flip() +
  theme(text=element_text(family="Calibri", size=65), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle=45, vjust=0.5, size=55), 
        axis.text.y = element_text(size=55), 
        legend.title=element_blank(), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm"), 
        legend.position = "top", panel.background = element_rect(fill = "white", colour = "grey50"))                                     
                   
#Top 25 countries by count of threatened species                   
length(unique(countsum$NAME_EN[countsum$TotalThreatened>62]))
plotB <- countsum[countsum$TotalThreatened>62 & (countsum$AWiucn == "EN" | countsum$AWiucn == "CR" |countsum$AWiucn == "VU" | countsum$AWiucn == "EW"),]                 
b<- ggplot(data=plotB, aes(x=reorder(NAME_EN, TotalThreatened), y=CategoryCount, fill=AWiucn)) +
  geom_bar(stat="identity", color="black", size=0.4, width=0.8)+
  labs(title="B.") +
  scale_fill_manual(values=IUCNColors)+
  scale_y_continuous(name="Number of endangered species")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+ 
  theme_cowplot(font_size=24) + 
  coord_flip() +
  guides(fill="none")+                 
  theme(text=element_text(family="Calibri", size=65), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle=45, vjust=0.5, size=55), 
        axis.text.y = element_text(size=55), 
        legend.title=element_blank(), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm"), 
        legend.position = "top", 
        panel.background = element_rect(fill = "white", colour = "grey50"))                        
                   
#Top 10 countries by count of unprotected species                   
length(unique(totes$NAME_EN[totes$TotalUnprotected>20]))
plotC <- totes[totes$TotalUnprotected>20,]
c<-ggplot(data=plotC[plotC$PAstatus=="NotProtected",], aes(x=reorder(NAME_EN, TotalUnprotected), y=PACatCount, fill=AWiucn)) +
  geom_bar(stat="identity", color="black", size=0.4, width=0.8)+
  labs(title="C.") +
  scale_y_continuous(name="Number of unprotected species")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+ 
  scale_fill_manual(values=IUCNColors)+
  theme_cowplot(font_size=24) +
  guides(ncol=2)+                 
  coord_flip() +
  theme(text=element_text(family="Calibri", size=65), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle=45, vjust=0.5, size=55), 
        axis.text.y = element_text(size=55), 
        legend.title=element_blank(), legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm"), 
        legend.position = "right", 
        panel.background = element_rect(fill = "white", colour = "grey50"))    

# Plot these three together                   
jpeg("../Figures/IUCNstatus_top10countries.jpg", width=3000, height=1000)
plot_grid(a,b,c,nrow=1, rel_widths=c(1, 1, 1.3))
dev.off()    
                                  
##############
# Bonus: data deficient
                            
length(unique(countsum$NAME_EN[countsum$TotalDD>50]))
plotD <- countsum[countsum$TotalDD>50& countsum$AWiucn == "DD",]
jpeg("../Figures/IUCNstatus_top10DD.jpg", width=1000, height=1000)
ggplot(data=plotD, aes(x=reorder(NAME_EN, TotalDD), y=TotalDD)) +
  geom_bar(stat="identity", color="black", size=0.4, width=0.8, fill="grey")+
  labs(title="") +
  scale_y_continuous(name="Data deficient species")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+ 
  theme_cowplot(font_size=24) + 
  guides(fill="none")+                 
  coord_flip() +
  theme(text=element_text(family="Calibri", size=65), 
        axis.title.y=element_blank(), 
        axis.text.x = element_text(angle=45, vjust=0.5, size=55), 
        axis.text.y = element_text(size=55), 
        legend.title=element_blank(), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm"), 
        legend.position = "top", panel.background = element_rect(fill = "white", colour = "grey50"))                         
dev.off()                   
