# R language

# Plots summary of protected and unprotected amphibians by IUCN threatened status
# This includes pie charts of this data per geographic region

#Set up working environment
library(shiny)
library(dplyr) 
library(ggplot2) 
library(plyr) 
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

# Load all species found in all PAs
SpPA <- read.csv("Data using Amphibian Ranges for Species Richness/All_Amphibia with WDPA one to many.csv")
SpPA<-SpPA[,2:3]
#total number of frog spp in this spreadsheet    
length(unique(SpPA$binomial)) 

# Load and clean up threat data from Amphibiaweb
Amph <- read.csv("Data using Amphibian Ranges for Species Richness/Amphibians_w_ranges.csv")
length(unique(Amph$binomial))   #7094 species
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

# Merge threat and WDPA overlap data for amphibians
SpPA <- left_join(Amph, SpPA, by="binomial")
length(unique(SpPA$binomial)) 

# Calculate unprotected species
length(SpPA$binomial[is.na(SpPA$WDPA_PID)])/length(unique(SpPA$binomial))    

# Label amphibians not on PAs as "not protected"
SpPA$WDPA_PID[is.na(SpPA$WDPA_PID)] <- "NotProtected"
# double check this is right
length(SpPA$binomial[SpPA$WDPA_PID=="NotProtected"])

# Of the 1115 species that don't overlap with PAs, donut chart of IUCN status... 
unprotected <- SpPA[SpPA$WDPA_PID=="NotProtected" & !is.na(SpPA$AWiucn), ] %>%
  dplyr::group_by(AWiucn) %>%
  dplyr::summarise(counts = length(unique(binomial))) 

#Order by endangerement
unprotected$AWiucn <- factor(unprotected$AWiucn, levels = rev(c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "In assessment")))
unprotected$perc <- round((unprotected$counts/sum(unprotected$counts)*100), digits=1)

# Unprotected donut
jpeg("../Figures/unprotected_amphibians.jpg", width=1000, height=1000)
ggplot(unprotected, aes(x = 2, y = counts, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c('white', 'grey', 'chartreuse4', 'greenyellow', 'yellow', 'orange', 'red', 'darkmagenta', 'black'))+
  labs(title="") +
  coord_polar(theta = "y") +
  xlim(c(0.2, 3.5)) +
  annotate(geom = 'text', x = 0.5, y = 0, label = "\n1115\nunprotected\namphibian\nspecies", size=16, family="Calibri") +
  theme_cowplot() + 
  theme(text=element_text(family="Calibri"), 
        axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 

# Now protected donut...
protected <- SpPA[SpPA$WDPA_PID!="NotProtected" & !is.na(SpPA$AWiucn), ] %>%
  dplyr::group_by(AWiucn) %>%
  dplyr::summarise(counts = length(unique(binomial)))     
protected$AWiucn <- factor(protected$AWiucn, levels = rev(c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "In assessment")))
protected$perc <- round((protected$counts/sum(protected$counts)*100), digits=1)
jpeg("../Figures/protected_amphibians.jpg", width=1000, height=1000)
ggplot(protected, aes(x = 2, y = counts, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c('white', 'grey', 'chartreuse4', 'greenyellow', 'yellow', 'orange', 'red', 'darkmagenta', 'black'))+
  labs(title="Amphibians protected by PAs:\nIUCN status") +
  coord_polar(theta = "y") +
  xlim(c(0.2, 3.5)) +
  annotate(geom = 'text', x = 0.5, y = 0, label = "\n5979\nprotected\namphibian\nspecies", size=16, family="Calibri") +
  theme_cowplot() + 
  theme(text=element_text(family="Calibri"), 
        axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 

# Calculate percentage of each IUCN category that is unprotected
sum(unprotected$counts[unprotected$AWiucn %in% c("CR")])/(sum(unprotected$counts[unprotected$AWiucn %in% c("CR")])+sum(protected$counts[protected$AWiucn %in% c("CR")]))
sum(unprotected$counts[unprotected$AWiucn %in% c("EN")])/(sum(unprotected$counts[unprotected$AWiucn %in% c("EN")])+sum(protected$counts[protected$AWiucn %in% c("EN")]))
sum(unprotected$counts[unprotected$AWiucn %in% c("VU")])/(sum(unprotected$counts[unprotected$AWiucn %in% c("VU")])+sum(protected$counts[protected$AWiucn %in% c("VU")]))
sum(unprotected$counts[unprotected$AWiucn %in% c("NT")])/(sum(unprotected$counts[unprotected$AWiucn %in% c("NT")])+sum(protected$counts[protected$AWiucn %in% c("NT")]))
sum(unprotected$counts[unprotected$AWiucn %in% c("LC")])/(sum(unprotected$counts[unprotected$AWiucn %in% c("LC")])+sum(protected$counts[protected$AWiucn %in% c("LC")]))
sum(unprotected$counts[unprotected$AWiucn %in% c("DD")])/(sum(unprotected$counts[unprotected$AWiucn %in% c("DD")])+sum(protected$counts[protected$AWiucn %in% c("DD")]))
sum(unprotected$counts[unprotected$AWiucn %in% c("In assessment")])/(sum(unprotected$counts[unprotected$AWiucn %in% c("In assessment")])+sum(protected$counts[protected$AWiucn %in% c("In assessment")]))

# Clean up working environment
remove(protected)
remove(unprotected)

# Summarize species counts per PA
SpPaSum <- SpPA %>%
  dplyr::group_by(WDPA_PID) %>%
  dplyr::summarise(totRichness = length(unique(binomial))) 

######################
# REGIONAL ASSESSMENTS

#Now, while I have the species, their status, and their WDPAs in a sheet, I need to make a donut for each continent: protected v. unprotected species, 
#proportion of species that are of each IUCN status. So got to add that continent information from WDPA data. 
xtra <- read.csv("All_Amphibia_Country_Cont.csv")  # This is Species+Continent+Hemisphere.csv in Dryad
xtra <- xtra[ , which(names(xtra) %in% c("binomial", "CONTINENT"))]
Donuts <- left_join(SpPA, xtra, by=c("binomial"="binomial"))

#Anything that's NOT not protected is protected
Donuts$WDPA_PID[Donuts$WDPA_PID!="NotProtected"]<-"Protected"

#Now, for each continent, protectVunprotected, and IUCN status combination, summarize
DonutSums <- Donuts %>%
  dplyr::group_by(CONTINENT, WDPA_PID, AWiucn) %>%
  dplyr::summarise(count = length(unique(binomial))) 

#Order by endangerement
DonutSums$AWiucn <- factor(DonutSums$AWiucn, levels = rev(c("CR", "EN", "VU", "NT", "LC", "DD", "In assessment")))

#Melanesia, Micronesia, Polynesia
MMPProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Melanesia, Micronesia, and Polynesia" & !is.na(DonutSums$AWiucn),]
MMPProtected$perc <- round((MMPProtected$count/sum(MMPProtected$count)*100), digits=1)
MMPProtected <- MMPProtected[rowSums(is.na(MMPProtected)) != ncol(MMPProtected), ]
sum(MMPProtected$count) #174
sum(MMPProtected$count[MMPProtected$AWiucn=="EN" | MMPProtected$AWiucn=="CR" | MMPProtected$AWiucn=="VU" | MMPProtected$AWiucn=="EX" | MMPProtected$AWiucn=="EW"])/sum(MMPProtected$count)  #317
jpeg("../Figures/protected_MMP.jpg", width=1000, height=1000)
ggplot(MMPProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  annotate(geom = 'text', x = 0.5, y = 0, label = "174", size=60, family="Calibri") +
  theme_cowplot() + 
  guides(fill="none")+
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(MMPProtected)
MMPUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Melanesia, Micronesia, and Polynesia" & !is.na(DonutSums$AWiucn),]
MMPUnProtected$perc <- round((MMPUnProtected$count/sum(MMPUnProtected$count*100)), digits=1)
MMPUnProtected <- MMPUnProtected[rowSums(is.na(MMPUnProtected)) != ncol(MMPUnProtected), ]
sum(MMPUnProtected$count, na.rm=T) #123
sum(MMPUnProtected$count[MMPUnProtected$AWiucn=="EN" | MMPUnProtected$AWiucn=="CR" | MMPUnProtected$AWiucn=="VU" | MMPUnProtected$AWiucn=="EX" | MMPUnProtected$AWiucn=="EW"])/sum(MMPUnProtected$count)  #317
jpeg("../Figures/unprotected_MMP.jpg", width=1000, height=1000)
ggplot(MMPUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "123", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(MMPUnProtected)

# Madagascar
MadagascarProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Madagascar" & !is.na(DonutSums$AWiucn),]
MadagascarProtected$perc <- round((MadagascarProtected$count/sum(MadagascarProtected$count)*100), digits=1)
MadagascarProtected <- MadagascarProtected[rowSums(is.na(MadagascarProtected)) != ncol(MadagascarProtected), ]
sum(MadagascarProtected$count) #317
sum(MadagascarProtected$count[MadagascarProtected$AWiucn=="EN" | MadagascarProtected$AWiucn=="CR" | MadagascarProtected$AWiucn=="VU" | MadagascarProtected$AWiucn=="EX" | MadagascarProtected$AWiucn=="EW", ]) #317
jpeg("../Figures/protected_Madagascar.jpg", width=1000, height=1000)
ggplot(MadagascarProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  annotate(geom = 'text', x = 0.5, y = 0, label = "317", size=60, family="Calibri") +
  theme_cowplot() + 
  guides(fill="none")+
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(MadagascarProtected)
MadagascarUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Madagascar" & !is.na(DonutSums$AWiucn),]
MadagascarUnProtected$perc <- round((MadagascarUnProtected$count/sum(MadagascarUnProtected$count*100)), digits=1)
MadagascarUnProtected <- MadagascarUnProtected[rowSums(is.na(MadagascarUnProtected)) != ncol(MadagascarUnProtected), ]
sum(MadagascarUnProtected$count, na.rm=T) #4
jpeg("../Figures/unprotected_Madagascar.jpg", width=1000, height=1000)
ggplot(MadagascarUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "4", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(MadagascarUnProtected)

# Africa
AfricaProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Africa" & !is.na(DonutSums$AWiucn),]
AfricaProtected$perc <- round((AfricaProtected$count/sum(AfricaProtected$count)*100), digits=1)
AfricaProtected <- AfricaProtected[rowSums(is.na(AfricaProtected)) != ncol(AfricaProtected), ]
sum(AfricaProtected$count) #752
jpeg("../Figures/protected_africa.jpg", width=1000, height=1000)
ggplot(AfricaProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  annotate(geom = 'text', x = 0.5, y = 0, label = "752", size=60, family="Calibri") +
  theme_cowplot() + 
  guides(fill="none")+
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(AfricaProtected)
AfricaUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Africa" & !is.na(DonutSums$AWiucn),]
AfricaUnProtected$perc <- round((AfricaUnProtected$count/sum(AfricaUnProtected$count*100)), digits=1)
AfricaUnProtected <- AfricaUnProtected[rowSums(is.na(AfricaUnProtected)) != ncol(AfricaUnProtected), ]
sum(AfricaUnProtected$count, na.rm=T) #101
jpeg("../Figures/unprotected_africa.jpg", width=1000, height=1000)
ggplot(AfricaUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "101", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(AfricaUnProtected)

# SE Asia
SEAsiaProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Southeast Asia" & !is.na(DonutSums$AWiucn),]
SEAsiaProtected$perc <- round((SEAsiaProtected$count/sum(SEAsiaProtected$count)*100), digits=1)
SEAsiaProtected <- SEAsiaProtected[rowSums(is.na(SEAsiaProtected)) != ncol(SEAsiaProtected), ]
sum(SEAsiaProtected$count) #518
jpeg("../Figures/protected_SEasia.jpg", width=1000, height=1000)
ggplot(SEAsiaProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "518", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(SEAsiaProtected)
SEAsiaUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Southeast Asia" & !is.na(DonutSums$AWiucn),]
SEAsiaUnProtected$perc <- round((SEAsiaUnProtected$count/sum(SEAsiaUnProtected$count)*100), digits=1)
SEAsiaUnProtected <- SEAsiaUnProtected[rowSums(is.na(SEAsiaUnProtected)) != ncol(SEAsiaUnProtected), ]
sum(SEAsiaUnProtected$count) #63
jpeg("../Figures/unprotected_SEasia.jpg", width=1000, height=1000)
ggplot(SEAsiaUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "63", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(SEAsiaUnProtected)

# Asia
AsiaProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Asia" & !is.na(DonutSums$AWiucn),]
AsiaProtected$perc <- round((AsiaProtected$count/sum(AsiaProtected$count)*100), digits=1)
AsiaProtected <- AsiaProtected[rowSums(is.na(AsiaProtected)) != ncol(AsiaProtected), ]
sum(AsiaProtected$count) #817
jpeg("../Figures/protected_asia.jpg", width=1000, height=1000)
ggplot(AsiaProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "817", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(AsiaProtected)
AsiaUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Asia" & !is.na(DonutSums$AWiucn),]
AsiaUnProtected$perc <- round((AsiaUnProtected$count/sum(AsiaUnProtected$count)*100), digits=1)
AsiaUnProtected <- AsiaUnProtected[rowSums(is.na(AsiaUnProtected)) != ncol(AsiaUnProtected), ]
sum(AsiaUnProtected$count) #296
jpeg("../Figures/unprotected_asia.jpg", width=1000, height=1000)
ggplot(AsiaUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "296", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(AsiaUnProtected)

# Europe
EuropeProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Europe" & !is.na(DonutSums$AWiucn),]
EuropeProtected$perc <- round((EuropeProtected$count/sum(EuropeProtected$count)*100), digits=1)
EuropeProtected <- EuropeProtected[rowSums(is.na(EuropeProtected)) != ncol(EuropeProtected), ]
sum(EuropeProtected$count) #204
jpeg("../Figures/protected_europe.jpg", width=1000, height=1000)
ggplot(EuropeProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "204", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(EuropeProtected)
EuropeUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Europe" & !is.na(DonutSums$AWiucn),]
EuropeUnProtected$perc <- round((EuropeUnProtected$count/sum(EuropeUnProtected$count)*100), digits=1)
EuropeUnProtected <- EuropeUnProtected[rowSums(is.na(EuropeUnProtected)) != ncol(EuropeUnProtected), ]
sum(EuropeUnProtected$count) #0

# North America
NorthAmericaProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Canada and U.S.A." & !is.na(DonutSums$AWiucn),]
NorthAmericaProtected$perc <- round((NorthAmericaProtected$count/sum(NorthAmericaProtected$count)*100), digits=1)
NorthAmericaProtected <- NorthAmericaProtected[rowSums(is.na(NorthAmericaProtected)) != ncol(NorthAmericaProtected), ]
sum(NorthAmericaProtected$count) #283
jpeg("../Figures/protected_northamerica.jpg", width=1000, height=1000)
ggplot(NorthAmericaProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "283", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(NorthAmericaProtected)
NorthAmericaUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Canada and U.S.A." & !is.na(DonutSums$AWiucn),]
NorthAmericaUnProtected$perc <- round((NorthAmericaUnProtected$count/sum(NorthAmericaUnProtected$count)*100), digits=1)
NorthAmericaUnProtected <- NorthAmericaUnProtected[rowSums(is.na(NorthAmericaUnProtected)) != ncol(NorthAmericaUnProtected), ]
sum(NorthAmericaUnProtected$count) #8
jpeg("../Figures/unprotected_northamerica.jpg", width=1000, height=1000)
ggplot(NorthAmericaUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "8", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(NorthAmericaUnProtected)

# Australia and New Zealand
AZProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Autralia and New Zealand" & !is.na(DonutSums$AWiucn),]
AZProtected$perc <- round((AZProtected$count/sum(AZProtected$count)*100), digits=1)
AZProtected <- AZProtected[rowSums(is.na(AZProtected)) != ncol(AZProtected), ]
sum(AZProtected$count) #218
jpeg("../Figures/protected_AZ.jpg", width=1000, height=1000)
ggplot(AZProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "218", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(AZProtected)
AZUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Autralia and New Zealand" & !is.na(DonutSums$AWiucn),]
AZUnProtected$perc <- round((AZUnProtected$count/sum(AZUnProtected$count)*100), digits=1)
AZUnProtected <- AZUnProtected[rowSums(is.na(AZUnProtected)) != ncol(AZUnProtected), ]
sum(AZUnProtected$count) #3
jpeg("../Figures/unprotected_AZ.jpg", width=1000, height=1000)
ggplot(AZUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "3", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(AZUnProtected)

# South America
SouthAmericaProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="South America" & !is.na(DonutSums$AWiucn),]
SouthAmericaProtected$perc <- round((SouthAmericaProtected$count/sum(SouthAmericaProtected$count)*100), digits=1)
SouthAmericaProtected <- SouthAmericaProtected[rowSums(is.na(SouthAmericaProtected)) != ncol(SouthAmericaProtected), ]
sum(SouthAmericaProtected$count) ##2108
jpeg("../Figures/protected_southamerica.jpg", width=1000, height=1000)
ggplot(SouthAmericaProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "2108", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(SouthAmericaProtected)
SouthAmericaUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="South America" & !is.na(DonutSums$AWiucn),]
SouthAmericaUnProtected$perc <- round((SouthAmericaUnProtected$count/sum(SouthAmericaUnProtected$count)*100), digits=1)
SouthAmericaUnProtected <- SouthAmericaUnProtected[rowSums(is.na(SouthAmericaUnProtected)) != ncol(SouthAmericaUnProtected), ]
sum(SouthAmericaUnProtected$count) #349
jpeg("../Figures/unprotected_southamerica.jpg", width=1000, height=1000)
ggplot(SouthAmericaUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "349", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(SouthAmericaUnProtected)

# Carribean
CAProtected<-DonutSums[DonutSums$WDPA_PID=="Protected" & DonutSums$CONTINENT=="Central America, Mexico, and the Caribbean" & !is.na(DonutSums$AWiucn),]
CAProtected$perc <- round((CAProtected$count/sum(CAProtected$count)*100), digits=1)
CAProtected <- CAProtected[rowSums(is.na(CAProtected)) != ncol(CAProtected), ]
sum(CAProtected$count) ##887
sum(CAProtected$count[CAProtected$AWiucn=="EN" | CAProtected$AWiucn=="CR" | CAProtected$AWiucn=="VU" | CAProtected$AWiucn=="EX" | CAProtected$AWiucn=="EW"])/sum(CAProtected$count)  #317
jpeg("../Figures/protected_centralamerica.jpg", width=1000, height=1000)
ggplot(CAProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "887", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(CAProtected)
CAUnProtected<-DonutSums[DonutSums$WDPA_PID=="NotProtected" & DonutSums$CONTINENT=="Central America, Mexico, and the Caribbean" & !is.na(DonutSums$AWiucn),]
CAUnProtected$perc <- round((CAUnProtected$count/sum(CAUnProtected$count)*100), digits=1)
CAUnProtected <- CAUnProtected[rowSums(is.na(CAUnProtected)) != ncol(CAUnProtected), ]
sum(CAUnProtected$count) #138
sum(CAUnProtected$count[CAUnProtected$AWiucn=="EN" | CAUnProtected$AWiucn=="CR" | CAUnProtected$AWiucn=="VU" | CAUnProtected$AWiucn=="EX" | CAUnProtected$AWiucn=="EW"])/sum(CAUnProtected$count)  #317
jpeg("../Figures/unprotected_centralamerica.jpg", width=1000, height=1000)
ggplot(CAUnProtected, aes(x = 2, y = count, fill = AWiucn)) +
  geom_col(color="black") +
  scale_fill_manual(values=c("In assessment"='white', "DD"='grey', "LC"='chartreuse4', "NT"='greenyellow', "VU"='yellow', "EN"='orange', "CR"='red', "EW"='darkmagenta', "EX"='black'))+
  coord_polar(theta = "y") +
  guides(fill="none")+
  annotate(geom = 'text', x = 0.5, y = 0, label = "138", size=60, family="Calibri") +
  theme_cowplot() + 
  theme(axis.title=element_blank(), 
        axis.text = element_blank(), 
        plot.margin = unit(c(0, 0, 0, 0), "cm"), 
        legend.position = "top", 
        legend.title = element_blank(), 
        plot.title=element_text(size=40), 
        legend.text=element_text(size=30), 
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1.5,"cm")) 
dev.off() 
remove(CAUnProtected)
