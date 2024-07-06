
# R language

# Compares range sizes for different taxa through statistical analysis and frequency histogram plots

#Set up working environment
library(dplyr) 
library(ggplot2) 
library(plyr) 
library(RColorBrewer)
library(stringr) 
library(cowplot)
library(SciViews)
library(tidyr) 
library(forcats)
library(ggridges)
library(khroma) 
muted <- colour("muted")
library(lessR)
library(data.table) 
library(scales)
library(nortest)
library(car)

# Read taxon ranges for reptiles, mammals, and birds
ranges <- read.csv("OtherTaxaRanges/All_taxa_ranges.csv")
ranges <- ranges[,c("binomial", "label", "category", "Area")] 

# Add extra reptiles from GARD database
reps <- read.csv("OtherTaxaRanges/GARD_extraReptiles.csv")[2:5]
reps <- reps[,c("Species", "label", "category", "Area")] 
names(reps)[1]<-"binomial"
ranges<-rbind(ranges, reps)

# Add bird iucn statuses
bird_status <- read.csv("OtherTaxaRanges/Bird_IUCNstatus.csv")     
bird_status<-bird_status[,1:2]
ranges<-setDT(ranges)[bird_status, category := i.category, on = .(binomial)]                   

# Add amphibian range data from AmphibiaWeb
Amph <- read.csv("Data using Amphibian Ranges for Species Richness/Amphibians_w_ranges.csv")
length(unique(Amph$binomial))  

# Add threat data from AmphibiaWeb
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
ranges <- setDT(ranges)[Amph, category := AWiucn, on = .(binomial)]                    
                   
# All but bird ranges in m^2 (birds in km^2), so convert
ranges$Area[ranges$label!="birds"] <- ranges$Area[ranges$label!="birds"]/1000000                   

#Summarize by species binomial (many species are entered in csv multiple times, once for each country occurs in
ranges$binomial <- as.factor(ranges$binomial)                  
range_summ <- ranges %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(label=label, category=category, Area = sum(Area)) 
ranges <- range_summ[!duplicated(range_summ), ]                                    
                   
# Calculate and order by the median range size per taxon
range_sum <- ranges %>%
       group_by(label) %>%
       dplyr::summarize(m = median(Area, na.rm=TRUE)) %>%           
       #summarize_at(vars(Area), list(m=mean)) %>%
       arrange(desc(m)) %>%
       ungroup() %>%
       mutate(label=factor(label, unique(label)))
range_sum_1 <- left_join(ranges, range_sum, by=c("label"="label")) 

#remove plants from dataset
range_sum_1 <- range_sum_1[range_sum_1$label!="plants",]
                   
range_sum_1$label <- as.factor(range_sum_1$label)
range_sum_1 <- range_sum_1 %>%
       mutate(label = factor(label, levels = c("birds", "mammals", "reptiles", "amphibians")))
range_sum_1 <- range_sum_1[complete.cases(range_sum_1$Area), ]

# Count species in dataset per taxon
length(unique(range_sum_1$binomial[range_sum_1$label=="amphibians"]))                  
length(unique(range_sum_1$binomial[range_sum_1$label=="reptiles"]))                   
length(unique(range_sum_1$binomial[range_sum_1$label=="mammals"]))                   
length(unique(range_sum_1$binomial[range_sum_1$label=="birds"])) 
              
# What are the max and min amphibian range size species?
range_sum_1$binomial[range_sum_1$Area==max(range_sum_1$Area[range_sum_1$label=="amphibians"])] 
range_sum_1$binomial[range_sum_1$Area==min(range_sum_1$Area[range_sum_1$label=="amphibians"])]                    

# Group into threatened and non-threatened categories
range_sum_1$group_1[range_sum_1$category=="LC" | range_sum_1$category=="LC-provisional" | range_sum_1$category=="DD-provisional" | range_sum_1$category=="DD" | range_sum_1$category=="In assessment" | range_sum_1$category=="LC/cd" | range_sum_1$category=="NE" | range_sum_1$category=="NT" | range_sum_1$category=="NT-provisional"] <- "not_threatened"
range_sum_1$group_1[range_sum_1$category=="CR" | range_sum_1$category=="CR-provisional" | range_sum_1$category=="CR-Provisional" | range_sum_1$category=="EN" | range_sum_1$category=="EN-provisional" | range_sum_1$category=="VU" | range_sum_1$category=="VU-provisional"] <- "threatened"

# Compare taxon ranges via a smoothed frequency histogram                    
jpeg(file="../Figures/TaxaRanges_27Dec23.jpeg", height=1000, width=2000)                   
ggplot(range_sum_1, aes(x=Area, y=label, fill=label)) +
  geom_density_ridges(scale=1, rel_min_height=0, quantile_lines=T, quantiles=2) + 
  scale_y_discrete(limits = c("birds", "mammals", "reptiles", "amphibians"), expand = c(0.05, 0))+
  scale_x_continuous(trans='log10', name=expression("Range area ("~km^2~", log scale)"),                               
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))  +              
  scale_fill_manual(values=c("#DDDDDD","#999933",  "#DDCC77", "deeppink3"))+
  coord_cartesian(xlim = c(1, 1e8)) +                   
  theme_ridges() +
  theme(text=element_text(family="Calibri"), 
        plot.title = element_blank(), 
        legend.position = "none", 
        axis.title = element_blank(), 
        axis.text.x = element_text(size=35, angle=45), 
        axis.text.y = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "grey50"), 
        plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"))
dev.off()
 
################################
                     
# Test to check whether range size different between taxa

# First, test assumptions:  Assumes normal distribution of data and equal variances between groups.               
# Check normality of residuals
residuals <- lm(Area ~ label, data = range_sum_1)$residuals
par(mar = c(1, 1, 1, 1))  
qqPlot(residuals, main = "Normal Q-Q Plot of Residuals")
# Check homogeneity of variances
p1<-leveneTest(Area ~ label, data = range_sum_1)[1,3]
print(paste0("p-value for Levene's is ",p1))
# Shapiro-Wilk test for normality (optional)
p2<-ad.test(residuals)$p.value
print(paste0("p-value for Anderson-Darling's is ",p2))

# if these tests are significant, conduct a Kruskal-wallis test                     
if (p1 < 0.05 || p2 < 0.05){   
           kruskal_result <- kruskal.test(Area ~ label, data = range_sum_1)
           cat("Kruskal-Wallis test results")
           print(kruskal_result)
           pairwise_result <- pairwise.wilcox.test(range_sum_1$Area, range_sum_1$label, p.adj = "bonferroni")
           cat("Pairwise Wilcoxon test results")
           print(pairwise_result)
# If neither test is significant, conduct an ANOVA  
} else {                    
           anova_result <- aov(Area ~ label, data = range_sum_1)
           print(paste0("ANOVA test results"))
           print(summary(anova_result))     
           # Post hoc Tukey's HSD test
           tukey_result <- TukeyHSD(anova_result)
           cat("Tukey's HSD test results")
           print(tukey_result)}
                     
 ################################
                     
# Test to compare range sizes between taxa when considering THREATENED speces only

threatened<-range_sum_1[range_sum_1$group_1=="threatened",]      
                     
# First, test assumptions:  Assumes normal distribution of data and equal variances between groups.               
# Check normality of residuals
residuals <- lm(Area ~ label, data = threatened)$residuals
par(mar = c(1, 1, 1, 1))  
qqPlot(residuals, main = "Normal Q-Q Plot of Residuals")
# Check homogeneity of variances
p1<-leveneTest(Area ~ label, data = threatened)[1,3]
print(paste0("p-value for Levene's is ",p1))
# Shapiro-Wilk test for normality (optional)
p2<-ad.test(residuals)$p.value
print(paste0("p-value for Anderson-Darling's is ",p2))

# if these tests are significant, conduct a Kruskal-wallis test                     
if (p1 < 0.05 || p2 < 0.05){   
           kruskal_result <- kruskal.test(Area ~ label, data = threatened)
           cat("Kruskal-Wallis test results")
           print(kruskal_result)
           pairwise_result <- pairwise.wilcox.test(threatened$Area, threatened$label, p.adj = "bonferroni")
           cat("Pairwise Wilcoxon test results")
           print(pairwise_result)
# Otherwise, conduct ANOVA  
} else {                    
           anova_result <- aov(Area ~ label, data = threatened)
           print(paste0("ANOVA test results"))
           print(summary(anova_result))     
           # Post hoc Tukey's HSD test
           tukey_result <- TukeyHSD(anova_result)
           cat("Tukey's HSD test results")
           print(tukey_result)}                    
                                   
###########################################################################                     
                     
# Create a box and whisker plot for each taxon, showing range sizes for THREATENED taxa only
                     
plotting <- transform(range_sum_1, label=factor(label,levels=c("birds", "mammals", "reptiles", "amphibians")))
plotting <-plotting[plotting$category=="CR" | plotting$category=="CR-provisional" | plotting$category=="CR-Provisional" | plotting$category=="EN" | plotting$category=="EN-provisional" | plotting$category=="VU-provisional" | plotting$category=="VU" | plotting$category=="EW"|  plotting$category=="EX"| plotting$category=="EX in the Wild",]                   
plotting <- plotting %>%
       mutate(label = factor(label, levels = c("amphibians", "reptiles", "mammals", "birds")))
#max y-lim value?
max(plotting$Area)                   

 # Count species per taxon in plot                   
table(plotting$label)
  
#Find the median value for annotation purposes
annot <- plotting %>%
       group_by(label) %>%
       dplyr::summarize(m = median(Area, na.rm=TRUE))  

#Log-scale of this box and whisker plot
# Function to use boxplot.stats to set the box-and-whisker locations  
mybxp = function(x) {
  bxp = boxplot.stats(x)[["stats"]]
  names(bxp) = c("ymin","lower", "middle","upper","ymax")
  return(bxp)
}  
# Function to use boxplot.stats for the outliers
myout = function(x) {
  data.frame(y=boxplot.stats(x)[["out"]])
}

fancy_scientific <- function(l) {
# turn in to character string in scientific notation
l <- format(l, scientific = TRUE)
# quote the part before the exponent to keep all the digits
l <- gsub("^(.*)e", "'\\1'e", l)
# turn the 'e+' into plotmath format
l <- gsub("e", "%*%10^", l)
# return this as an expression
parse(text=l)
}
                                         
jpeg(file="../Figures/TaxaEndangeredRanges_8April22_log.jpeg", height=3000, width=2000) 
ggplot(plotting, aes(x=factor(label), y=Area)) +
  stat_summary(fun.data=mybxp, geom="boxplot", fatten=0.5, aes(fill=label), lwd=2) +                                   
  stat_summary(fun.data=myout, geom="point", cex=3) +                 
  scale_fill_manual(values=c("deeppink3", "#DDCC77", "#999933", "#DDDDDD"))+  
  labs(x="", y="") +  #expression("Range size ("~km^2~")")
  #scale_y_continuous(breaks=c(1, 10, 100, 1000, 1e+4, 1e+5, 1e+6, 1e+7), labels=fancy_scientific, limits=c(1, 1e+7)) + 
  #scale_y_log10(breaks=trans_breaks('log10', function(x) 10^x),
  #                   labels=trans_format('log10', math_format(10^.x)))  +                     
  scale_y_continuous(trans = log10_trans(),
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)), 
    limits=c(0.1, 2e5)) + 
 #ylim(0, 2e5) +                      
  #coord_cartesian(ylim = c(0, log10(2e5))) +                      
  theme(plot.title = element_blank(), legend.position = "none", axis.title = element_text(size=85), 
    axis.text.y = element_text(size=100, angle=45), axis.text.x = element_blank(), 
    panel.background = element_rect(fill = "white", colour = "grey50"), plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"), 
    strip.text=element_blank())                 
dev.off()
