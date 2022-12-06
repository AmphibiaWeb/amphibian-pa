#Exploratory Analyses of AWeb Conservation Working Group Data                                                                                                               #
#author: Emma Steigerwald                                                                                                                                                   #
#############################################################################################################################################################################

#Set up working environment
setwd("G:/Shared drives/AW-Data/ConservationWG/ConservationWG_GithubRepo/Data")
#Load libraries
library(dplyr) #yes
library(ggplot2) #yes
library(plyr) #yes
library(RColorBrewer)
library(stringr) #yes
library(cowplot) #NO
library(SciViews)
library(tidyr) #yes
library(forcats)
library(ggridges)
library(khroma) #Maybe use https://www.rdocumentation.org/packages/ggsci/versions/2.9 instead?
muted <- colour("muted")
library(lessR)
library(data.table) #yes
library(scales)
library(extrafont)
font_import()
           
#######################                   
#Comparing the range sizes of various taxa 
ranges <- read.csv("OtherTaxaRanges/All_taxa_ranges.csv")
ranges <- ranges[,c("binomial", "label", "category", "Area")] 
                   
#add bird iucn status to ranges
bird_status <- read.csv("OtherTaxaRanges/Bird_IUCNstatus.csv")     
bird_status<-bird_status[,1:2]
ranges<-setDT(ranges)[bird_status, category := i.category, on = .(binomial)]                   

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
ranges <- setDT(ranges)[Amph, category := AWiucn, on = .(binomial)]                    
                   
#All but bird ranges in m^2 (birds in km^2), so need to convert
ranges$Area[ranges$label!="birds"] <- ranges$Area[ranges$label!="birds"]/1000000                   

#Summarize by species binomial (many species are entered in csv multiple times, once for each country occurs in
ranges$binomial <- as.factor(ranges$binomial)                  
range_summ <- ranges %>%
  dplyr::group_by(binomial) %>%
  dplyr::summarise(label=label, category=category, Area = sum(Area)) 
ranges <- range_summ[!duplicated(range_summ), ]                                    
                   
#order by the median
range_sum <- ranges %>%
       group_by(label) %>%
       dplyr::summarize(m = median(Area, na.rm=TRUE)) %>%           
       #summarize_at(vars(Area), list(m=mean)) %>%
       arrange(desc(m)) %>%
       ungroup() %>%
       mutate(label=factor(label, unique(label)))
range_sum_1 <- left_join(ranges, range_sum, by=c("label"="label")) 
#remove plants
range_sum_1 <- range_sum_1[range_sum_1$label!="plants",]
                   
range_sum_1$label <- as.factor(range_sum_1$label)
range_sum_1 <- range_sum_1 %>%
       mutate(label = factor(label, levels = c("birds", "mammals", "reptiles", "amphibians")))
range_sum_1 <- na.omit(range_sum_1)      
                   
length(unique(range_sum_1$binomial[range_sum_1$label=="amphibians"]))                  
length(unique(range_sum_1$binomial[range_sum_1$label=="reptiles"]))                   
length(unique(range_sum_1$binomial[range_sum_1$label=="mammals"]))                   
length(unique(range_sum_1$binomial[range_sum_1$label=="birds"])) 
                   
#What are those median values for each taxon?
#amphibians=8,176    no species=7123
#reptiles=25,022      no species=8397
#mammals=160,425      no species=5850 
#birds- 306,335     no species=10487

#I shoudl annotate with max and min amphibian
range_sum_1$binomial[range_sum_1$Area==max(range_sum_1$Area[range_sum_1$label=="amphibians"])] 
#Salamandrella keyserlingii                   
range_sum_1$binomial[range_sum_1$Area==min(range_sum_1$Area[range_sum_1$label=="amphibians"])]                    
#Nectophrynoides asperginis
                   
#and plot it...                    
jpeg(file="../Figures/TaxaRanges_8Ap22.jpeg", height=1000, width=2000)                   
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
#Check ANOVA assumptions for complete dataset
#everything should be 'amphibian' or 'non-amphibian'
range_sum_1$group[range_sum_1$label=="amphibians"] <- "amphibian"
range_sum_1$group[range_sum_1$label!="amphibians"] <- "non-amphibian"                     
model <- aov(log10(Area) ~ group, data = range_sum_1)   

#ANOVA: Compare amphibian range sizes to other taxa                         
                     
#(1) response values should be normally distributed
hist(log10(range_sum_1$Area))
qqnorm(model$residuals)
qqline(model$residuals)           
shapiro.test(log10(range_sum_1$Area))
#can't deal with more than 5000 data entries, 
#Other tests of normality do not have this limitation                   
library(nortest)
ad.test(log10(range_sum_1$Area))  
#p-value = p-value < 0.00000000000000022   <- not normal                     
#In general, a one-way ANOVA is considered to be fairly robust against violations 
#of the normality assumption as long as the sample sizes are sufficiently large.
#Also, if you have extremely large sample sizes then statistical tests 
#like the Shapiro-Wilk test will almost always tell you that your data 
#is non-normal. For this reason, it’s often best to inspect your data 
#visually using graphs like histograms and Q-Q plots. By simply looking 
#at the graphs, you can get a pretty good idea of whether or not the data is normally distributed.
 
#(2) equal variance
#variance looks roughly the same from the boxplot
#Bartlett's Test of variance 
bartlett.test(log10(Area) ~ group, data = range_sum_1)  
#variances not equal enough: p-value < p-value = 0.001201   
#In general, a one-way ANOVA is considered to be fairly robust 
#against violations of the equal variances assumption as long as each group has the same sample size.
#However, if the sample sizes are not the same and this assumption is 
#severely violated, you could instead run a Kruskal-Wallis Test, 
# which is the non-parametric version of the one-way ANOVA.  
                     
#Do Kruskal-Wallis Test instead!  (sample sizes not equal)
kruskal.test(log10(Area) ~ label, data = range_sum_1)   
#Kruskal-Wallis chi-squared = 5923.8, df = 3, p-value <0.00000000000000022 
pairwise.wilcox.test(log10(range_sum_1$Area), range_sum_1$label,
                 p.adjust.method = "BH")
                     
#           birds               mammals             reptiles           
#mammals    <0.0000000000000002 -                   -                  
#reptiles   <0.0000000000000002 <0.0000000000000002 -                  
#amphibians <0.0000000000000002 <0.0000000000000002 <0.0000000000000002
 
wilcox.test(log10(Area) ~ group, data = range_sum_1)                        
#Wilcox rank sum text (amph vs non-amph): W = 75631991, p-value < 0.00000000000000022         
                     
range_sum_1$group_1[range_sum_1$category=="LC" | range_sum_1$category=="LC-provisional" | range_sum_1$category=="DD-provisional" | range_sum_1$category=="DD" | range_sum_1$category=="In assessment" | range_sum_1$category=="LC/cd" | range_sum_1$category=="NE" | range_sum_1$category=="NT" | range_sum_1$category=="NT-provisional"] <- "not_threatened"
range_sum_1$group_1[range_sum_1$category=="CR" | range_sum_1$category=="CR-provisional" | range_sum_1$category=="CR-Provisional" | range_sum_1$category=="EN" | range_sum_1$category=="EN-provisional" | range_sum_1$category=="VU" | range_sum_1$category=="VU-provisional"] <- "threatened"
#ANOVA: Compare endangered amphibian range sizes to amphibians generally 
model_1 <- aov(log10(Area) ~ group_1, data = range_sum_1[range_sum_1$label=="amphibians",])  
                     
#(1) response values should be normally distributed
hist(log10(range_sum_1$Area))
qqnorm(model_1$residuals)
qqline(model_1$residuals)           
shapiro.test(log10(range_sum_1$Area))
#can't deal with more than 5000 data entries, 
#Other tests of normality do not have this limitation                   
library(nortest)
ad.test(log10(range_sum_1$Area))  
#p-value = 0.00000000000000022   <- not normal                     
#In general, a one-way ANOVA is considered to be fairly robust against violations 
#of the normality assumption as long as the sample sizes are sufficiently large.
#Also, if you have extremely large sample sizes then statistical tests 
#like the Shapiro-Wilk test will almost always tell you that your data 
#is non-normal. For this reason, it’s often best to inspect your data 
#visually using graphs like histograms and Q-Q plots. By simply looking 
#at the graphs, you can get a pretty good idea of whether or not the data is normally distributed.
 
#(2) equal variance
#variance looks roughly the same from the boxplot
#Bartlett's Test of variance 
bartlett.test(log10(Area) ~ group_1, data = range_sum_1)  
#variances not equal enough: p-value < p-value = 0.001341  
#In general, a one-way ANOVA is considered to be fairly robust 
#against violations of the equal variances assumption as long as each group has the same sample size.
#However, if the sample sizes are not the same and this assumption is 
#severely violated, you could instead run a Kruskal-Wallis Test, 
# which is the non-parametric version of the one-way ANOVA.  
                     
wilcox.test(log10(Area) ~ group_1, data = range_sum_1)                        
#Wilcox rank sum text (amph threatened vs amphibian non-threatened): W = 123424924, p-value < 0.00000000000000022   
                    
###########################################################################                     
                     
#Now create a box and whisker plot for each taxon, showing range sizes for different iucn categories. (CR, EN, and VU only)
plotting <- transform(range_sum_1, label=factor(label,levels=c("birds", "mammals", "reptiles", "amphibians")))
plotting <-plotting[plotting$category=="CR" | plotting$category=="CR-provisional" | plotting$category=="CR-Provisional" | plotting$category=="EN" | plotting$category=="EN-provisional" | plotting$category=="VU-provisional" | plotting$category=="VU" | plotting$category=="EW"|  plotting$category=="EX"| plotting$category=="EX in the Wild",]                   
plotting <- plotting %>%
       mutate(label = factor(label, levels = c("amphibians", "reptiles", "mammals", "birds")))
#max y-lim value?
max(plotting$Area) 
#34,589,158                   
  
#Find the median value for annotation purposes
annot <- plotting %>%
       group_by(label) %>%
       dplyr::summarize(m = median(Area, na.rm=TRUE))  
#Mammals = 10,882
#reptiles=1,114
#amphibians=1,274
#birds=7,707

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


##################################
#Check ANOVA assumptions for endangered subset                     
#ANOVA: Compare endangered amphibian range sizes to those of other taxa                      
                      
model_3 <- aov(log10(Area) ~ label, data = plotting)   
model_4 <- aov(log10(Area) ~ group, data = plotting)                        
                     
#(1) response values should be normally distributed
hist(log10(plotting$Area))
qqnorm(model_3$residuals)
qqline(model_3$residuals)  
qqnorm(model_4$residuals)
qqline(model_4$residuals)                       
shapiro.test(log10(plotting$Area))
#can't deal with more than 5000 data entries, 
#Other tests of normality do not have this limitation                   
library(nortest)
ad.test(log10(plotting$Area))  
#p-value = p-value < 0.000008492   <- not normal                     
#In general, a one-way ANOVA is considered to be fairly robust against violations 
#of the normality assumption as long as the sample sizes are sufficiently large.
#Also, if you have extremely large sample sizes then statistical tests 
#like the Shapiro-Wilk test will almost always tell you that your data 
#is non-normal. For this reason, it’s often best to inspect your data 
#visually using graphs like histograms and Q-Q plots. By simply looking 
#at the graphs, you can get a pretty good idea of whether or not the data is normally distributed.
 
#(2) equal variance
#variance looks roughly the same from the boxplot
#Bartlett's Test of variance 
bartlett.test(log10(Area) ~ group, data = plotting)  
bartlett.test(log10(Area) ~ label, data = plotting)                       
#variances not equal enough: p-value < p-value = 0.00000000000000022   (for both)  
#In general, a one-way ANOVA is considered to be fairly robust 
#against violations of the equal variances assumption as long as each group has the same sample size.
#However, if the sample sizes are not the same and this assumption is 
#severely violated, you could instead run a Kruskal-Wallis Test, 
# which is the non-parametric version of the one-way ANOVA.  
                     
#Do Kruskal-Wallis Test instead!  (sample sizes not equal)
kruskal.test(log10(Area) ~ label, data = plotting)   
#Kruskal-Wallis chi-squared = 728.85, df = 3, p-value < 0.00000000000000022 
pairwise.wilcox.test(log10(plotting$Area), plotting$label,
                 p.adjust.method = "BH")
                     
#         amphibians          reptiles            mammals
#reptiles 0.158               -                   -      
#mammals  <0.0000000000000002 <0.0000000000000002 -      
#birds    <0.0000000000000002 <0.0000000000000002 0.009  
 
wilcox.test(log10(Area) ~ group, data = plotting)                        
#Wilcox rank sum text (amph vs non-amph): W = 75631991, p-value < 0.00000000000000022                      
                     
#################################
#Non-log-transformed

jpeg(file="../Figures/TaxaEndangeredRanges_11Mar22_recover.jpeg", height=3000, width=2000) 
ggplot(plotting, aes(x=factor(label), y=Area)) +
  geom_boxplot(aes(fill=label), lwd=2, outlier.size=4) +
  scale_fill_manual(values=c("deeppink3", "#DDCC77", "#999933", "#DDDDDD"))+  
  labs(x="", y="")+ #expression("Range size ("~km^2~")")  
  scale_y_continuous(breaks=seq(0, 1e5, by=20000), 
                    labels=c(0, "2e+4", "4e+4", "6e+4", "8e+4", "1e+5"), 
                    expand=c(0,0))+                    
  #Zoom into y-axis without losing values from the data analysis!                  
  coord_cartesian(ylim = c(0, 2e5)) +
  theme(plot.title = element_blank(), legend.position = "none", axis.title = element_text(size=85), 
          axis.text.y = element_text(size=85, angle=45), axis.text.x = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "grey50"), plot.margin=unit(c(0.1, 0.1, 0.1, 0.1),"cm"), 
         strip.text=element_blank())                 
dev.off()                     
                     
