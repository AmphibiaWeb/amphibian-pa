# R language

# Plotting data resampled from WDPA network then subsampled with growth_tables.sh

# Establish working environment
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)                

# First, read *_growth.csv into R.  
huge_growth<-read.csv("huge_growth.csv")
huge_growth$cat<-"huge"
sub_large_growth<-read.csv("large_growth.csv")
sub_large_growth$cat<-"large"
sub_mid_growth<-read.csv("mid_growth.csv")
sub_mid_growth$cat<-"mid"
sub_small_growth<-read.csv("small_growth.csv")
sub_small_growth$cat<-"small"
sub_micro_growth<-read.csv("micro_growth.csv")
sub_micro_growth$cat<-"micro"

# Combine dataframes using rbind
combined_data <- rbind(huge_growth, sub_large_growth, sub_mid_growth, sub_small_growth, sub_micro_growth)
# Check the structure of the combined dataframe
str(combined_data)

# Group by 'cat' and summarize to get averages of numeric columns
averaged_data <- combined_data %>%
  group_by(cat) %>%
  summarize(across(where(is.numeric), mean))

# Melt the data for ggplot2
combined_data_melted <- melt(combined_data, id.vars = "cat")

jpeg("AccumulationCurve.jpg", width = 2000, height = 2000)               
ggplot(combined_data_melted, aes(x = variable, y = value, color = cat, group = cat)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +
  scale_color_manual(values = c("micro" = "#AA4499", "small" = "#CC6677", "mid" = "#999933", "large" = "#882255", "huge" = "#44AA99")) +
  labs(x = "Area added to the PA network", y = "Cumulative number of protected species") +
  scale_y_continuous(breaks = seq(0, 4000, by = 4000), limits = c(0, 4000)) +
  theme_minimal(base_size = 15) +
  theme(axis.title.x = element_text(margin = margin(t = 10)),  # Add margin to x-axis title
        axis.title.y = element_text(margin = margin(r = 10)),  # Add margin to y-axis title
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.background = element_blank(), 
        panel.background = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"))
dev.off()                  
