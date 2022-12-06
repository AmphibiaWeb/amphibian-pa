#!/usr/bin/Rscript --vanilla

#Feed in command-line arguments
#where arg 1 is a list of PAs in a given size category
#and arg 2 is the name of the output file (tracking cumulative number of amphibian species with protected area)
args = commandArgs(trailingOnly=TRUE)

#Read in the substrate files
PA_list <- read.csv(args[1], row.names=NULL)
PA_list <- PA_list$X808
SpPA <- read.csv("SpPA.csv")
SpPA$WDPA_PID[SpPA$WDPA_PID == ""] <- "NotProtected"
wdpa <- read.csv("wdpa.csv")

#Fix a few columns
#We need to take out marine area from what we're considering as protected area size!
wdpa$GIS_T_AREA <-wdpa$GIS_AREA - wdpa$GIS_M_AREA
#Make status year numeric for following analysis
wdpa$STATUS_YR <- as.numeric(wdpa$STATUS_YR)
#Remove blank rows. 
wdpa <- wdpa[rowSums(is.na(wdpa)) != ncol(wdpa),]                   

growth<- data.frame(matrix(ncol = 1000, nrow = 20))#initiate a blank data frame                 
#Randomly sample from PIDs in tiny_list until their areas sum to sum(wdpa$Area)                     
#Establish the dataframe where I'll put number of species in each trial 
permut<-1 #start on the first permutation...    
                
while (permut <= 20) {  #Until we get to the 20th permutation
  growth[permut, 1]<-0 #the first value is always 0
  cum_ar <- 0       #reset cumulative area to 0
  cum_spec <- list() #reset a blank list of species  
  index <- 2
  while (isTRUE(cum_ar <= 27939673)){ #until the sum of reserves sampled add up to target area=27939673
    PA<-sample(PA_list, 1, replace = TRUE) #Grab a PA of the right size class
    cum_ar <- cum_ar + wdpa$GIS_T_AREA[wdpa$WDPA_PID==PA] #Add its area to cum_ar  
    new_spec<-SpPA$binomial[SpPA$WDPA_PID==PA] #Grab the list of species in that PA
    cum_spec<-union(cum_spec, new_spec) #Append to cumulative list of species but only keep unique values
    growth[permut, index] <- length(cum_spec) #Add this new value in!
    #print(paste0("We've completed index ",index, " for permutation ", permut)) 
    index <- index + 1 #move onto the next index   
  }  
  print(index)
  permut<-permut+1   #now we've completed the permut+1th permutation
  print(paste0("We've completed permutation number ",permut))  
}
write.csv(growth, args[2]) 
