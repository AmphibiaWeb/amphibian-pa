#!/usr/bin/Rscript --vanilla

# R script called on by growth_tables.sh in process of demonstrating how PA networks composed of PAs of different sizes
#       accumulate amphibian species

#Feed in command-line arguments
args = commandArgs(trailingOnly=TRUE)

# Read in the substrate files
PA_list <- read.csv(args[1], row.names=NULL)
PA_list <- PA_list$X808
SpPA <- read.csv("SpPA.csv")   # All_Amphibia with WDPA one to many.csv on data dryad
SpPA$WDPA_PID[SpPA$WDPA_PID == ""] <- "NotProtected"
wdpa <- read.csv("wdpa.csv")

#Fix a few columns
# Remove marine area from what we're considering as protected area size!
wdpa$GIS_T_AREA <-wdpa$GIS_AREA - wdpa$GIS_M_AREA
#Make status year numeric
wdpa$STATUS_YR <- as.numeric(wdpa$STATUS_YR)
#Remove blank rows. 
wdpa <- wdpa[rowSums(is.na(wdpa)) != ncol(wdpa),]                   

growth<- data.frame(matrix(ncol = 1000, nrow = 20))#initiate a blank data frame                 
#Randomly sample from PIDs in tiny_list until their areas sum to sum(wdpa$Area)                     
permut<-1 #start on the first permutation / run...    
                
while (permut <= 1000) {  #Until we get to the 1000th permutation
  # Depending on your system, you may need to run permutations for each size set in several batches
  growth[permut, 1]<-0 #the first value is always 0
  cum_ar <- 0       #reset cumulative area to 0
  cum_spec <- list() #reset a blank list of species  
  index <- 2
  while (isTRUE(cum_ar <= 27939673)){ #until the sum of reserves sampled add up to target area=27939673
    # this is the cumulative terrestrial area of protected areas in the WDPA database
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
