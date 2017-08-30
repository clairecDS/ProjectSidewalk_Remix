
library(RJSONIO)
library(RCurl)
library(rjson)
library(plyr)
require(RCurl)

# Grab the data
Sidewalk.housing <- getURL("https://sidewalk.umiacs.umd.edu/v1/access/score/neighborhoods?lat1=38.999037&lng1=-77.150039&lat2=38.799492&lng2=-76.907179")
HousingPrices <- read.csv(text=getURL("https://raw.githubusercontent.com/clairecDS/ProjectSidewalk_Remix/master/datasets/Neighborhood_MedianValuePerSqft_AllHomes_DC.csv"), header = TRUE)
HousingCoords <- read.csv(text=getURL("https://raw.githubusercontent.com/clairecDS/ProjectSidewalk_Remix/master/datasets/neighborhood.csv"), header = TRUE)


# Clean the data
colnames(HousingPrices)[1] <- "id"
Merged_House2 <-merge(HousingCoords, HousingPrices,  by = "id")
Merged_House2 <- subset(Merged_House2, select=-RegionName) #remove duplicates

names(Merged_House2)

Sidewalk.housingDF<- fromJSON(Sidewalk.housing)
SideWalk.housing.data <- data.frame("region_id"=numeric(),"region_name"=character(), "Score"=numeric(), "Coverage"=numeric(),"CurbRamp"=numeric(),"NoCurbRamp"=numeric(),"Obstacle"=numeric(),"SurfaceProblem"=numeric() ,"CurbRampSig"=numeric(),"NoCurbRampSig"=numeric(),"ObstacleSig"=numeric(),"SurfaceProblemSig"=numeric() ,stringsAsFactors = FALSE)
for (i in 1:178){
  region_name <- Sidewalk.housingDF$features[[i]]["properties"]$properties['region_name']
  region_id<- Sidewalk.housingDF$features[[i]]["properties"]$properties['region_id']
  Score <- Sidewalk.housingDF$features[[i]]["properties"]$properties['score']
  Coverage<- Sidewalk.housingDF$features[[i]]["properties"]$properties['coverage']
  
  CurbRamp<- Sidewalk.housingDF$features[[i]]["properties"]$properties$feature$CurbRamp
  NoCurbRamp<- Sidewalk.housingDF$features[[i]]["properties"]$properties$feature$NoCurbRamp
  Obstacle<- Sidewalk.housingDF$features[[i]]["properties"]$properties$feature$Obstacle
  SurfaceProblem <- Sidewalk.housingDF$features[[i]]["properties"]$properties$feature$SurfaceProblem
  
  CurbRampSig <- Sidewalk.housingDF$features[[i]]["properties"]$properties$significance$CurbRamp
  NoCurbRampSig <- Sidewalk.housingDF$features[[i]]["properties"]$properties$significance$NoCurbRamp
  ObstacleSig <- Sidewalk.housingDF$features[[i]]["properties"]$properties$significance$Obstacle
  SurfaceProblemSig <- Sidewalk.housingDF$features[[i]]["properties"]$properties$significance$SurfaceProblem
  
  SideWalk.housing.data <- rbind( SideWalk.housing.data, data.frame(region_id, region_name, Score, Coverage, CurbRamp, NoCurbRamp, Obstacle, SurfaceProblem, CurbRampSig, NoCurbRampSig, ObstacleSig, SurfaceProblemSig))
}

write.csv(SideWalk.housing.data, file = 'F:\\School\\Southern Methodist University\\MSDS 6120 Capstone\\SideWalkData\\SideWalk.housing.data_8.29.17.csv')

Final_Merged_Housing_data <-merge(SideWalk.housing.data, Merged_House2,  by = "region_name")

# More Cleaning - Check for factor levels < 2
(l <- sapply(Final_Merged_Housing_data, function(x) is.factor(x)))
m <- Final_Merged_Housing_data[, l]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "Dont_DROP")
Final_Merged_Housing_data <- subset(Final_Merged_Housing_data, select=-c(City,State,Metro,CountyName)) #remove single level factors
Final_Merged_Housing_data[,18:197]<- list(NULL)
write.csv(Final_Merged_Housing_data, file = 'F:\\School\\Southern Methodist University\\MSDS 6120 Capstone\\SideWalkData\\Final_Merged_Housing_data_8.29.17.csv')

# Regression prediction SurfaceProblem

# Multiple Linear Regression Example 
fit <- lm(SurfaceProblem ~ ., data=Final_Merged_Housing_data)
summary(fit) # show results

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

