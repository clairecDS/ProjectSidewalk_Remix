#######################################
#                                     #
#             Real CODE               #
#                                     #
#######################################
library(RJSONIO)
library(RCurl)
library(rjson)
# grab the data
raw_data <- getURL("https://sidewalk.umiacs.umd.edu/v1/access/features?lat1=38.999037&lng1=-77.150039&lat2=38.799492&lng2=-76.907179")
# Then covert from JSON into a list in R
data <- fromJSON(raw_data)
data

data$features[[101]]["geometry"]
data$features[[101]]["properties"]$properties['label_type']


SideWalk.data <- data.frame("type"=character(), "Lat_coords"=numeric(),"Lng_coords"=numeric(), "label"=character(), "panorama"=character(),stringsAsFactors = FALSE)
for (i in 1:45893){
  type <- data$features[[i]]["geometry"]$geometry$type
  Lng_coords <- data$features[[i]]["geometry"]$geometry$coordinates[1]
  Lat_coords <- data$features[[i]]["geometry"]$geometry$coordinates[2]
  label <- data$features[[i]]['properties']$properties['label_type']
  panorama <- data$features[[i]]["properties"]$properties['panorama_id']
  SideWalk.data <- rbind( SideWalk.data, data.frame(type, Lat_coords,Lng_coords, label, panorama))
}

write.csv(SideWalk.data, file = 'F:\\School\\Southern Methodist University\\MSDS 6120 Capstone\\SideWalkData\\SideWalkData_7.3.17.csv')



#######################################
#                                     #
#             Aggregate               #
#                                     #
#######################################

# The following code creates a 50x50 grid over Washington DC
# and aggregates each label_type per square.
# Dimensions for the grid are the same lat/lon from the API call above

lon1 = -77.150039
lon2 = -76.907179
lat1 = 38.999037
lat2 = 38.799492

latDiff = lat1 - lat2
lonDiff = lon1 - lon2
latDelta = latDiff/50
lonDelta = lonDiff/50
latDelta
lonDelta


SideWalk.Agg <- data.frame("Block_Id"=integer(), "Other"=integer(),"NoSidewalk"=integer(),"SurfaceProblem"=integer(),"Obstacle"=integer(),"NoCurbRamp"=integer(),"Occlusion"=integer(),"CurbRamp"=integer(),stringsAsFactors = FALSE)
for (i in 1:50){
  for(j in 1:50){
    Block_Id <- (i*50)-50+j
    
    newdata <- subset(SideWalk.data, Lat_coords < (lat1-latDelta*(j-1)), 
                      select=c(Lat_coords, Lng_coords,label_type))
    newdata2 <- subset(newdata, Lat_coords >(lat1-latDelta*(j)), 
                      select=c(Lat_coords, Lng_coords,label_type))
    newdata3 <- subset(newdata2, Lng_coords > (lon1-lonDelta*(i-1)), 
                       select=c(Lat_coords, Lng_coords,label_type))
    newdata4 <- subset(newdata3, Lng_coords < (lon1-lonDelta*(i)), 
                       select=c(Lat_coords, Lng_coords,label_type))
    
    Other <-length(which(newdata4$label_type == "Other"))
    NoSidewalk <-length(which(newdata4$label_type == "NoSidewalk"))
    SurfaceProblem <-length(which(newdata4$label_type == "SurfaceProblem"))
    Obstacle <-length(which(newdata4$label_type == "Obstacle"))
    NoCurbRamp <-length(which(newdata4$label_type == "NoCurbRamp"))
    Occlusion <-length(which(newdata4$label_type == "Occlusion"))
    CurbRamp <-length(which(newdata4$label_type == "CurbRamp"))

    SideWalk.Agg<- rbind( SideWalk.Agg, data.frame(Block_Id, Other,NoSidewalk, SurfaceProblem, Obstacle, NoCurbRamp, Occlusion, CurbRamp))
  }
}

