#######################################
#                                     #
#             Real CODE               #
#                                     #
#######################################

library(rjson)
# grab the data
raw_data <- getURL("https://sidewalk.umiacs.umd.edu/v1/access/features?lat1=38.892915&lng1=-76.909855&lat2=38.934588&lng2=-77.118938")
# Then covert from JSON into a list in R
data <- fromJSON(raw_data)
data

data$features[[101]]["geometry"]
data$features[[101]]["properties"]


SideWalk.data <- data.frame("type"=character(), "Lat_coords"=numeric(),"Lng_coords"=numeric(), "label"=character(), "panorama"=character(),stringsAsFactors = FALSE)
for (i in 1:14133){
  type <- data$features[[i]]["geometry"]$geometry$type
  Lng_coords <- data$features[[i]]["geometry"]$geometry$coordinates[1]
  Lat_coords <- data$features[[i]]["geometry"]$geometry$coordinates[2]
  label <- data$features[[i]]["properties"]$properties$label_type
  panorama <- data$features[[i]]["properties"]$properties$panorama_id
  SideWalk.data <- rbind( SideWalk.data, data.frame(type, Lat_coords,Lng_coords, label, panorama))
}
