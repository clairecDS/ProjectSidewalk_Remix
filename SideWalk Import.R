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

#######################################
#   Create Training and Test Data     #
#######################################

#Sample Indexes
indexes = sample(1:nrow(SideWalk.data), size=0.2*nrow(SideWalk.data))

# Split data
Sidewalk.test = SideWalk.data[indexes,]
dim(Sidewalk.test) 
Sidewalk.train = SideWalk.data[-indexes,]
dim(Sidewalk.train)

# TRAINING DATA FOR SIDEWALK

Sidewalk.train.Agg <- data.frame("Block_Id"=integer(), "Other"=integer(),"NoSidewalk"=integer(),"SurfaceProblem"=integer(),"Obstacle"=integer(),"NoCurbRamp"=integer(),"Occlusion"=integer(),"CurbRamp"=integer(),stringsAsFactors = FALSE)
for (i in 1:50){
  for(j in 1:50){
    Block_Id <- (i*50)-50+j
    
    newdata <- subset(Sidewalk.train, Lat_coords < (lat1-latDelta*(j-1)), 
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

    Sidewalk.train.Agg<- rbind( Sidewalk.train.Agg, data.frame(Block_Id, Other,NoSidewalk, SurfaceProblem, Obstacle, NoCurbRamp, Occlusion, CurbRamp))
  }
}

# TESTING DATA FOR SIDEWALK

Sidewalk.test.Agg <- data.frame("Block_Id"=integer(), "Other"=integer(),"NoSidewalk"=integer(),"SurfaceProblem"=integer(),"Obstacle"=integer(),"NoCurbRamp"=integer(),"Occlusion"=integer(),"CurbRamp"=integer(),stringsAsFactors = FALSE)
for (i in 1:50){
  for(j in 1:50){
    Block_Id <- (i*50)-50+j
    
    newdata <- subset(Sidewalk.test, Lat_coords < (lat1-latDelta*(j-1)), 
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
    
    Sidewalk.test.Agg<- rbind( Sidewalk.test.Agg, data.frame(Block_Id, Other,NoSidewalk, SurfaceProblem, Obstacle, NoCurbRamp, Occlusion, CurbRamp))
  }
}

#######################################
#                                     #
#         Crime Aggregate             #
#                                     #
#######################################

# The following code creates a 50x50 grid over Washington DC
# and aggregates each Offense per square.
# Dimensions for the grid are the same lat/lon from the API call above

Crime.data <- read.csv('F:\\School\\Southern Methodist University\\MSDS 6120 Capstone\\SideWalkData\\Combined_2501-END.csv', stringsAsFactors = FALSE)
Crime.data$OFFENSE <- gsub("/", '_', Crime.data$OFFENSE)

#######################################
#   Create Training and Test Data     #
#######################################

#Sample Indexes
indexes = sample(1:nrow(Crime.data), size=0.2*nrow(Crime.data))

# Split data
test = Crime.data[indexes,]
dim(test) 
train = Crime.data[-indexes,]
dim(train)


# TRAINING DATA FOR CRIME

Train.Crime.Agg <- data.frame("Block_Id"=integer(), "Day"=integer(),"EVENING"=integer(),"MIDNIGHT"=integer(),"THEFT_F_AUTO"=integer(),"THEFT_OTHER"=integer(),"MOTOR_VEHICLE_THEFT"=integer(),"ROBBERY"=integer(),"ASSAULT_W_DANGEROUS_WEAPON"=integer(),"BURGLARY"=integer(),"HOMICIDE"=integer(),"SEX_ABUSE"=integer(),"ARSON"=integer(),stringsAsFactors = FALSE)
for (i in 1:50){
  for(j in 1:50){
    Block_Id <- (i*50)-50+j
    
    newdata <- subset(train, lat < (lat1-latDelta*(j-1)), 
                      select=c(lat, lon,SHIFT, OFFENSE))
    newdata2 <- subset(newdata, lat >(lat1-latDelta*(j)), 
                       select=c(lat, lon, SHIFT,OFFENSE))
    newdata3 <- subset(newdata2, lon > (lon1-lonDelta*(i-1)), 
                       select=c(lat, lon, SHIFT,OFFENSE))
    newdata4 <- subset(newdata3, lon < (lon1-lonDelta*(i)), 
                       select=c(lat, lon, SHIFT,OFFENSE))
    
    #REPORT_DAT <-length(which(newdata4$label_type == "Other"))
    Day <-length(which(newdata4$SHIFT == "DAY"))
    EVENING <-length(which(newdata4$SHIFT == "EVENING"))
    MIDNIGHT <-length(which(newdata4$SHIFT == "MIDNIGHT"))
    THEFT_F_AUTO <-length(which(newdata4$OFFENSE == "THEFT F/AUTO"))
    THEFT_OTHER <-length(which(newdata4$OFFENSE == "THEFT/OTHER"))
    MOTOR_VEHICLE_THEFT <-length(which(newdata4$OFFENSE == "MOTOR VEHICLE THEFT"))
    ASSAULT_W_DANGEROUS_WEAPON <-length(which(newdata4$OFFENSE == "ASSAULT W/DANGEROUS WEAPON"))
    BURGLARY <-length(which(newdata4$OFFENSE == "BURGLARY"))
    HOMICIDE <-length(which(newdata4$OFFENSE == "HOMICIDE"))
    SEX_ABUSE <-length(which(newdata4$OFFENSE == "SEX ABUSE"))
    ROBBERY <-length(which(newdata4$OFFENSE == "ROBBERY"))
    ARSON <-length(which(newdata4$OFFENSE == "ARSON"))

    Train.Crime.Agg<- rbind( Train.Crime.Agg, data.frame(Block_Id,Day,EVENING, MIDNIGHT, THEFT_F_AUTO, THEFT_OTHER, MOTOR_VEHICLE_THEFT, ROBBERY,ASSAULT_W_DANGEROUS_WEAPON, BURGLARY, HOMICIDE,SEX_ABUSE, ARSON))
  }
    
}

# TESTING DATA FOR CRIME

Test.Crime.Agg <- data.frame("Block_Id"=integer(), "Day"=integer(),"EVENING"=integer(),"MIDNIGHT"=integer(),"THEFT_F_AUTO"=integer(),"THEFT_OTHER"=integer(),"MOTOR_VEHICLE_THEFT"=integer(),"ROBBERY"=integer(),"ASSAULT_W_DANGEROUS_WEAPON"=integer(),"BURGLARY"=integer(),"HOMICIDE"=integer(),"SEX_ABUSE"=integer(),"ARSON"=integer(),stringsAsFactors = FALSE)
for (i in 1:50){
  for(j in 1:50){
    Block_Id <- (i*50)-50+j
    
    newdata <- subset(test, lat < (lat1-latDelta*(j-1)), 
                      select=c(lat, lon,SHIFT, OFFENSE))
    newdata2 <- subset(newdata, lat >(lat1-latDelta*(j)), 
                       select=c(lat, lon, SHIFT,OFFENSE))
    newdata3 <- subset(newdata2, lon > (lon1-lonDelta*(i-1)), 
                       select=c(lat, lon, SHIFT,OFFENSE))
    newdata4 <- subset(newdata3, lon < (lon1-lonDelta*(i)), 
                       select=c(lat, lon, SHIFT,OFFENSE))
    
    #REPORT_DAT <-length(which(newdata4$label_type == "Other"))
    Day <-length(which(newdata4$SHIFT == "DAY"))
    EVENING <-length(which(newdata4$SHIFT == "EVENING"))
    MIDNIGHT <-length(which(newdata4$SHIFT == "MIDNIGHT"))
    THEFT_F_AUTO <-length(which(newdata4$OFFENSE == "THEFT F/AUTO"))
    THEFT_OTHER <-length(which(newdata4$OFFENSE == "THEFT/OTHER"))
    MOTOR_VEHICLE_THEFT <-length(which(newdata4$OFFENSE == "MOTOR VEHICLE THEFT"))
    ASSAULT_W_DANGEROUS_WEAPON <-length(which(newdata4$OFFENSE == "ASSAULT W/DANGEROUS WEAPON"))
    BURGLARY <-length(which(newdata4$OFFENSE == "BURGLARY"))
    HOMICIDE <-length(which(newdata4$OFFENSE == "HOMICIDE"))
    SEX_ABUSE <-length(which(newdata4$OFFENSE == "SEX ABUSE"))
    ROBBERY <-length(which(newdata4$OFFENSE == "ROBBERY"))
    ARSON <-length(which(newdata4$OFFENSE == "ARSON"))
    
    Test.Crime.Agg<- rbind( Test.Crime.Agg, data.frame(Block_Id,Day,EVENING, MIDNIGHT, THEFT_F_AUTO, THEFT_OTHER, MOTOR_VEHICLE_THEFT, ROBBERY,ASSAULT_W_DANGEROUS_WEAPON, BURGLARY, HOMICIDE,SEX_ABUSE, ARSON))
  }
  
}
summary(SideWalk.Agg)
sum(SideWalk.Agg$NoSidewalk)


#######################################
#                                     #
#         MACHINE LEARNING            #
#                                     #
#######################################
Final_Data_Train <-merge(Sidewalk.train.Agg, Train.Crime.Agg, by = "Block_Id")
Final_Data_Test <-merge(Sidewalk.test.Agg, Test.Crime.Agg, by = "Block_Id")
write.csv(Final_Data_Train, "\\Users\\natha_000\\Desktop\\Crime GeoCodes\\Final_Training_Sidewalk_Data.csv", row.names=FALSE)
write.csv(Final_Data_Test, "\\Users\\natha_000\\Desktop\\Crime GeoCodes\\Final_Testing_Sidewalk_Data.csv", row.names=FALSE)

Test1 <- Final_Data_Test
Test2 <- Final_Data_Test
Test3 <- Final_Data_Test
Test4 <- Final_Data_Test
Test5 <- Final_Data_Test
Test6 <- Final_Data_Test
Test7 <- Final_Data_Test
Train1 <- Final_Data_Train
Train2 <- Final_Data_Train
Train3 <- Final_Data_Train
Train4 <- Final_Data_Train
Train5 <- Final_Data_Train
Train6 <- Final_Data_Train
Train7 <- Final_Data_Train

# For Accuracy Test2 leave in "Block_Id", and for Accuracy Test3 Remove it
drops1 <- c("Block_Id","NoSidewalk","Obstacle","NoCurbRamp","Occlusion","SurfaceProblem","CurbRamp")
drops2 <- c("Block_Id","Other","Obstacle","NoCurbRamp","Occlusion","SurfaceProblem","CurbRamp")
drops3 <- c("Block_Id","Other","NoSidewalk","NoCurbRamp","Occlusion","SurfaceProblem","CurbRamp")
drops4 <- c("Block_Id","Other","NoSidewalk","Obstacle","Occlusion","SurfaceProblem","CurbRamp")
drops5 <- c("Block_Id","Other","NoSidewalk","Obstacle","NoCurbRamp","SurfaceProblem","CurbRamp")
drops6 <- c("Block_Id","Other","NoSidewalk","Obstacle","NoCurbRamp","Occlusion","CurbRamp")
drops7 <- c("Block_Id","Other","NoSidewalk","Obstacle","NoCurbRamp","Occlusion","SurfaceProblem")

Test.Other <- Test1[ , !(names(Test1) %in% drops1)]
Test.NoSidewalk <- Test2[ , !(names(Test2) %in% drops2)]
Test.Obstacle <- Test3[ , !(names(Test3) %in% drops3)]
Test.NoCurbRamp <- Test4[ , !(names(Test4) %in% drops4)]
Test.Occlusion <- Test5[ , !(names(Test5) %in% drops5)]
Test.SurfaceProblem <- Test6[ , !(names(Test6) %in% drops6)]
Test.CurbRamp <- Test7[ , !(names(Test7) %in% drops7)]

Train.Other <- Train1[ , !(names(Train1) %in% drops1)]
Train.NoSidewalk <- Train2[ , !(names(Train2) %in% drops2)]
Train.Obstacle <- Train3[ , !(names(Train3) %in% drops3)]
Train.NoCurbRamp <- Train4[ , !(names(Train4) %in% drops4)]
Train.Occlusion <- Train5[ , !(names(Train5) %in% drops5)]
Train.SurfaceProblem <- Train6[ , !(names(Train6) %in% drops6)]
Train.CurbRamp <- Train7[ , !(names(Train7) %in% drops7)]

install.packages("randomForest")
install.packages("miscTools")
library(randomForest)
library(miscTools)

# Train on Entire Crime Data Set and ONE Sidewalk column
cols <- names(Train.Other)[1:13]
system.time(clf <- randomForest(factor(Other) ~ ., data=Train.Other[,cols], ntree=20, nodesize=5, mtry=9))
#table(Test.Other$Other, predict(clf, Test.Other[cols]))
sum(Test.Other$Other==predict(clf, Test.Other[cols])) / nrow(Test.Other)


cols <- names(Train.NoSidewalk)[1:13]
system.time(clf <- randomForest(factor(NoSidewalk) ~ ., data=Train.NoSidewalk[,cols], ntree=20, nodesize=5, mtry=9))
#table(Test.NoSidewalk$NoSidewalk, predict(clf, Test.NoSidewalk[cols]))
sum(Test.NoSidewalk$NoSidewalk==predict(clf, Test.NoSidewalk[cols])) / nrow(Test.NoSidewalk)


cols <- names(Train.Obstacle)[1:13]
system.time(clf <- randomForest(factor(Obstacle) ~ ., data=Train.Obstacle[,cols], ntree=20, nodesize=5, mtry=9))
#table(Test.Obstacle$Obstacle, predict(clf, Test.Obstacle[cols]))
sum(Test.Obstacle$Obstacle==predict(clf, Test.Obstacle[cols])) / nrow(Test.Obstacle)


cols <- names(Train.NoCurbRamp)[1:13]
system.time(clf <- randomForest(factor(NoCurbRamp) ~ ., data=Train.NoCurbRamp[,cols], ntree=20, nodesize=5, mtry=9))
#table(Test.NoCurbRamp$NoCurbRamp, predict(clf, Test.NoCurbRamp[cols]))
sum(Test.NoCurbRamp$NoCurbRamp==predict(clf, Test.NoCurbRamp[cols])) / nrow(Test.NoCurbRamp)


cols <- names(Train.Occlusion)[1:13]
system.time(clf <- randomForest(factor(Occlusion) ~ ., data=Train.Occlusion[,cols], ntree=20, nodesize=5, mtry=9))
#table(Test.Occlusion$Occlusion, predict(clf, Test.Occlusion[cols]))
sum(Test.Occlusion$Occlusion==predict(clf, Test.Occlusion[cols])) / nrow(Test.Occlusion)


cols <- names(Train.SurfaceProblem)[1:13]
system.time(clf <- randomForest(factor(SurfaceProblem) ~ ., data=Train.SurfaceProblem[,cols], ntree=20, nodesize=5, mtry=9))
#table(Test.SurfaceProblem$SurfaceProblem, predict(clf, Test.SurfaceProblem[cols]))
sum(Test.SurfaceProblem$SurfaceProblem==predict(clf, Test.SurfaceProblem[cols])) / nrow(Test.SurfaceProblem)


cols <- names(Train.CurbRamp)[1:13]
system.time(clf <- randomForest(factor(CurbRamp) ~ ., data=Train.CurbRamp[,cols], ntree=20, nodesize=5, mtry=9))
#table(Test.CurbRamp$CurbRamp, predict(clf, Test.CurbRamp[cols]))
sum(Test.CurbRamp$CurbRamp==predict(clf, Test.CurbRamp[cols])) / nrow(Test.CurbRamp)

# Train On Entire Merged Data set
cols <- names(Final_Data_Train)[1:20]
system.time(clf <- randomForest(factor(NoCurbRamp) ~ ., data=Final_Data_Train[,cols], ntree=20, nodesize=5, mtry=9))


# Predict "NoCurbRamp"
#table(Final_Data_Test$NoCurbRamp, predict(clf, Final_Data_Test[cols]))
sum(Final_Data_Test$NoCurbRamp==predict(clf, Final_Data_Test[cols])) / nrow(Final_Data_Test)

# Predict "Obstacle"
#table(Final_Data_Test$Obstacle, predict(clf, Final_Data_Test[cols]))
sum(Final_Data_Test$Obstacle==predict(clf, Final_Data_Test[cols])) / nrow(Final_Data_Test)

# Predict "NoSidewalk"
#table(Final_Data_Test$NoSidewalk, predict(clf, Final_Data_Test[cols]))
sum(Final_Data_Test$NoSidewalk==predict(clf, Final_Data_Test[cols])) / nrow(Final_Data_Test)

# Predict "Occlusion"
#table(Final_Data_Test$Occlusion, predict(clf, Final_Data_Test[cols]))
sum(Final_Data_Test$Occlusion==predict(clf, Final_Data_Test[cols])) / nrow(Final_Data_Test)

# Predict "Other"
#table(Final_Data_Test$Other, predict(clf, Final_Data_Test[cols]))
sum(Final_Data_Test$Other==predict(clf, Final_Data_Test[cols])) / nrow(Final_Data_Test)

# Predict "CurbRamp"
#table(Final_Data_Test$CurbRamp, predict(clf, Final_Data_Test[cols]))
sum(Final_Data_Test$CurbRamp==predict(clf, Final_Data_Test[cols])) / nrow(Final_Data_Test)

# Predict "SurfaceProblem"
#table(Final_Data_Test$SurfaceProblem, predict(clf, Final_Data_Test[cols]))
sum(Final_Data_Test$SurfaceProblem==predict(clf, Final_Data_Test[cols])) / nrow(Final_Data_Test)

##################################### UNTESTED CODE BELOW THIS LINE ############################################
################################################################################################################

library(randomForest)
library(miscTools)
library(ggplot2)

cor(Final_Data_Train)
rf <- randomForest(alcohol ~ ., data=train[,cols], ntree=20)

(r2 <- rSquared(test$alcohol, test$alcohol - predict(rf, test[,cols])))
# [1] 0.6481
(mse <- mean((test$alcohol - predict(rf, test[,cols]))^2))
# [1] 0.6358

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=test$alcohol, pred=predict(rf, test[,cols])))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))
