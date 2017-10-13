#######################################
#                                     #
#             Real CODE               #
#                                     #
#######################################
install.packages('RJSONIO')
install.packages('RCurl')
install.packages('rjson')
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
for (i in 1:61420){
  type <- data$features[[i]]["geometry"]$geometry$type
  Lng_coords <- data$features[[i]]["geometry"]$geometry$coordinates[1]
  Lat_coords <- data$features[[i]]["geometry"]$geometry$coordinates[2]
  label <- data$features[[i]]['properties']$properties['label_type']
  panorama <- data$features[[i]]["properties"]$properties['panorama_id']
  SideWalk.data <- rbind( SideWalk.data, data.frame(type, Lat_coords,Lng_coords, label, panorama))
}

write.csv(SideWalk.data, file = 'F:\\School\\Southern Methodist University\\MSDS 6120 Capstone\\SideWalkData\\SideWalkData_10.12.17.csv')
SideWalk.data <- read.csv('F:\\School\\Southern Methodist University\\MSDS 6120 Capstone\\SideWalkData\\SideWalkData_10.12.17.csv', stringsAsFactors = FALSE)


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
Width = 50

latDiff = lat1 - lat2
lonDiff = lon1 - lon2
latDelta = latDiff/Width
lonDelta = lonDiff/Width
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
for (i in 1:Width){
  for(j in 1:Width){
    Block_Id <- (i*Width)-Width+j
    
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
for (i in 1:Width){
  for(j in 1:Width){
    Block_Id <- (i*Width)-Width+j
    
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
for (i in 1:Width){
  for(j in 1:Width){
    Block_Id <- (i*Width)-Width+j
    
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

    Train.Crime.Agg<- rbind( Train.Crime.Agg, data.frame(Block_Id,Day,EVENING, MIDNIGHT,  MOTOR_VEHICLE_THEFT, ROBBERY, BURGLARY, HOMICIDE,SEX_ABUSE, ARSON))
  }
    
}

# TESTING DATA FOR CRIME

Test.Crime.Agg <- data.frame("Block_Id"=integer(), "Day"=integer(),"EVENING"=integer(),"MIDNIGHT"=integer(),"THEFT_F_AUTO"=integer(),"THEFT_OTHER"=integer(),"MOTOR_VEHICLE_THEFT"=integer(),"ROBBERY"=integer(),"ASSAULT_W_DANGEROUS_WEAPON"=integer(),"BURGLARY"=integer(),"HOMICIDE"=integer(),"SEX_ABUSE"=integer(),"ARSON"=integer(),stringsAsFactors = FALSE)
for (i in 1:Width){
  for(j in 1:Width){
    Block_Id <- (i*Width)-Width+j
    
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
    
    Test.Crime.Agg<- rbind( Test.Crime.Agg, data.frame(Block_Id,Day,EVENING, MIDNIGHT,  MOTOR_VEHICLE_THEFT, ROBBERY, BURGLARY, HOMICIDE,SEX_ABUSE, ARSON))
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
write.csv(Final_Data_Train, "\\Users\\natha_000\\Desktop\\Crime GeoCodes\\Final_Training_Sidewalk_Data-0.csv", row.names=FALSE)
write.csv(Final_Data_Test, "\\Users\\natha_000\\Desktop\\Crime GeoCodes\\Final_Testing_Sidewalk_Data-0.csv", row.names=FALSE)

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
drops11 <- "Other"
drops2 <- c("Block_Id","Other","Obstacle","NoCurbRamp","Occlusion","SurfaceProblem","CurbRamp")
drops12 <-"NoSidewalk"
drops3 <- c("Block_Id","Other","NoSidewalk","NoCurbRamp","Occlusion","SurfaceProblem","CurbRamp")
drops13 <- "Obstacle"
drops4 <- c("Block_Id","Other","NoSidewalk","Obstacle","Occlusion","SurfaceProblem","CurbRamp")
drops14 <- "NoCurbRamp"
drops5 <- c("Block_Id","Other","NoSidewalk","Obstacle","NoCurbRamp","SurfaceProblem","CurbRamp")
drops15 <- "Occlusion"
drops6 <- c("Block_Id","Other","NoSidewalk","Obstacle","NoCurbRamp","Occlusion","CurbRamp")
drops16 <- "SurfaceProblem"
drops7 <- c("Block_Id","Other","NoSidewalk","Obstacle","NoCurbRamp","Occlusion","SurfaceProblem")
drops17 <- "CurbRamp"

Test.Other <- Test1[ , !(names(Test1) %in% drops1)]
#Test.Other$Other.C <- ifelse(Test.Other$Other == 0, 0, ifelse((Test.Other$Other>=1)&(Test.Other$Other<=5),1,2))
Test.Other$Other.C <- ifelse(Test.Other$Other == 0, 0,1)
Test.Other <- Test.Other[ , !(names(Test.Other) %in% drops11)]
Test.NoSidewalk <- Test2[ , !(names(Test2) %in% drops2)]
#Test.NoSidewalk$NoSidewalk.C <- ifelse(Test.NoSidewalk$NoSidewalk == 0, 0, ifelse((Test.NoSidewalk$NoSidewalk>=1)&(Test.NoSidewalk$NoSidewalk<=5),1,2))
Test.NoSidewalk$NoSidewalk.C <- ifelse(Test.NoSidewalk$NoSidewalk == 0, 0, 1)
Test.NoSidewalk <- Test.NoSidewalk[ , !(names(Test.NoSidewalk) %in% drops12)]
Test.Obstacle <- Test3[ , !(names(Test3) %in% drops3)]
#Test.Obstacle$Obstacle.C <- ifelse(Test.Obstacle$Obstacle == 0, 0, ifelse((Test.Obstacle$Obstacle>=1)&(Test.Obstacle$Obstacle<=5),1,2))
Test.Obstacle$Obstacle.C <- ifelse(Test.Obstacle$Obstacle == 0, 0,1)
Test.Obstacle <- Test.Obstacle[ , !(names(Test.Obstacle) %in% drops13)]
Test.NoCurbRamp <- Test4[ , !(names(Test4) %in% drops4)]
#Test.NoCurbRamp$NoCurbRamp.C <- ifelse(Test.NoCurbRamp$NoCurbRamp == 0, 0, ifelse((Test.NoCurbRamp$NoCurbRamp>=1)&(Test.NoCurbRamp$NoCurbRamp<=5),1,2))
Test.NoCurbRamp$NoCurbRamp.C <- ifelse(Test.NoCurbRamp$NoCurbRamp == 0, 0,1)
Test.NoCurbRamp <- Test.NoCurbRamp[ , !(names(Test.NoCurbRamp) %in% drops14)]
Test.Occlusion <- Test5[ , !(names(Test5) %in% drops5)]
#Test.Occlusion$Occlusion.C <- ifelse(Test.Occlusion$Occlusion == 0, 0, ifelse((Test.Occlusion$Occlusion>=1)&(Test.Occlusion$Occlusion<=5),1,2))
Test.Occlusion$Occlusion.C <- ifelse(Test.Occlusion$Occlusion == 0, 0,1)
Test.Occlusion <- Test.Occlusion[ , !(names(Test.Occlusion) %in% drops15)]
Test.SurfaceProblem <- Test6[ , !(names(Test6) %in% drops6)]
#Test.SurfaceProblem$SurfaceProblem.C <- ifelse(Test.SurfaceProblem$SurfaceProblem == 0, 0, ifelse((Test.SurfaceProblem$SurfaceProblem>=1)&(Test.SurfaceProblem$SurfaceProblem<=5),1,2))
Test.SurfaceProblem$SurfaceProblem.C <- ifelse(Test.SurfaceProblem$SurfaceProblem == 0, 0,1)
Test.SurfaceProblem <- Test.SurfaceProblem[ , !(names(Test.SurfaceProblem) %in% drops16)]
Test.CurbRamp <- Test7[ , !(names(Test7) %in% drops7)]
#Test.CurbRamp$CurbRamp.C <- ifelse(Test.CurbRamp$CurbRamp == 0, 0, ifelse((Test.CurbRamp$CurbRamp>=1)&(Test.CurbRamp$CurbRamp<=5),1,2))
Test.CurbRamp$CurbRamp.C <- ifelse(Test.CurbRamp$CurbRamp == 0, 0, 1)
Test.CurbRamp <- Test.CurbRamp[ , !(names(Test.CurbRamp) %in% drops17)]

Train.Other <- Train1[ , !(names(Train1) %in% drops1)]
#Train.Other$Other.C <- ifelse(Train.Other$Other == 0, 0, ifelse((Train.Other$Other>=1)&(Train.Other$Other<=5),1,2))
Train.Other$Other.C <- ifelse(Train.Other$Other == 0, 0,1)
Train.Other <- Train.Other[ , !(names(Train.Other) %in% drops11)]
Train.NoSidewalk <- Train2[ , !(names(Train2) %in% drops2)]
#Train.NoSidewalk$NoSidewalk.C <- ifelse(Train.NoSidewalk$NoSidewalk == 0, 0, ifelse((Train.NoSidewalk$NoSidewalk>=1)&(Train.NoSidewalk$NoSidewalk<=5),1,2))
Train.NoSidewalk$NoSidewalk.C <- ifelse(Train.NoSidewalk$NoSidewalk == 0, 0, 1)
Train.NoSidewalk <- Train.NoSidewalk[ , !(names(Train.NoSidewalk) %in% drops12)]
Train.Obstacle <- Train3[ , !(names(Train3) %in% drops3)]
#Train.Obstacle$Obstacle.C <- ifelse(Train.Obstacle$Obstacle == 0, 0, ifelse((Train.Obstacle$Obstacle>=1)&(Train.Obstacle$Obstacle<=5),1,2))
Train.Obstacle$Obstacle.C <- ifelse(Train.Obstacle$Obstacle == 0, 0, 1)
Train.Obstacle <- Train.Obstacle[ , !(names(Train.Obstacle) %in% drops13)]
Train.NoCurbRamp <- Train4[ , !(names(Train4) %in% drops4)]
#Train.NoCurbRamp$NoCurbRamp.C <- ifelse(Train.NoCurbRamp$NoCurbRamp == 0, 0, ifelse((Train.NoCurbRamp$NoCurbRamp>=1)&(Train.NoCurbRamp$NoCurbRamp<=5),1,2))
Train.NoCurbRamp$NoCurbRamp.C <- ifelse(Train.NoCurbRamp$NoCurbRamp == 0, 0,1)
Train.NoCurbRamp <- Train.NoCurbRamp[ , !(names(Train.NoCurbRamp) %in% drops14)]
Train.Occlusion <- Train5[ , !(names(Train5) %in% drops5)]
#Train.Occlusion$Occlusion.C <- ifelse(Train.Occlusion$Occlusion == 0, 0, ifelse((Train.Occlusion$Occlusion>=1)&(Train.Occlusion$Occlusion<=5),1,2))
Train.Occlusion$Occlusion.C <- ifelse(Train.Occlusion$Occlusion == 0, 0, 1)
Train.Occlusion <- Train.Occlusion[ , !(names(Train.Occlusion) %in% drops15)]
Train.SurfaceProblem <- Train6[ , !(names(Train6) %in% drops6)]
#Train.SurfaceProblem$SurfaceProblem.C <- ifelse(Train.SurfaceProblem$SurfaceProblem == 0, 0, ifelse((Train.SurfaceProblem$SurfaceProblem>=1)&(Train.SurfaceProblem$SurfaceProblem<=5),1,2))
Train.SurfaceProblem$SurfaceProblem.C <- ifelse(Train.SurfaceProblem$SurfaceProblem == 0, 0, 1)
Train.SurfaceProblem <- Train.SurfaceProblem[ , !(names(Train.SurfaceProblem) %in% drops16)]
Train.CurbRamp <- Train7[ , !(names(Train7) %in% drops7)]
#Train.CurbRamp$CurbRamp.C <- ifelse(Train.CurbRamp$CurbRamp == 0, 0, ifelse((Train.CurbRamp$CurbRamp>=1)&(Train.CurbRamp$CurbRamp<=5),1,2))
Train.CurbRamp$CurbRamp.C <- ifelse(Train.CurbRamp$CurbRamp == 0, 0, 1)
Train.CurbRamp <- Train.CurbRamp[ , !(names(Train.CurbRamp) %in% drops17)]


#install.packages("randomForest")
#install.packages("miscTools")
#install.packages("party")
#install.packages("ROCR")
#install.packages("mlbench")
#install.packages("caret")
#install.packages("car")
#install.packages("stringi")
#install.packages("e1071")
library(randomForest)
library(miscTools)
library(mlbench)
library(caret)
library(ROCR)

# Train on Entire Crime Data Set and ONE Sidewalk column

##################
#  "Other"
##################
Train.Other$Other.C <- as.factor(Train.Other$Other.C)
set.seed(222)
table(Train.Other$Other.C)
cols <- names(Train.Other)[1:10]
clf <- randomForest(factor(Other.C) ~ ., data=Train.Other)
print(clf)
mtry <- tuneRF(Train.Other[-10],Train.Other$Other.C, 
               ntreeTry=300,
               stepFactor=1.5,
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

clf <- randomForest(factor(Other.C) ~ ., data=Train.Other[,cols], mtry=best.m, ntree=300, importance=TRUE, proximity = TRUE)
print(clf)
plot(clf)
# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Train.Other)
confusionMatrix(p2, Train.Other$Other.C)

# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.Other)
confusionMatrix(p2, Test.Other$Other.C)

# No. of Trees
hist(treesize(clf), main = "No. of Nodes for the Trees", col = "green")
#Var Importance
varImpPlot(clf, sort = T, n.var = 9,main = "Top 9 - Variable Importance")
importance(clf)

# Var actually used
varUsed(clf)

# Partial Dependence Plot
partialPlot(clf, Train.Other,ROBBERY, "1")

# Print out the Tree
getTree(clf,k = 1, labelVar = TRUE)

MDSplot(clf,Train.Other$Other.C)

#Calculate predictive probabilities of training dataset.
pred1=predict(clf,type = "prob",Train.Other)

#Evaluate the performance of the random forest for classification.
pred2 <- predict(clf,type = "prob",Train.Other)[,1]

#prediction is ROCR function
perf = prediction(pred2, Train.Other$Other.C)
length(pred2)

#1. Area under curve
auc = performance(perf,measure="tpr", x.measure="fpr")
plot(auc)
title(main = 'ROC for Other')

# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Train.Other) - 1))
{
  partialPlot(clf, Train.Other, names(Train.Other)[i], xlab = names(Train.Other)[i],
              main = NULL);
}


# Importances scaled to 100
(as.data.frame(clf$importanceSD)[2])*100/sum(as.data.frame(clf$importanceSD)[2]) 
#table(Test.NoSidewalk$NoSidewalk, predict(clf, Test.NoSidewalk[cols]))
sum(Train.Other$Other.C==predict(clf, Train.Other[cols])) / nrow(Train.Other)



############################################################################
##################
#  "NoSidewalk"
##################
cols <- names(Train.NoSidewalk)[1:10]
Train.NoSidewalk$NoSidewalk.C <- as.factor(Train.NoSidewalk$NoSidewalk.C)
set.seed(222)
system.time(clf <- randomForest(factor(NoSidewalk.C) ~ ., data=Train.NoSidewalk))
print(clf)
plot(clf)
mtry <- tuneRF(Train.NoSidewalk[-10],Train.NoSidewalk$NoSidewalk.C, ntreeTry=500,
               stepFactor=1.5,
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
clf <- randomForest(factor(NoSidewalk.C) ~ ., data=Train.NoSidewalk[,cols], mtry=best.m, ntree=500,importance=TRUE, proximity = TRUE, cutoff=c(.99,.01))
print(clf)

# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.NoSidewalk)
confusionMatrix(p2, Test.NoSidewalk$NoSidewalk.C)

# No. of Trees
hist(treesize(clf), main = "No. of Nodes for the Trees", col = "green")
#Var Importance
varImpPlot(clf, sort = T, n.var = 9,main = "Top 9 - Variable Importance")
importance(clf)

# Var actually used
varUsed(clf)

# Partial Dependence Plot
partialPlot(clf, Train.NoSidewalk,MOTOR_VEHICLE_THEFT, "1")

# Print out the Tree
getTree(clf,k = 1, labelVar = TRUE)

MDSplot(clf,Train.NoSidewalk$NoSidewalk.C)

#Calculate predictive probabilities of training dataset.
pred1=predict(clf,type = "prob",Test.NoSidewalk)

#Evaluate the performance of the random forest for classification.
pred2 <- predict(clf,type = "prob",Test.NoSidewalk)[,1]

#prediction is ROCR function
perf = prediction(pred2, Test.NoSidewalk$NoSidewalk.C)
length(pred2)

#1. Area under curve
auc = performance(perf,measure="tpr", x.measure="fpr")
plot(auc)
title(main = 'ROC Graph for NoSidewalk')

# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Train.NoSidewalk) - 1))
{
  partialPlot(clf, Train.NoSidewalk, names(Train.NoSidewalk)[i], xlab = names(Train.NoSidewalk)[i],
              main = NULL);
}


# Importances scaled to 100
(as.data.frame(clf$importanceSD)[1])*100/sum(as.data.frame(clf$importanceSD)[1]) 
#table(Test.NoSidewalk$NoSidewalk, predict(clf, Test.NoSidewalk[cols]))
sum(Test.NoSidewalk$NoSidewalk==predict(clf, Test.NoSidewalk[cols])) / nrow(Test.NoSidewalk)


##################
#  "Obstacle"
##################
cols <- names(Train.Obstacle)[1:10]
Train.Obstacle$Obstacle.C <- as.factor(Train.Obstacle$Obstacle.C)
set.seed(222)
system.time(clf <- randomForest(factor(Obstacle.C) ~ ., data=Train.Obstacle))
print(clf)
mtry <- tuneRF(Train.Obstacle[-10],Train.Obstacle$Obstacle.C, ntreeTry=500,
               stepFactor=1.5,
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
clf <- randomForest(factor(Obstacle.C) ~ ., data=Train.Obstacle[,cols], mtry=best.m, ntree=500,importance=TRUE, proximity = TRUE, cutoff=c(.99,.01))
print(clf)
plot(clf)
# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.Obstacle)
confusionMatrix(p2, Test.Obstacle$Obstacle.C)

# No. of Trees
hist(treesize(clf), main = "No. of Nodes for the Trees", col = "green")
#Var Importance
varImpPlot(clf, sort = T, n.var = 9,main = "Top 9 - Variable Importance")
importance(clf)

# Var actually used
varUsed(clf)

# Partial Dependence Plot
partialPlot(clf, Train.Obstacle,ROBBERY, "1")

# Print out the Tree
getTree(clf,k = 1, labelVar = TRUE)

MDSplot(clf,Train.Obstacle$Obstacle.C)

#Calculate predictive probabilities of training dataset.
pred1=predict(clf,type = "prob",Train.Obstacle)

#Evaluate the performance of the random forest for classification.
pred2 <- predict(clf,type = "prob",Test.Obstacle)[,1]

#prediction is ROCR function
perf = prediction(pred2, Test.Obstacle$Obstacle.C)
length(pred2)

#1. Area under curve
auc = performance(perf,measure="tpr", x.measure="fpr")
plot(auc)
title(main = 'ROC Graph for Obstacle')

# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Train.Obstacle) - 1))
{
  partialPlot(clf, Train.Obstacle, names(Train.Obstacle)[i], xlab = names(Train.Obstacle)[i],
              main = NULL);
}


# Importances scaled to 100
(as.data.frame(clf$importanceSD)[1])*100/sum(as.data.frame(clf$importanceSD)[1]) 
#table(Test.NoSidewalk$NoSidewalk, predict(clf, Test.NoSidewalk[cols]))
sum(Train.Obstacle$Obstacle.C==predict(clf, Train.Obstacle[cols])) / nrow(Train.Obstacle)

##################
#  "NoCurbRamp"
##################
cols <- names(Train.NoCurbRamp)[1:10]
Train.NoCurbRamp$NoCurbRamp.C <- as.factor(Train.NoCurbRamp$NoCurbRamp.C)
set.seed(222)
system.time(clf <- randomForest(factor(NoCurbRamp.C) ~ ., data=Train.NoCurbRamp))
print(clf)

p1 <- predict(clf,Train.NoCurbRamp)
confusionMatrix(p1, Train.NoCurbRamp$NoCurbRamp.C)

# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.NoCurbRamp)
confusionMatrix(p2, Test.NoCurbRamp$NoCurbRamp.C)

# Error rate of Random Forest
plot(clf)

# Tune mtry
mtry <- tuneRF(Train.NoCurbRamp[-10],Train.NoCurbRamp$NoCurbRamp.C, 
               ntreeTry=500,
               stepFactor=1.5,
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

clf <- randomForest(factor(NoCurbRamp.C) ~ ., data=Train.NoCurbRamp, mtry=best.m, ntree=500, importance=TRUE,proximity = TRUE, cutoff = c(.99, .01))
clf
# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.NoCurbRamp)
confusionMatrix(p2, Test.NoCurbRamp$NoCurbRamp.C)

# No. of Trees
hist(treesize(clf), main = "No. of Nodes for the Trees for NoCurbRamp", col = "green")

#Var Importance
varImpPlot(clf, sort = T, n.var = 9,main = "Top 9 - Variable Importance")
importance(clf)

# Var actually used
varUsed(clf)



# Partial Dependence Plot
partialPlot(clf, Train.NoCurbRamp,ROBBERY, "1")

# Print out the Tree
getTree(clf,k = 1, labelVar = TRUE)

MDSplot(clf,Train.NoCurbRamp$NoCurbRamp.C)

#Calculate predictive probabilities of training dataset.
pred1=predict(clf,type = "prob",Test.NoCurbRamp)

#Evaluate the performance of the random forest for classification.
pred2 <- predict(clf,type = "prob",Test.NoCurbRamp)[,1]



#prediction is ROCR function
perf = prediction(pred2, Test.NoCurbRamp$NoCurbRamp.C)
length(pred2)

#1. Area under curve
auc = performance(perf,measure="tpr", x.measure="fpr")
plot(auc)
title(main = 'ROC Graph for NoCurbRamp')
# Importances scaled to 100
(as.data.frame(clf$importanceSD)[1])*100/sum(as.data.frame(clf$importanceSD)[1]) 
#table(Test.NoCurbRamp$NoCurbRamp, predict(clf, Test.NoCurbRamp[cols]))
sum(Test.NoCurbRamp$NoCurbRamp==predict(clf, Test.NoCurbRamp[cols])) / nrow(Test.NoCurbRamp)

##################
#  "Occlusion"
##################
cols <- names(Train.Occlusion)[1:10]
Train.Occlusion$Occlusion.C <- as.factor(Train.Occlusion$Occlusion.C)
set.seed(222)
system.time(clf <- randomForest(factor(Occlusion.C) ~ ., data=Train.Occlusion))
print(clf)

# Error rate of Random Forest
plot(clf)
mtry <- tuneRF(Train.Occlusion[-10],Train.Occlusion$Occlusion.C, ntreeTry=500,
               stepFactor=1.5,
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
clf <- randomForest(factor(Occlusion.C) ~ ., data=Train.Occlusion[,cols], mtry=best.m, ntree=500,importance=TRUE, proximity = TRUE, cutoff=c(.99, .01))
print(clf)
plot(clf)
# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.Occlusion)
confusionMatrix(p2, Test.Occlusion$Occlusion.C)


# No. of Trees
hist(treesize(clf), main = "No. of Nodes for the Trees for Occlusion", col = "green")
#Var Importance
varImpPlot(clf, sort = T, n.var = 9,main = "Top 9 - Variable Importance")
importance(clf)

# Var actually used
varUsed(clf)

# Partial Dependence Plot
partialPlot(clf, Train.Occlusion,ROBBERY, "1")

# Print out the Tree
getTree(clf,k = 1, labelVar = TRUE)

MDSplot(clf,Train.Occlusion$Occlusion.C)

#Calculate predictive probabilities of training dataset.
pred1=predict(clf,type = "prob",Train.Occlusion)

#Evaluate the performance of the random forest for classification.
pred2 <- predict(clf,type = "prob",Test.Occlusion)[,1]

#prediction is ROCR function
perf = prediction(pred2, Test.Occlusion$Occlusion.C)
length(pred2)

#1. Area under curve
auc = performance(perf,measure="tpr", x.measure="fpr")
plot(auc)
title(main = 'ROC Graph for Occlusion')

# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Train.Occlusion) - 1))
{
  partialPlot(clf, Train.Occlusion, names(Train.Occlusion)[i], xlab = names(Train.Occlusion)[i],
              main = NULL);
}


# Importances scaled to 100
(as.data.frame(clf$importanceSD)[1])*100/sum(as.data.frame(clf$importanceSD)[1]) 
#table(Test.NoSidewalk$NoSidewalk, predict(clf, Test.NoSidewalk[cols]))
sum(Train.Occlusion$Occlusion.C==predict(clf, Train.Occlusion[cols])) / nrow(Train.Occlusion)


##################
#  "SurfaceProblem"
##################
cols <- names(Train.SurfaceProblem)[1:10]
cols
Train.SurfaceProblem$SurfaceProblem.C <- as.factor(Train.SurfaceProblem$SurfaceProblem.C)
set.seed(222)
system.time(clf <- randomForest(factor(SurfaceProblem.C) ~ ., data=Train.SurfaceProblem))
print(clf)
# Error rate of Random Forest
plot(clf)

mtry <- tuneRF(Train.SurfaceProblem[-10],Train.SurfaceProblem$SurfaceProblem.C, ntreeTry=500,
               stepFactor=1.5,
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
clf <- randomForest(factor(SurfaceProblem.C) ~ ., data=Train.SurfaceProblem[,cols], mtry=best.m, ntree=500,importance=TRUE, proximity = TRUE, cutoff=c(.99, .01))
print(clf)

# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.SurfaceProblem)
confusionMatrix(p2, Test.SurfaceProblem$SurfaceProblem.C)

# No. of Trees
hist(treesize(clf), main = "No. of Nodes for the Trees for SurfaceProblems", col = "green")
#Var Importance
varImpPlot(clf, sort = T, n.var = 9,main = "Top 9 - Variable Importance")
importance(clf)

# Var actually used
varUsed(clf)

# Partial Dependence Plot
partialPlot(clf, Train.SurfaceProblem,ROBBERY, "1")

# Print out the Tree
getTree(clf,k = 1, labelVar = TRUE)

MDSplot(clf,Train.SurfaceProblem$SurfaceProblem.C)

#Calculate predictive probabilities of training dataset.
pred1=predict(clf,type = "prob",Train.SurfaceProblem)

#Evaluate the performance of the random forest for classification.
pred2 <- predict(clf,type = "prob",Test.SurfaceProblem)[,1]

#prediction is ROCR function
perf = prediction(pred2, Test.SurfaceProblem$SurfaceProblem.C)
length(pred2)

#1. Area under curve
auc = performance(perf,measure="tpr", x.measure="fpr")
plot(auc)
title(main = 'ROC Graph for SurfaceProblems')

# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Train.SurfaceProblem) - 1))
{
  partialPlot(clf, Train.SurfaceProblem, names(Train.SurfaceProblem)[i], xlab = names(Train.SurfaceProblem)[i],
              main = NULL);
}


# Importances scaled to 100
(as.data.frame(clf$importanceSD)[1])*100/sum(as.data.frame(clf$importanceSD)[1]) 
#table(Test.NoSidewalk$NoSidewalk, predict(clf, Test.NoSidewalk[cols]))
sum(Train.SurfaceProblem$SurfaceProblem.C==predict(clf, Train.SurfaceProblem[cols])) / nrow(Train.SurfaceProblem)
               

##################
#  "CurbRamp"
##################
cols <- names(Train.CurbRamp)[1:10]
Train.CurbRamp$CurbRamp.C <- as.factor(Train.CurbRamp$CurbRamp.C)
set.seed(222)
clf <- randomForest(factor(CurbRamp.C) ~ ., data=Train.CurbRamp)
clf
plot(clf, main='No. of trees with Minimum Error for CurbRamp')

mtry <- tuneRF(Train.CurbRamp[-10],Train.CurbRamp$CurbRamp.C, ntreeTry=500,
               stepFactor=1.5,
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

clf <- randomForest(factor(CurbRamp.C) ~ ., data=Train.CurbRamp[,cols], mtry=best.m, ntree=500, importance=TRUE, proximity = TRUE, cutoff = c(.99, .01))
print(clf)
plot(clf, main='No. of trees with Minimum Error for CurbRamp')
# Prediction & Confusion Matrix - Test Data round 1
p2 <- predict(clf,Test.CurbRamp)
confusionMatrix(p2, Test.CurbRamp$CurbRamp.C)

# No. of Trees
hist(treesize(clf), main = "No. of Nodes for the Trees", col = "green")
#Var Importance
varImpPlot(clf, sort = T, n.var = 9,main = "Top 9 - Variable Importance")
importance(clf)

# Var actually used
varUsed(clf)

# Partial Dependence Plot
partialPlot(clf, Train.CurbRamp,MOTOR_VEHICLE_THEFT, "1")

# Print out the Tree
getTree(clf,k = 1, labelVar = TRUE)

MDSplot(clf,Train.CurbRamp$CurbRamp.C)

#Calculate predictive probabilities of training dataset.
pred1=predict(clf,type = "prob",Test.CurbRamp)

#Evaluate the performance of the random forest for classification.
pred2 <- predict(clf,type = "prob",Test.CurbRamp)[,1]

#prediction is ROCR function
perf = prediction(pred2, Test.CurbRamp$CurbRamp.C)
length(pred2)

#1. Area under curve
auc = performance(perf,measure="tpr", x.measure="fpr")
plot(auc)
title(main = 'ROC Graph for CurbRamp')
# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Train.CurbRamp) - 1))
{
  partialPlot(clf, Train.CurbRamp, names(Train.CurbRamp)[i], xlab = names(Train.CurbRamp)[i],
              main = NULL);
}

# Importances scaled to 100
(as.data.frame(clf$importanceSD)[1])*100/sum(as.data.frame(clf$importanceSD)[1]) 
#table(Test.CurbRamp$CurbRamp.C, predict(clf, Test.CurbRamp[cols]))
sum(Test.CurbRamp$CurbRamp.C==predict(clf, Test.CurbRamp[cols])) / nrow(Test.CurbRamp)

# Train On Entire Merged Data set
cols <- names(Final_Data_Train)[1:20]
system.time(clf <- randomForest(factor(NoCurbRamp) ~ ., data=Final_Data_Train[,cols], ntree=20, nodesize=5, mtry=9, importance=TRUE, cutoff = c(.001, 1, 1)))
clf$importance
importance(clf, type = 2)
importance(clf, type = 1, scale = FALSE)
varImpPlot(clf, type = 2)
varImpPlot(clf, type = 1)
as.data.frame(clf$importance)[1]
(as.data.frame(clf$importanceSD)[1])*100/sum(as.data.frame(clf$importanceSD)[1]) 

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


################################################################################################################

# Scatter Plots

plot(Test1$NoSidewalk, Test1$ROBBERY, main="Scatterplot Example", 
     xlab="sidewalk count ", ylab="Crime count ", pch=19)

abline(lm(Test1$ROBBERY ~Test1$NoSidewalk), col="red") # regression line (y~x) 
lines(lowess(Train.NoSidewalk$Train.NoSidewalk$BURGLARY), col="blue") # lowess line (x,y)
