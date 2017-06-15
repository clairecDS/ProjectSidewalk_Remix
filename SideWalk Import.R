install.packages("data.tree")
install.packages("jsonlite")
install.packages("magrittr")
library(data.tree)
library(jsonlite)
library(magrittr)

#######################################
#                                     #
#           EXAMPLE CODE              #
#                                     #
#######################################

reposLoL <- fromJSON("https://api.github.com/users/hadley/repos", simplifyDataFrame = FALSE)

library(data.tree)
repos <- as.Node(reposLoL)
print(repos, "id", "login")

#convert this to a data.frame
reposdf <- repos %>% ToDataFrameTable(ownerId = "id", 
                                  "login", 
                                  repoName = function(x) x$parent$name, #relative to the leaf
                                  fullName = "full_name", #unambiguous values are inherited from ancestors
                                  repoId = function(x) x$parent$id,
                                  "fork", 
                                  "type")

reposdf
#######################################
#                                     #
#             Real CODE               #
#                                     #
#######################################

RawSideWalkData <- fromJSON("https://sidewalk.umiacs.umd.edu/v1/access/score/streets?lat1=38.899&lng1=-77.008&lat2=38.920&lng2=-76.971", simplifyDataFrame = FALSE)

library(data.tree)
SideWalk <- as.Node(RawSideWalkData)
print(SideWalk, "score", "significance") #to test

#convert this to a data.frame
SideWalk_Final <- SideWalk %>% ToDataFrameTable(streetEdgeId = "street_edge_id", 
                                      "score", 
                                      NoCurbRamp = function(x) x$feature$NoCurbRamp,
                                      CurbRamp = function(x) x$feature$CurbRamp,
                                      Obstacle = function(x) x$feature$Obstacle,
                                      SurfaceProblem = function(x) x$feature$SurfaceProblem,
                                      Coordinates = function(x) x$geometry$coordinates, #relative to the leaf
                                      "type")

SideWalk_Final