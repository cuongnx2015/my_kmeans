# Algorithm (iterative approximation) 
# (1) Random initialization of K cluster centers 
# (2) Assign each data point the closest cluster 
# (3) Estimate the cluster center according to the re-assignments
# (4) Repeat until cluster centers are no longer changing
# if changing: go to (2)

####### CLEAR ENVIRONMENT AND SET WD
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####### LIBRARIES
library(ggplot2)


####### DATA PREP
data <- read.csv("data/persons_by_height_weight.csv", header=F)

data <- data[,c(1, 2)]
data <- as.matrix(data)
colnames(data) <- NULL

plot(data)



####### FUNCTION DEFINITION BLUEPRINT
# myfunction <- function(arg1, arg2, ... ){
#   statements
#   return(object)
# }

####### DEFINE MY K-MEANS
myKMeans <- function(df, k){
  # (1) Random initialization of K cluster centers 
  dimensions <- ncol(df)
  examples <- nrow(df)
  
  # get min and max value for each column
  limits <- c()
  for (i in c(1:dimensions)){
    minVal <- min(df[,i])
    maxVal <- max(df[,i])
    limits <- c(limits, c(minVal, maxVal))
  }
  limits <- matrix(limits, byrow=T,ncol=2)
  #print(limits)
  
  #create k randonly initialized centroids
  centroids <- c()
  for (i in c(1:k)) {
    for (j in c(1:dimensions)){
      #cat("Min Value : " ,min(limits[j, 1]), "\n")
      #cat("Max Value: " ,max(limits[j, 2]), "\n")
      # random point between min and max value of column
      centroids <- c(centroids, runif(1, min=limits[j,1],
                     max=limits[j, 2]))
    }
  }
  centroids <- matrix(centroids, byrow=T, ncol=dimensions, nrow=k)
  #print(centroids)
  
  while (TRUE) {
  
    euclDistances <- c()
    # measure the distance
    for (i in c(1:k)) {
      # select centroid matrix and compute distance for each axis
      selectedCentroidMatrix <- matrix(rep(centroids[i,], examples),
                                       byrow=T, nrow=examples)
      distancesPerAxis <- df - selectedCentroidMatrix
      #print(distancesPerAxis)
      # calulate euclidian distance by sqrt of sum of sq distances
      euclDistPerCentroid <- sqrt(rowSums((distancesPerAxis**2)))
      euclDistances <- c(euclDistances, euclDistPerCentroid)
    }
    euclDistances <- matrix(euclDistances, nrow=examples, ncol=k)
    #print(euclDistances)
    
    # (2) Assign each data point the closest cluster
    # which.min returns die index of the lowest column
    clusters <- apply(euclDistances, 1, which.min)
    
    # (3) Estimate the cluster center according to the re-assignments
    meansPerCluster <- aggregate(df, by=list(clusters), FUN='mean')
    # remove group column
    meansPerCluster <- as.matrix(meansPerCluster[,2:3])
    colnames(meansPerCluster) <- NULL
    
    # correction of orphan centroids 
    # (centroids with no datapoint):
    if (nrow(meansPerCluster) < k){
      #for each orphan centroid
      for (i in c(1:(k-nrow(meansPerCluster)))){
        # assign new randomized point
        rowToAppend <- c()
        # for each feature create random val
        # between min and max
        for (j in c(1:dimensions)){
          # random point between min and max value of column
          rowToAppend <- c(rowToAppend, runif(1, min=limits[j,1],
                                          max=limits[j, 2]))
        }
        meansPerCluster <- rbind(meansPerCluster, rowToAppend)
      }
    }
    
    #print(centroids)
    #print(meansPerCluster)
    
    # if new cluster-means equals centroids break loop
    if (isTRUE(all.equal(meansPerCluster, centroids))){
      break
    }
    # else cluster means become the centroids for the next interation
    else {
      centroids <- meansPerCluster
    }
  }
  returnCluster <- clusters
  
  returnCentroids <- as.data.frame(centroids)
  
  
  returnList <- list("cluster"=returnCluster, "centroids"=returnCentroids)
  
  return(returnList)

}

###### Plotting
ptm = proc.time()

# Training
myResult <- myKMeans(data, 5)
# Ausgabe Laufzeit
cat("Laufzeit mein K-Means:\n")
pt = proc.time() - ptm
print(pt)
rtSubtitle <- sprintf('Runtime: %s seconds', round(pt[3], 3))



a <- ggplot(as.data.frame(data))
a <- a + geom_point(aes(x = V1, y = V2, color = factor(myResult$cluster)), size=2)
a <- a + geom_point(aes(x = V1, y = V2, color = rownames(myResult$centroids)),
                    data=myResult$centroids, shape=0, size=4)
a <- a + labs(title = "My K-Means Implementation",
              subtitle = rtSubtitle,
              x = "Height in cm", y = "Weight in kg",
              color = "Cluster")
a