
# flower exercises, first load the data and convert it to a matrix then a vector
# so we can imput the data into our clustering algorithm.
flower = read.csv("flower.csv", header=FALSE)
flowerMatrix = as.matrix(flower)
flowerVector = as.vector(flowerMatrix)
str(flowerVector)
distance = dist(flowerVector, method = "euclidean")
#hierarchical clustering
clusterIntensity = hclust(distance, method = "ward.D")
# plot cluster Dendrogram
plot(clusterIntensity)
# create boarder around 3 clusters
rect.hclust(clusterIntensity, k=3, border="red")
# creates a vector of our clusters
flowerClusters = cutree(clusterIntensity, k=3)
# gather information about our clusters shows cluster 3 is the least intense(whitest) 
# while cluster 1 is the most intense or darkest.
tapply(flowerVector, flowerClusters, mean)
# to see the image first we convert it to a matrix to use the image function. 50x50 is the size of the image.
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes=FALSE)
# to see the original image, #256 corresponds to the grey colorscale
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))


# MRI brain scan images, load data and convert a matrix
healthy = read.csv("healthy.csv", header=F)
healthyMatrix = as.matrix(healthy)
# showing the output we can see the image size is 566x646
str(healthyMatrix)
#num [1:566, 1:646] 0.00427 0.00855 0.01282 0.01282 0.01282 ...
#- attr(*, "dimnames")=List of 2
#..$ : NULL
#..$ : chr [1:646] "V1" "V2" "V3" "V4" ...
# use the image funcation to see what the image looks like
image(healthyMatrix, axes=F, col = grey(seq(0,1,length=256)))
# convert the matrix to a vector for clustering
healthyVector = as.vector(healthyMatrix)
# hierarchical clustering will not work because our vector is huge! 
# we will use k-means clustering instead, we set k=5 for the number
# of clusters and set max iterations to 1000
k=5
set.seed(1)
KMC = kmeans(healthyVector, centers=k, iter.max=1000)
str(KMC)
#List of 9
#$ cluster     : int [1:365636] 3 3 3 3 3 3 3 3 3 3 ...
#$ centers     : num [1:5, 1] 0.4818 0.1062 0.0196 0.3094 0.1842
#..- attr(*, "dimnames")=List of 2
#.. ..$ : chr [1:5] "1" "2" "3" "4" ...
#.. ..$ : NULL
#$ totss       : num 5775
#$ withinss    : num [1:5] 96.6 47.2 39.2 57.5 62.3
#$ tot.withinss: num 303
#$ betweenss   : num 5472
#$ size        : int [1:5] 20556 101085 133162 31555 79278
#$ iter        : int 2
#$ ifault      : int 0
#- attr(*, "class")= chr "kmeans"

# extracting the information for each cluster
healthyClusters = KMC$cluster

# use that information in the healthyClusters variable to create a matrix
# and display our image
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes=F, col = rainbow(k))

#load our tumor data to compare to our health tumor data set

tumor = read.csv("tumor.csv", header=F)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)
install.packages("flexclust")
library(flexclust)
KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata=tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))

