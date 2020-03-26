#Perform Principal component analysis and perform clustering using first 
#3 principal component scores (both heirarchial and k mean clustering(scree plot or elbow curve) and obtain 
#optimum number of clusters and check whether we have obtained same number of clusters with the original data 
#(class column we have ignored at the begining who shows it has 3 clusters)df


install.packages("cluster")
install.packages("fpc")
install.packages("NbClust")
install.packages("factoextra")
install.packages("eclust")
install.packages("clusplot")

library(ggplot2)
library(cluster)
library(fpc)
library(NbClust)
library(plyr)
library(factoextra)
library(eclust)
library(clusplot)

wine <- read.csv(choose.files())

View(wine)

attach(wine)

cor(wine)

#Dilution and Flavaniods having strong correlation. 
#Hue and Ash is having negative strong correlation

#creating the PCA object.

pcaobj <- princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaobj) #comp8 to comp.14 having above 90%
str(pcaobj)

pcaobj$loadings
#sum of weights of all the columns is always equal to 1


plot(pcaobj$scores[,2], pcaobj$scores[,4]) #No relation

cor(pcaobj$scores[,2], pcaobj$scores[,4]) #the correlation between any two columns is always zero (0)

cor(pcaobj$scores)

plot(pcaobj) #graph showing principal components
# Comp.1 is having the highest importance (Highest Variance)

pcaobj$scores[,1:3] #Top 3 PCA scores which represents the whole data

mydata <- cbind(wine, pcaobj$scores[,1:4])

#preparing data for clustering (considering only PCA scores as they represent the whole data)

clust_data <- mydata[,15:18]

#standarzation the data

norm_clust <- scale(clust_data) #scale function used to normalize the data

dist1 <- dist(norm_clust, method = "euclidean") #Euclidean Distance Measuring Method

#clustering the data using hclust function (Hierarical)

fit_clust <- hclust(dist1, method = "complete") #method is complete linkage

plot(fit_clust, hang = -1) #displaying dendogram

rect.hclust(fit_clust, k=3, border = "red") 

#K-Means clustering

Kmeans <- kmeans(norm_clust, 3, nstart = 25) #extract the results using these 3 clusters.
Kmeans

Kmeans$centers #centriods

#Visualizing K-means Clusters
clusplot(norm_clust, Kmeans$cluster, main = '2D representation', color = TRUE, shade = TRUE, lables = 2, lines = 0)

#optimal number of clusters that are 3 in numbers
fviz_cluster(object = Kmeans, data = norm_clust, ellipse.type = "norm", geom = "point", palette = "jco", main = "", ggtheme = theme_minimal())

groups <- cutree(fit_clust, 5)

membership1 <- as.matrix("groups") #cluster numbering

final_data <- cbind(membership1, mydata) #binding column wise data with original

View(aggregate(final_data[,-c(2,16:19)], by=list(membership1), FUN=mean))
     
