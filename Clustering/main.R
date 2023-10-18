# import WK function
source("./WK_R.r")

# import datasets
seed = read.csv('seeds_dataset.csv', sep = ',') 
realseed = read.csv('seeds_real.csv', sep = ',')


# optional: Prepare Data
seed = na.omit(seed) # deletion of missing data
seed = scale(seed) # standardize variables


#---------------------------------------#


# Hierarchical Clustering - Single

d <- dist(seed, method = "euclidean") # generate distance matrix between datapoints

fit <- hclust(d, method="single") # perform clustering (s, a, c)
plot(fit, main="Single Cluster Dendogram") # display dendogram

Single_Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red") # draw the dendrogram with red borders around these 5 clusters
plot(seed, col=Single_Hgroups, main="Single Linkage Method") # draw a scatterplot with the assigned clusters as colours


# Hierarchical Clustering - Average

d <- dist(seed, method = "euclidean") # generate distance matrix between datapoints

fit <- hclust(d, method="average") # perform clustering (s, a, c)
plot(fit, main="Average Cluster Dendogram") # display dendogram

Average_Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="green") # draw the dendrogram with red borders around these 5 clusters
plot(seed, col=Average_Hgroups, main="Average Linkage Method") # draw a scatterplot with the assigned clusters as colours


# Hierarchical Clustering - complete

d <- dist(seed, method = "euclidean") # generate distance matrix between datapoints

fit <- hclust(d, method="complete") # perform clustering (s, a, c)
plot(fit, main="Complete Cluster Dendogram") # display dendogram

Complete_Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="blue") # draw the dendrogram with red borders around these 5 clusters
plot(seed, col=Complete_Hgroups, main="Complete Linkage Method") # draw a scatterplot with the assigned clusters as colours


#---------------------------------------#


# K Means Clustering

k=3

fit <- kmeans(seed, k) # k clusters solution

# aggregate(seed,by=list(fit$cluster),FUN=mean) # calculate mean value for each variable 

Kgroups_3 = fit$cluster # split cluster assignments

plot(seed, col=Kgroups_3, main="K-Means scatterplot for 3 clusters")

#--

k=4

fit <- kmeans(seed, k) # k clusters solution

# aggregate(seed,by=list(fit$cluster),FUN=mean) # calculate mean value for each variable 

Kgroups_4 = fit$cluster # split cluster assignments

plot(seed, col=Kgroups_4, main="K-Means scatterplot for 4 clusters")

#--

k=5

fit <- kmeans(seed, k) # k clusters solution

# aggregate(seed,by=list(fit$cluster),FUN=mean) # calculate mean value for each variable 

Kgroups_5 = fit$cluster # split cluster assignments

plot(seed, col=Kgroups_5, main="K-Means scatterplot for 5 clusters")

#---------------------------------------#

# get clusters from solution dataset to compare with KMeans and HC using weighted kappa
fitreal_k <- kmeans(realseed, 3) 
real_group = fitreal_k$cluster


# get kappa of other linkage methods and also random values of k for k means

WK_KC3 = WK_R(Kgroups_3, real_group) # K Means where k=3 compared to actual cluster (weighted kappa value)
WK_KC4 = WK_R(Kgroups_4, real_group) # K Means where k=4 compared to actual cluster (weighted kappa value)
WK_KC5 = WK_R(Kgroups_5, real_group) # K Means where k=5 compared to actual cluster (weighted kappa value)

WK_singleHC = WK_R(Single_Hgroups, real_group) # Hierarchical single linkage compared to actual cluster (weighted kappa value)
WK_averageHC = WK_R(Average_Hgroups, real_group) # Hierarchical average linkage compared to actual cluster (weighted kappa value)
WK_completeHC = WK_R(Complete_Hgroups, real_group) # Hierarchical complete linkage compared to actual cluster (weighted kappa value)



graph_x <- c("k=3","k=4","k=5","single","average","complete")
graph_y <- c(WK_KC3, WK_KC4, WK_KC5, WK_singleHC, WK_averageHC, WK_completeHC)

# barchart with added parameters
barplot(graph_y,
        main = "Weighted Kappa of clusters compared to actual clusters",
        xlab = "Method/Algorithm",
        ylab = "Weighted Kappa",
        names.arg = graph_x,
        col = "grey",
        ylim=c(0,1)
        )
