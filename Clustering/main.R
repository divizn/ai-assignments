source("./WK_R.r")
seed = read.csv('seeds_dataset.csv', sep = ',')
plot(seed)
realseed = read.csv('seeds_real.csv', sep = ',')
plot(realseed)


# optional: Prepare Data
seed = na.omit(seed) # deletion of missing data
seed = scale(seed) # standardize variables


# Hierarchical Clustering

d <- dist(seed, method = "euclidean") # generate distance matrix between datapoints

fit <- hclust(d, method="average") # perform clustering (s, a, c)
plot(fit) # display dendogram

Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red") # draw the dendrogram with red borders around these 5 clusters
plot(seed, col=Hgroups) # draw a scatterplot with the assigned clusters as colours


# K Means Clustering

fit <- kmeans(seed, 3) # 3 clusters solution (k=3)

aggregate(seed,by=list(fit$cluster),FUN=mean) # calculate mean value for each variable 

Kgroups = fit$cluster # split cluster assignments

plot(seed, col=Kgroups)



# calculate weighted kappa for k means compared to hierarchical (and also correct clusters)


wk = WK_R(Kgroups, Hgroups) # weighted kappa of KC vs HC

fitreal_k <- kmeans(realseed, 3) 
realGroup = fitreal_k$cluster


# get kappa of other linkage methods and also random values of k for k means


wk1 = WK_R(Kgroups, realGroup) # K Means compared to actual cluster (weighted kappa value)

wk2 = WK_R(Hgroups, realGroup) # Hierarchical compared to actual cluster (weighted kappa value)

