# mydata = read.csv('./spaeth_01.csv', sep = ',')
# plot(mydata)

seed = read.csv('seeds_dataset.csv', sep = ',')
plot(seed)
realseed = read.csv('seeds_real.csv', sep = ',')
plot(realseed)


# optional: Prepare Data
seed = na.omit(seed) # deletion of missing data
seed = scale(seed) # standardize variables


# HC

d <- dist(seed, method = "euclidean") # generate distance matrix between datapoints

fit <- hclust(d, method="average") # perform clustering (s, a, c)
plot(fit) # display dendogram

Hgroups <- cutree(fit, k=3) # cut tree into 3 clusters

rect.hclust(fit, k=5, border="red") # draw the dendrogram with red borders around these 3 clusters
plot(seed, col=Hgroups) # draw a scatterplot with the assigned clusters as colours


# KC

fit <- kmeans(seed, 5) # 3 clusters solution (k=3)

aggregate(seed,by=list(fit$cluster),FUN=mean) # calculate mean value for each variable 

Kgroups = fit$cluster # split cluster assignments

plot(seed, col=Kgroups)



# calculate weighted kappa

source("./WK_R.r")

wk = WK_R(Kgroups, Hgroups)

fitreal_k <- kmeans(realseed, 3) # 3 clusters
realGroup = fitreal_k$cluster

wkKCReal = WK_R(Kgroups, realGroup)
