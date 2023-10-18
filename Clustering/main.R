mydata = read.csv('./spaeth_01.csv', sep = ',')
plot(mydata)

seed = read.csv('seeds_dataset.csv')
plot(seed)
realseed = read.csv('seeds_real.csv')
plot(realseed)


# optional: Prepare Data
mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables


# HC

d <- dist(mydata, method = "euclidean") # generate distance matrix between datapoints

fit <- hclust(d, method="average") # perform clustering (s, a, c)
plot(fit) # display dendogram

Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red") # draw the dendrogram with red borders around these 5 clusters
plot(mydata, col=Hgroups) # draw a scatterplot with the assigned clusters as colours


# KC

fit <- kmeans(mydata, 5) # 5 clusters solution (k=5)

aggregate(mydata,by=list(fit$cluster),FUN=mean) # calculate mean value for each variable 

Kgroups = fit$cluster # split cluster assignments

plot(mydata, col=Kgroups)



# calculate weighted kappa

source("./WK_R.r")

wk = WK_R(Kgroups, Hgroups)
