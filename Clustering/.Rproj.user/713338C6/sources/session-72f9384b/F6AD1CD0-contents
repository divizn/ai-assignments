mydata = read.csv('./spaeth_01.csv', sep = ',')

plot(mydata)

# optional: Prepare Data
mydata = na.omit(mydata) # deletion of missing data
mydata = scale(mydata) # standardize variables

d <- dist(mydata, method = "euclidean") # distance matrix
