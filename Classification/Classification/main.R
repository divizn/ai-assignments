# read data into R
winedata <- read.csv('./winedata.csv', sep=",")

# split data into class and values variables
wineclass <- winedata[,1]
winevalues <- winedata[,-1]

# set up training and test datasets
wineclass_train <- wineclass[1:100]
winevalues_train <- winevalues[1:100,]

wineclass_test <- wineclass[100:178]
winevalues_test <- winevalues[100:178,]


# build decision tree using rpart
install.packages("rpart")
library(rpart)

fit <- rpart(wineclass_train~ ., data=winevalues_train, method="class")

# plot decision tree
plot(fit, uniform=TRUE, main="Decision tree for winedata")
text(fit, use.n=TRUE, all=TRUE, cex=.8)



# q1 why - sample of data could be small or biassed so good to use resampling method