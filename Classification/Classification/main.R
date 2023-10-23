# read data into R
seeds <- read.csv('./seeds_dataset_class.csv', sep=",")
# split data into class and values variables
seeds_rand <- seeds[sample(150, 150),]
seedsclass <- seeds_rand[,1]
seedsvalues <- seeds_rand[,-1]

# set up training and test datasets (holdout method)
seedsclass_train <- seedsclass[1:100]
seedsvalues_train <- seedsvalues[1:100,]

seedsclass_test <- seedsclass[100:150]
seedsvalues_test <- seedsvalues[100:150,]


# build decision tree using rpart
# install.packages("rpart")
library(rpart)

# create decision tree and pruned decision trees (to compare)
fit <- rpart(seedsclass_train~ ., data=seedsvalues_train, method="class")
# plot decision tree
plot(fit, uniform=TRUE, main="Decision tree for seeds data")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# sensitivity analysis - test classification accuracy on test dataset by comparing predicted and actual values
pred <- predict(fit, seedsvalues_test, type="class")

n <- length(seedsclass_test) # num of test cases
ncorrect <- sum(pred == seedsclass_test) # num of correct predictions
accuracy <- ncorrect/n # accuracy of predictions
print(accuracy)

# show results in a confusion matrix table
table_mat <- table(seedsclass_test, pred)
print(table_mat)

# pruning data
pfit <- prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned decision tree for seeds data")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


# q1 why - sample of data could be small or biassed so good to use resampling method