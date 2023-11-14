# read data into R
seeds <- read.csv('./seeds_dataset_class.csv', sep=",")
# import/install libraries
# install.packages("rpart")
# install.packages("class")
library(rpart)
library(class)
# split data into class and values variables
seeds_rand <- seeds[sample(150, 150),] # randomise data since data could be small or biassed so good to use resampling method
seedsclass <- seeds_rand[,1]
seedsvalues <- seeds_rand[,-1]

# set up training and test datasets (holdout method)
seedsclass_train <- seedsclass[1:100]
seedsvalues_train <- seedsvalues[1:100,]

seedsclass_test <- seedsclass[100:150]
seedsvalues_test <- seedsvalues[100:150,]


# create decision tree
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



# 2. Test it by scoring the accuracy on test data using different degrees of pruning using
# prune.

# testing classification by pruning the tree and comparing accuracy
arr = array(dim=c(1,5)) # initialise array of size 1x5 (to fit pruned accuracy values)
# want to test multiple pruned trees, so use for loop to get different values of cp (e.g. 0.1, 0.2, 0.3, etc.)
for (i in 1:5) {
  pfit<-prune(fit, cp=i/10)
  plot(pfit, uniform = TRUE, main = "Pruned Decision Tree for SeedsData3 (cp = i/10") # display pruned tree where cp = i/10
  text(pfit, use.n = TRUE, cex=.8)
  p_treepred<-predict(pfit, seedsvalues_test, type = 'class')
  p_n = length(seedsclass_test) # test case
  p_ncorrect = sum(p_treepred==seedsclass_test) # correctly predicted
  p_accuracy = p_ncorrect/p_n
  arr[1,i]=p_accuracy # add accuracy value to array
}
print(arr) # print accuracies of pruned trees


# 3. Scatterplot 2 selected variables of the data (and colour code according to the decision
# tree output), display the learnt tree and calculate the accuracy.
plot(seeds$Length, seeds$Width, col=pred)
# (accuracy and tree displayed in previous code block)


# 4. Compare the accuracy of the different pruned trees to KNN with different values of k
arr = array(dim=c(2,5)) # 2x5 array to store accuracy values for pruned tree and knn
for (i in 1:5) {
  pfit<-prune(fit, cp=i/10)
  plot(pfit, uniform = TRUE, main = "Pruned Decision Tree for SeedsData3")
  text(pfit, use.n = TRUE, cex=.8)
  p_treepred<-predict(pfit, seedsvalues_test, type = 'class')
  p_n = length(seedsclass_test) #num of test case
  p_ncorrect = sum(p_treepred==seedsclass_test) #num correctly predicted
  p_accuracy = p_ncorrect/p_n
  arr[1,i]=p_accuracy
  knn3pred = knn(seedsvalues_train, seedsvalues_test, seedsclass_train, k=i) # generate predicted class
  n_knn = length(seedsclass_test) # num test cases
  ncorrect_knn = sum(knn3pred==seedsclass_test) #num correctly predicted
  accuracy_knn = ncorrect_knn/n_knn
  arr[2,i]=accuracy_knn
}
print(arr) # (1 is pruned tree, 2 is knn)