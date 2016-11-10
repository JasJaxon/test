# This a practice tutorial for getting started in R. 
# I have combined two practices I found online in order 
# to get a better sense of different ways to load, look at, clean, summarize, test, 
# and visualize data, building models, evaluating algorithms and making predictions. 


# Check working directory 
# Install any packages you need to use i.e. "caret"
# Load the package

# Load raw data from CSV file

# Define the file name, don't forget the ""
filename <- "iris.csv"

# Load CSV file from local directory
iris_dataset <- read.csv(filename, header = TRUE)

# Set the header euqal to "TRUE" if you do not want to edit column names
# Otherwise, set column names
# The online example says to rename the columns however, I ran into trouble 
# when having to take out "Species" as a level b/c the original header got 
# bumped to row 1. After a googling different ways to remove that row,
# I was still left with 4 levels of species. So I decided to keep the
# header equal to TRUE instead of FALSE
# =========================================
# Below is what the tutorial tells you to do next but I skipped and continued
# to next steps:

#  colnames(iris_dataset) <- c("ID", "Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# ==========================================

# The dataset contains 150 observations of iris flowers. 
# There are four columns of measurements of the flowers in centimeters. 
# The fifth column is the species of the flower observed. 
# All observed flowers belong to one of three species.


# Create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(iris_dataset$Species, p=0.80, list=FALSE)
# Select 20% of the data for validation
validation <- iris_dataset[-validation_index,]
# Use the remaining 80% of data to training and testing the models
iris_dataset <- iris_dataset[validation_index,]

# Now we have training data in the "iris_dataset" variable
# And a validation (test) set we will use in the validation variable

# Summarizing the dataset in 6 ways

# 1 Dimensions of the dataset (looking at # of rows and columns)
dim(iris_dataset)

# 2 Types of attributes (listing the types of attributes, what are we working with?)
sapply(iris_dataset, class)
lapply(iris_dataset, class)

# 3 Peek at the data (take a peek at the first 5 rows of data)
head(iris_dataset)

# 4 Levels of the class  (listing levels for the class)
levels(iris_dataset$Species)

# Now you can see why this is called a multi-class classifcation problem

# 5 Class distribution (summarize the class distribution). 
# View it as an absolute count or a percentage
percentage <- prop.table(table(iris_dataset$Species)) * 100
cbind(freq=table(iris_dataset$Species), percentage=percentage)

# 6 Statiscal summary (mean, max, min, percentiles)
summary(iris_dataset)

# Ok, now we will visualize the data set
# Let's start with univarible attributes to understand each attribute

# Univarite plots
# split input (x) and output (y)
x <- iris_dataset[,1:4]
y <- iris_dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# barplot for class breakdown
plot(y)

# Multivariate plots
# Scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")


# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Evaluate Algorithms
# 1 Test Harness

# We will 10-fold crossvalidation to estimate accuracy.
# This will split our dataset into 10 parts, train in 9 and test on 1 
# and release for all combinations of train-test splits. We will also 
# repeat the process 3 times for each algorithm with different splits of 
# the data into 10 groups, in an effort to get a more accurate estimate.

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# 2 Build models
# Let’s evaluate 5 different algorithms:
  
# Linear Discriminant Analysis (LDA)
# Classification and Regression Trees (CART).
# k-Nearest Neighbors (kNN).
# Support Vector Machines (SVM) with a linear kernel.
# Random Forest (RF)

# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=iris_dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=iris_dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=iris_dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=iris_dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest

# Select the best model. 
# We need to compare the models and pick the most accurate one

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# We can also create a plot of the model evaluation results and 
# compare the spread and the mean accuracy of each model. There 
# is a population of accuracy measures for each algorithm because 
# each algorithm was evaluated 10 times (10 fold cross validation).

# compare accuracy of models
dotplot(results)

# rf and lda are not plotted howver, from the results we can see 
# that they are most accurate.

# summarize Best Model
print(fit.lda)
print(fit.rf)

# This gives a nice summary of what was used to train the model and 
# the mean and standard deviation (SD) accuracy achieved, 
# specifically 97.5% accuracy +/- 4%

## THIS IS TRUE FOR THE ONLINE TUTORIAL HOWEVER, MY NUMBERS CAME OUT A LITTLE DIFFERENT

# Make preditions


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)


  
