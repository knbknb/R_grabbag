# h2o_ML_dzone-blogpost.R
# code example from
# https://dzone.com/articles/a-trial-run-with-h2o-automl-automatic-machine-lear
#
# A Trial Run With H2O AutoML in R: Automated Machine Learning Functionality
# No more coding for different models, noting down the results, and selecting the best model — AutoML is going to do all of these for you while you brew a cuppa!
#        by Sibanjan Das
#
# Oct. 10, 17 · AI Zone
# dataset from https://github.com/SaadEddin/DataScienceProjects/blob/master/conversion_rate/conversion_data.csv
# knb 20171013
# setwd("~/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag")
# classify a numeric representaion of the MNIST data set
# by constructing a neural network with 3 hidden layers
library(h2o)
library(RCurl)
h2o.init(nthreads = -1) # This means nthreads = num available cores
# 12.7 MB compressed, 100MB uncompressed, 60000 rows, 785 columns
conv_data <- read.csv("./conversion_data.csv")

str(conv_data)

conv_data$age <- as.factor(conv_data$age)

conv_data$total_pages_visited <- as.factor(conv_data$total_pages_visited)

conv_data$converted <- as.factor(conv_data$converted)

# Country is a nominal attribute. When using the H2O algorithm, we can just convert it to factor so that it can be one hot encoded

#automatically

conv_data$country  <- as.factor(conv_data$country)

conv_data$ID  <- 1:nrow(conv_data)

conv_data.hex  <- as.h2o(conv_data)

#Split data into Train/Validation/Test Sets

split_h2o <- h2o.splitFrame(conv_data.hex, c(0.6, 0.2), seed = 1234 )

train_conv_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 60%

valid_conv_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 20%

test_conv_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 20%

#Model

# Set names for h2o

target <- "converted"

predictors <- setdiff(names(train_conv_h2o), target)

# AutoML
#
# After all these basic preprocessings, now is the time to create models.
# We run h2o.automl() by setting few mandatory parameters as below.
#
# x: The names of the predictors
#
# y:  The target column name
#
# training_frame: The training data set that is to be used for creating the model
#
# leaderboard_frame: The validation data set used by h2o to ensure
# the model doesn't overfit the data

# Run the automated machine learning

automl_h2o_models <- h2o.automl(

        x = predictors,

        y = target,

        training_frame    = train_conv_h2o,

        leaderboard_frame = valid_conv_h2o

)

# Extract leader model

automl_leader <- automl_h2o_models@leader


# Predict on hold-out test set

pred_conversion <- h2o.predict(object = automl_leader, newdata = test_conv_h2o)

#Confusion matrix on test data set

h2o.table(pred_conversion$predict, test_conv_h2o$converted)

#compute performance

perf <- h2o.performance(automl_leader,conv_data.hex)

h2o.confusionMatrix(perf)

h2o.accuracy(perf)

h2o.tpr(perf)
