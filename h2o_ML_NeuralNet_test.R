# h2o_ML_test.R
# code example from
# https://www.quora.com/What-are-the-best-ways-to-pick-up-Deep-Learning-skills-as-an-engineer
# knb 20161108
# setwd("~/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag")
# classify a numeric representaion of the MNIST data set
# by constructing a neural network with 3 hidden layers
library(h2o)
library(RCurl)
h2o.init(nthreads = -1) # This means nthreads = num available cores
# 12.7 MB compressed, 100MB uncompressed, 60000 rows, 785 columns
traingz <- "train.csv.gz"
if(! file.exists(traingz)){
        train_file <- sprintf("https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/%s", traingz)
}
testgz <- "test.csv.gz"
if(! file.exists(testgz)){
        test_file <- sprintf("https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/%s", testgz)
}

train <- h2o.importFile(train_file)
test <- h2o.importFile(test_file)
# To see a brief summary of the data, run the following command
summary(train)
summary(test)
# Specify the response and predictor columns
y <- "C785"
x <- setdiff(names(train), y)
# We encode the response column as categorical for multinomial classification
train[,y] <- as.factor(train[,y])
# unique(as.data.frame(train[,y])) # 10 unique categories
test[,y] <- as.factor(test[,y])
# Train a Deep Learning model and validate on a test set
model <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train,
                          validation_frame = test,
                          distribution = "multinomial",
                          activation = "RectifierWithDropout",
                          hidden = c(20,20,20), # c(200,200,200),
                          input_dropout_ratio = 0.2,
                          l1 = 1e-5,
                          epochs = 10)

summary(model) # contains confusionMatrix etc

getClass(model)
