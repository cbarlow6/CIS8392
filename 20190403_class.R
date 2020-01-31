install.packages("keras")
library(tidyverse)
library(keras)
install_keras()


mnist <- dataset_mnist() # get the data from the internet; ~200MB
train_images <- mnist$train$x 
train_labels <- mnist$train$y 
test_images <- mnist$test$x 
test_labels <- mnist$test$y
str (mnist)