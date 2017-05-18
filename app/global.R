
require(shiny)
require(ggplot2)
require(pROC)

# Jarren's Working Directory
setwd("~/Desktop/shinycats/app")

# Use datasets previously cleaned; for more information, see catsClean.R
train <- read.csv("catsTrain.csv")
test <- read.csv("catsTest.csv")

# Function needed to plot ROC Curve/calculate AUC
simple_roc <- function(labels, scores){
    labels <- labels[order(scores, decreasing=TRUE)]
    data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}