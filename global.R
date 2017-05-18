
require(shiny)
require(ggplot2)
require(ROCR)
require(pROC)

# Jarren's Working Directory
setwd("~/Desktop/shinycats")

# Use datasets previously cleaned; for more information, see catsClean.R
cats <- read.csv("catsFinal.csv")
train <- read.csv("catsTrain.csv")
test <- read.csv("catsTest.csv")

# Function needed to plot ROC Curve/calculate AUC
simple_roc <- function(labels, scores){
    labels <- labels[order(scores, decreasing=TRUE)]
    data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}