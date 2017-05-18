### Setup

# Libraries
require(dplyr)
require(ggplot2)
require(ROCR)
require(pROC)

##  Working Directories -- Comment out working directories that aren't yours.

# Jarren's Working Directory
setwd("~/Desktop/shinycats")

# Uses a dataset previously cleaned; for more information, see consolidatedcode.R
cats <- read.csv("allcatsin.csv")


## Necessary function for ROC Curves
simple_roc <- function(labels, scores){
    labels <- labels[order(scores, decreasing=TRUE)]
    data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}


### Miscellaneous Data Cleaning

## Data Format

# Create intake column in numeric format
# cats$Intake.Num <- as.numeric(cats$Intake.Type)

# Get age in number of days
cats$DOB <- as.POSIXlt(as.Date(cats$DOB))
cats$Intake.Date <- as.POSIXlt(as.Date(cats$Intake.Date))
cats$date.y <- as.POSIXlt(as.Date(cats$date.y))
cats$AgeDaysInt <- as.numeric(difftime(cats$Intake.Date, cats$DOB, units = "days"))

# For any cats who are below the age of 0, take them out of the dataset
cats <- cats[cats$AgeDaysInt >= 0, ]

# Change Sex upon Intake variable to character
cats$Sex.upon.Intake <- as.character(cats$Sex.upon.Intake)

# For any cats who have NA as sex, classify as "unknown"
cats$Sex.upon.Intake <- ifelse(is.na(cats$Sex.upon.Intake), "Unknown", cats$Sex.upon.Intake)
cats$Sex.upon.Intake <- as.factor(cats$Sex.upon.Intake)

# For any cats who have NA as adopted, remove from data
cats$adopted <- as.factor(cats$adopted)
cats <- cats[!is.na(cats$adopted), ]

# Produce fully cleaned dataset
write.csv(cats, file = "catsFinal.csv")


## Creating training and test data

n <- nrow(cats)
set.seed(2017)

# Randomly decide on the groups (~75%)
x <- (sample(seq(1:n),ceiling(.75*n)))

# Divide into the two groups
catsTrain <- cats[x,]
catsTest <- cats[-x,]

# Produce the training and testing datasets
write.csv(catsTrain, file = "catsTrain.csv")
write.csv(catsTest, file = "catsTest.csv")



### Analysis via Logistic Regression

## Training Model
model1 <- glm(adopted ~ Intake.Type + Sex.upon.Intake + kitten + juvenile + young_adult + adult + multicolor + black + orange + AgeDaysInt, family = binomial(link = 'logit'), data = catsTrain)

summary(model1)


## Obtaining AUC for catsTrain

# Adding relevant components of logistic regression to training dataset
catsTrain$link1 <- predict(model1, catsTrain, type = "link")
catsTrain$prob1 <- predict(model1, catsTrain, type = "response")

# Using the `pROC` package
roc(response = catsTrain$adopted, predictor = catsTrain$prob1, direction = "<")

# Using the `ROCR` package
pr <- prediction(catsTrain$prob1, catsTrain$adopted)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



## Obtaining AUC for catsTest

# Adding relevant components of logistic regression to testing dataset
catsTest$link1 <- predict(model1, catsTest, type = "link")
catsTest$prob1 <- predict(model1, catsTest, type = "response")

catsTest <- catsTest[catsTest$prob1 > 0.1, ]

# Using the `pROC` package
roc(response = catsTest$adopted, predictor = catsTest$prob1, direction = "auto")

# Using the `ROCR` package
pr <- prediction(catsTest$prob1, catsTest$adopted)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



### Data Visualizations

## Training Data Visualizations

# Probability Plot
catsTrain %>%
    ggplot(aes(x = link1, y = prob1, col = as.factor(adopted))) +
    scale_color_manual(values = c("red", "blue")) + 
    geom_point() +
    geom_rug() +
    ggtitle("Test")

# ROC Curve
roc1 <- plot(roc(catsTrain$adopted, catsTrain$prob1, direction = "<"),
     col = "black", lwd = 3, main = "Test")

roc2 <- simple_roc(catsTrain$adopted == "1", catsTrain$link1)
with(roc2, points(1 - FPR, TPR, col = 1 + labels, pch = 2))
with(roc2, legend('topright', levels(catsTrain$adopted), pch = 2, col = c("red", "black"), bty='n', cex=.75))


## Testing Data Visualizations

# Probability Plot
catsTest %>%
    ggplot(aes(x = link1, y = prob1, col = as.factor(adopted))) +
    scale_color_manual(values = c("red", "blue")) + 
    geom_point() +
    geom_rug() +
    ggtitle("Test")

# ROC Curve
roc1 <- plot(roc(catsTest$adopted, catsTest$prob1, direction = "<"),
             col = "black", lwd = 3, main = "Test")

roc2 <- simple_roc(catsTest$adopted == "1", catsTest$link1)
with(roc2, points(1 - FPR, TPR, col = 1 + labels, pch = 2))
with(roc2, legend('topright', levels(cats$adopted), pch = 2, col = c("red", "black"), bty='n', cex=.75))



### Miscellaneous Code

#### Exploratory Data Analysis
#with(cats, plot(as.numeric(adopted)~as.numeric(Intake.Type), xlab = "Intake Type", ylab = "Outcome"))
#curve(predict(model1, data.frame(Intake.Type = x), type = "resp"), add = TRUE)
#points(cats$Intake.Type, fitted(model1), pch = 20)