dTrain <- read.csv("D:\\MyNewRProject\\train.csv")
dTest <- read.csv("D:\\MyNewRProject\\test.csv")

summary(dTrain)

head(dTrain)    # First 6 records of the data
tail(dTrain)    # Last 6 records of the data

# Checking structuring of the data
str(dTrain)

## Data Cleaning
# Converting required columns into factors & renaming some of them
dTrain[dTrain$sex==0, ]$sex <- "F"
dTrain[dTrain$sex==1, ]$sex <- "M"
dTrain$sex <- as.factor(dTrain$sex)

dTrain$cp <- as.factor(dTrain$cp)
dTrain$fbs <- as.factor(dTrain$fbs)
dTrain$restecg <- as.factor(dTrain$restecg)
dTrain$exang <- as.factor(dTrain$exang)
dTrain$slope <- as.factor(dTrain$slope)
dTrain$ca <- as.factor(dTrain$ca)
dTrain$thal <- as.factor(dTrain$thal)

dTrain$target <- ifelse(test = dTrain$target==0, yes = "Healthy", no = "Unhealthy")
dTrain$target <- as.factor(dTrain$target)

# Verifying the structure of the data
str(dTrain)

# Checking whether the data consists of NA values
nrow(dTrain[is.na(dTrain$ca) | is.na(dTrain$thal),])

## Checking whether the data is balanced or not !!!

xtabs(~target + sex, data = dTrain)
xtabs(~target + cp, dTrain)
xtabs(~target + fbs, dTrain)
xtabs(~target + restecg, dTrain)  #
xtabs(~target + exang, dTrain)
xtabs(~target + slope, dTrain)
xtabs(~target + ca, dTrain)
xtabs(~target + thal, dTrain)

## Constructing a Logistic regression model

model <- glm(target~. , data = dTrain, family = "binomial")
# Summary of that model
summary(model)

library(ggplot2)

# install.packages("cowplot")
library(cowplot)

predicted.data <- data.frame(
                      probability.of.target = model$fitted.values,
                      target = dTrain$target
                  )
predicted.data <- predicted.data[order(predicted.data$probability.of.target, decreasing = FALSE), ]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data = predicted.data, aes(x=rank, y=probability.of.target)) +
  geom_point(aes(color=target), alpha = 1, shape = 4, stroke = 1) + xlab("Index") + 
  ylab("Predicted probability of getting heart disease")

ggsave("ForTestData.pdf")

# For Test data
dTest[dTest$sex==0, ]$sex <- "F"
dTest[dTest$sex==1, ]$sex <- "M"
dTest$sex <- as.factor(dTest$sex)

dTest$cp <- as.factor(dTest$cp)
dTest$fbs <- as.factor(dTest$fbs)
dTest$restecg <- as.factor(dTest$restecg)
dTest$exang <- as.factor(dTest$exang)
dTest$slope <- as.factor(dTest$slope)
dTest$ca <- as.factor(dTest$ca)
dTest$thal <- as.factor(dTest$thal)

dTest$target <- ifelse(test = dTest$target==0, yes = "Healthy", no = "Unhealthy")
dTest$target <- as.factor(dTest$target)


dTest$op <- predict(model, dTest, type = "response")

predicted.data2 <- data.frame(
                      probability.of.target = dTest$op,
                      target = dTest$target
                  )
predicted.data2 <- predicted.data2[order(predicted.data2$probability.of.target, decreasing = FALSE), ]
predicted.data2$rank <- 1:nrow(predicted.data2)

ggplot(data = predicted.data2, aes(x=rank, y=probability.of.target)) +
  geom_point(aes(color=target), alpha = 1, shape = 4, stroke = 1) + xlab("Index") + 
  ylab("Predicted probability of getting heart disease")

ggsave("ForTestData2.pdf")