library(caret)
library(doSNOW)
library(dplyr)

load("/Users/ken/Documents/instacart/modelingdata.Rdata")
setwd("~/data/instacart")

#create a subset of the date for cross-validation
ID <- seq.int(nrow(train_train))

set.seed(1)
trainingRows <- createDataPartition(ID, p = .02, list= FALSE) 

sub_train_train <- train_train[trainingRows,]

fullset <- c(2, 7, 8, 9, 10)

sub_update<- sub_train_train[,fullset]

#create a dataframe for the dependent variable
ytrain <- as.data.frame(sub_train_train$reordered)
names(ytrain)[1] <- c("reordered")
ytrain <- ytrain %>%
  mutate(reordered1 = ifelse(reordered == 1, 'Yes', 'No'))
ytrain$reordered <- NULL
names(ytrain)[1] <- c("reordered")

#create dummy variable
simpleMod <- dummyVars("~. ", data = sub_update)
dummies.update <- predict(simpleMod, sub_update)
train.update1 <- dummies.update

train.update1 <- as.data.frame(train.update1)

train.update1 <- train.update1 %>%
  mutate(reordered = ytrain$reordered)

subset <- c(1:144)


#save data
#save(sub_update, train.update1, trainingRows, ytrain, file = "data_caret.Rdata")

#begin selecting model
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = trainingRows),
                     savePredictions = TRUE)


cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

glmFull <- train(x = train.update1[,subset],
                y = ytrain$reordered,
                method = "glm",
                metric = "ROC",
                trControl = ctrl)


confusionMatrix(data = glmFull$pred$pred,
                reference = glmFull$pred$obs)
dt_test1 <- dt_test[,-11]


pred <- predict(glmFull, newdata =dt_test, type = 'raw')
