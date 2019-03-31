# Libraries 
library(readr)
library(randomForest)

# Script to predict end dates 
set.seed(1234)

data <- read_csv("~/GitHub/PhillyAI_Hackathon_SingleSyllable/data_out/model_sample_02.csv")
#data$user_id <- as.factor(data$user_id)
# Naive model 
ave_cycle = mean(data$TARGET_act_cycle)



# create test and training sets 
inTrain <- sample(1:nrow(data), nrow(data) * 0.85) # select 85% of the items 
train <- data[inTrain, ]
test <- data[-inTrain, ]
rm(data, inTrain)


cols <- c('acne', 'backache', 'bloating', 'cramp', 'diarrhea', 
          'dizzy', 'est_cycle', 'est_period', 'act_period')
rf <- randomForest(train$TARGET_act_cycle ~ ., data=train[,cols], ntree=500)
predicted <- predict(rf, test[,cols])

# actual results 
results <- as.data.frame(cbind(test$TARGET_act_cycle, predicted))
results$err = abs(results$V1-results$predicted)
print(mean(results$err))

# Naive Results 
results2 <- as.data.frame(cbind(test$TARGET_act_cycle))
results2$predicted = 28.75
results2$err = abs(results2$V1-results2$predicted)
print(mean(results2$err))

print((mean(results2$err)- mean(results$err))/mean(results2$err))

