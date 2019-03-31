# Libraries 
library(readr)
library(gbm)
library(Metrics)

# Script to predict end dates 
set.seed(1234)

data <- read_csv("~/GitHub/PhillyAI_Hackathon_SingleSyllable/data_out/model_sample_02.csv")
# data$user_id <- as.factor(data$user_id)
# Naive model 
ave_cycle = mean(data$TARGET_act_cycle)



# create test and training sets 
inTrain <- sample(1:nrow(data), nrow(data) * 0.85) # select 85% of the items 
train <- data[inTrain, ]
test <- data[-inTrain, ]
rm(data, inTrain)


gbmModel = gbm(formula = TARGET_act_cycle ~ user_id + acne + backache + 
                 bloating + cramp + diarrhea + dizzy + headache + mood + 
                 nausea + sore + est_cycle + est_period + act_period,
               distribution = "gaussian",
               data = train,
               n.trees = 2500,
               shrinkage = .01,
               n.minobsinnode = 20)

best.iter <- gbm.perf(gbmModel, method = "OOB")


predicted <- predict(gbmModel, 
                     newdata = test, 
                     n.trees = best.iter,
                     type = "response")


# actual results 
results <- as.data.frame(cbind(test$TARGET_act_cycle, predicted))
results$err = (results$V1-results$predicted)^2
mse_rf = mean(results$err)
print(sqrt(mse_rf))

# Naive Results 
results2 <- as.data.frame(cbind(test$TARGET_act_cycle))
results2$predicted = 28.75
results2$err = (results2$V1-results2$predicted)^2
mse_ne = mean(results2$err)
print(sqrt(mse_ne))

# Improvement 
net_improvement = (mse_ne - mse_rf)/mse_ne
print(net_improvement)
