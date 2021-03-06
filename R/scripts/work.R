# Libraries 
library(readr)

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

# Lets start with a standard glm 
model <- glm(TARGET_act_cycle ~ user_id + acne + backache + bloating + cramp + diarrhea + dizzy + headache + mood + nausea + sore + est_cycle + est_period + act_period, data=train)
predicted <- predict(model, test, type=c("response" ))

# actual results 
results <- as.data.frame(cbind(test$TARGET_act_cycle, predicted))
results$err = abs(results$V1-results$predicted)
mean(results$err)

# Naive Results 
results2 <- as.data.frame(cbind(test$TARGET_act_cycle))
results2$predicted = 28.75
results2$err = abs(results2$V1-results2$predicted)
mean(results2$err)

(mean(results2$err)- mean(results$err))/mean(results2$err)

