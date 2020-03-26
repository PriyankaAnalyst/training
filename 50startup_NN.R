#PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS


library(readr)
library(neuralnet)#Regression
library(nnet)#Classification

startup <- read.csv(file.choose())

View(startup)
class(startup)
summary(startup)
attach(startup)
str(startup)

normalize <- function(x) 
{
  
  return((x-min(x))/(max(x)-min(x))) #converting the values into 0 and 1
  
}

startup$State <- as.numeric(startup$State)

str(startup$State)

startup_norm <- as.data.frame(lapply(startup, FUN = normalize))

summary(startup_norm)  #converted the values into 0 and 1

startup_train <- startup_norm[1:24,]
startup_test <- startup_norm[25:50,]

startup_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State, 
                            data = startup_train)

plot(startup_model)

model_results <- compute(startup_model, startup_test[1:4])                              
str(model_results)

predicted_profit <- model_results$net.result

cor(predicted_profit, startup_test$Profit) #53.76%, Use function to remove 'NA' values
plot(predicted_profit, startup_test$Profit) #scatter plot

model5 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State, 
                    data = startup_train, hidden = 5)
plot(model5)  #five nodes with more complexity level

model5$weights
model5_res<- compute(model5, startup_test[1:4])
pred_strn_5 <- model5_res$net.result

cor(pred_strn_5, startup_test$Profit) #93.05%
plot(pred_strn_5, startup_test$Profit, col = "blue")

#Now the Accuracy is 92.87% so, its the best model we can have. 
