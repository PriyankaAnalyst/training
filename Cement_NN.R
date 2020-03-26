#Prepare a model for strength of concrete data using Neural Networks

install.packages("neuralnet")  #Regression
install.packages("nnet") #Classification

library(readr)
library(neuralnet)#Regression
library(nnet)#Classification

concrete <- read.csv(file.choose())

View(concrete)
class(concrete)
summary(concrete)
attach(concrete)

normalize <- function(x) 
{
  
  return((x-min(x))/(max(x)-min(x))) #converting the values into 0 and 1
  
}

concrete_norm <- as.data.frame(lapply(concrete, FUN = normalize))

summary(concrete_norm)  #converted the values into 0 and 1

concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1031,]

concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = concrete_train)

plot(concrete_model)

model_results <- compute(concrete_model, concrete_test[1:8])                              
str(model_results)

predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength, use = "complete.obs") #Use function to remove 'NA' values
plot(predicted_strength, concrete_test$strength) #scatter plot

model5 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = concrete_train, hidden = 5)
plot(model5)  #five nodes with more complexity level

model5$weights
model5_res<- compute(model5, concrete_test[1:8])
pred_strn_5 <- model5_res$net.result

cor(pred_strn_5, concrete_test$strength, use = "complete.obs") #92.87%
plot(pred_strn_5, concrete_test$strength, col = "blue")

#Now the Accuracy is 92.87% so, its the best model we can have. 

