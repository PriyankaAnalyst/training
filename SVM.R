# classify the Size_Categorie using SVM, Size_Categorie the burned area of the forest ( Small , Large)


library(kernlab)
library(caret)
library(plyr)
library(e1071)

forest_data <- read.csv(file.choose())

str(forest_data)
View(forest_data)

# The area value has lots of zeros
 
hist(forest_data$area)  #histrogram plot for area field, and checking the zeros count
rug(forest_data$area)  #rug checking the zero count

summary(forest_data)


# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Applying Normalization Method :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forest_data$temp = normalize(forest_data$temp)
forest_data$RH   = normalize(forest_data$RH)
forest_data$wind = normalize(forest_data$wind)
forest_data$rain = normalize(forest_data$rain)

attach(forest_data)

#Train and Test dataset.Data Partitioning


set.seed(123) #reproducing the results

forest_train <- forest_data[1:400, ]
forest_test <- forest_data[401:517, ]

?kernel

model1 <- ksvm(size_category~temp+rain+wind+RH, data = forest_data, kernel = "vanilladot")
model1

area_pred <- predict(model1, forest_test)

table(area_pred,forest_test$size_category) #confusion matrix

agreement <- area_pred == forest_test$size_category
table(agreement)

prop.table(table(agreement))

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rbfdot 

model_rbfdot <- ksvm(size_category~temp+rain+wind+RH, data = forest_test, kernel = "rbfdot")

pred_rbfdot<-predict(model_rbfdot,newdata=forest_test)

mean(pred_rbfdot==forest_test$size_category) # 74.35

# kernal = besseldot

model_besseldot <- ksvm(size_category~temp+rain+wind+RH, data = forest_test, kernel = "besseldot")

pred_besseldot<-predict(model_besseldot,newdata=forest_test)

mean(pred_besseldot==forest_test$size_category) # 71.79

# kernel = polydot

model_polydot <- ksvm(size_category~temp+rain+wind+RH, data = forest_train, kernel = "polydot")

pred_polydot<-predict(model_polydot,newdata=forest_test)

mean(pred_polydot==forest_test$size_category) # 70.94

****************************************************

******************************************************************************

#2. Prepare a classification model using SVM for salary data.

install.packages("kernlab")

library(psych)

library("ggplot2")
library(kernlab)
library(caret)

train_sal <- read.csv(file.choose())
str(train_sal)

test_sal <- read.csv(file.choose())
str(test_sal)

train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)

test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)

#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() + ggtitle("Box Plot")

model1<-ksvm(train_sal$Salary ~.,data = train_sal,kernel = "vanilladot")
model1

sal_pred<-predict(model1,test_sal)
head(sal_pred)
table(sal_pred,test_sal$Salary)

agreement<-sal_pred==test_sal$Salary
table(agreement)

prop.table(table(agreement))#84% accurate

model2<-ksvm(train_sal$Salary ~.,data = train_sal,kernel = "rbfdot")
model2
sal_pred<-predict(model1,test_sal)
head(sal_pred)
table(sal_pred,test_sal$Salary)

agreement1<-sal_pred==test_sal$Salary
table(agreement1)
prop.table(table(agreement1)) #84% accurate

###################################################################


