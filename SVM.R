# classify the Size_Categorie using SVM, Size_Categorie the burned area of the forest ( Small , Large)

library(kernlab)
library(caret)


forest <- read.csv(choose.files())

# train and test analysis

fires_train<-forest[1:450,]

fires_test<-forest[451:517,]

model1<-ksvm(size_category ~.,data = fires_train,kernel = "vanilladot")
model1

fire_pred<-predict(model1,fires_test)
head(fire_pred)
table(fire_pred,fires_test$size_category)

agreement<-fire_pred==fires_test$size_category
table(agreement)
prop.table(table(agreement)) #98% is the accuracy.

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


