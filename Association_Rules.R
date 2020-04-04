****************************************************************
Prepare rules for the all the data sets 
1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
2) Change the minimum length in apriori algorithm
3) Visulize the obtained rules using different plots 

#Books

install.packages("arules")
install.packages("arulesViz")

library("arules")
library("arulesViz")

book <- read.csv(file.choose())

View(book)

rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))

# Provided the rules with 2 % Support, 50 % Confidence and Minimum to purchase 
# 5 books 

rules

inspect(head(sort(rules, by = "lift")))  

head(quality(rules))

plot(rules,method = "scatterplot")

plot(rules,method = "grouped")

# The Art books are being sold at a larger extent along with other Cook, art, geo, child books
# Cook books are also being sold at a larger extent along with other child, art, geo, Doit books)

plot(rules,method = "graph")

#Movies

library(arules)

mymovies <- read.csv(file.choose())


rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))

# Provided the rules with 2% Support, 50 % Confidence and watched a minimum of 2 movies

rules

inspect(head(sort(rules, by = "lift"))) 

head(quality(rules))


plot(rules,method = "scatterplot")

plot(rules, method = "grouped")

# It looks ike most of them has watched Lord of the rings movies along with Gladiator and Greenville
# Also most of them has watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator. 

plot(rules,method = "graph")
