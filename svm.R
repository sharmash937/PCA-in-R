

#===== Support Vector Machine #======
# details https://www.youtube.com/watch?v=Y6RRHw9uN9o
rm(list=ls())

data(iris)
str(iris)

#classification model

library(ggplot2)
qplot(Petal.Length, Petal.Width, data = iris,
       color = Species)

# Support Vector machine
library(e1071)
mymodel <- svm(Species~., data = iris)
summary(mymodel)


# Parameters:
#   SVM-Type:  C-classification # type is classification since, it has y which is classification
#             if it was numeric then it would had been regression
# SVM-Kernel:  radial # basis function
# cost:  1 #default
# gamma:  0.25 
# Number of Support Vectors:  51
# 
# ( 8 22 21 )

plot(mymodel, data = iris, 
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
# Error in plot.new() : figure margins too large ..... clean the plot using broom icon

#support vectors are represented by 'x' in the graphs as seen frm the summary(mymodel)






#confusion matrix and misclassification error
pred <- predict(mymodel, iris)

#create a table with column names as "Predicted" and "Actual"
tab <- table(Predicted = pred, Actual=iris$Species)
tab

#misclassification error/rate
1-sum(diag(tab))/sum(tab)

# u can use "linear" kernel. see the changes. Similarliy kernel='polynomial',or sigmoid
#error is min in radial kernel
mymodel1 <- svm(Species~., data = iris, kernel="linear")
summary(mymodel1)
mymodel1


#==== Tuning

set.seed(123)
tmodel <- tune(svm, Species~., data = iris, 
               ranges=list(epsilon=seq(0,1,0.1)
               ,cost = 2^(2:9)))
#cost captures cost of constraint. Default is 1
#if is too high means high penality. too many support vector. overfitting
#if cost is too low. underfitting
# 8 diff cost values. 11 epsilon values. 88 combinations. it'll give optimal solution
plot(tmodel) #----- recheck
summary(tmodel)
#dark region better results means lower misclassification error



#Best model
mymodel <- tmodel$best.model
summary(mymodel)



#confusion matrix and misclassification error
pred <- predict(mymodel, iris)
tab <- table(Predicted=pred, Actual=iris$Species)
tab
1-sum(diag(tab))/sum(tab)



















































