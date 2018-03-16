library(data.table)
library(sandwich)
library(ggplot2)
library(lmtest)
library(tseries)
library(plm)
library(MASS)
library(RcmdrMisc)
library(evtree)
library(party)
library(dplyr)
library(devtools)
library(psych)
#install.packages("scales")
#install_github("ggbiplot", "vqv")
library(ggbiplot)
library(scales)
library(nnet)


rate <- read.csv(file.choose(), header = T)
set.seed(123)
ind <- sample(2, nrow(rate), replace = T,
              prob = c(0.7, 0.3))
train <- rate[ind==1,]
test <- rate[ind==2,]
str(train)
train$Resolve_Accept._Decline._Undecided <- as.factor(train$Resolve_Accept._Decline._Undecided)


#Scatter Plot & Correlations
pairs.panels(train[,c(-1,-2,-7,-11,-14,-8,-10,-12,-5)],
             gap = 0,#gap b/w scatter plot
             bg=rainbow(12)[train$Resolve_Accept._Decline._Undecided],
             pch=21)

#principle component analysis
#Principal Component Analysis (PCA) is used to explain the variance-covariance structure of a set of variables through linear combinations. It is often used as a dimensionality-reduction technique.
pc <- prcomp(train[,c(13:25)],
             center = T,
             scale. = T)

# GLM model
# modelxxx <- glm(train$Resolve_Accept._Decline._Undecided~
#                   Overall.Work.Satisfaction+Overall.Team.Envi.Satisfaction+Overall.Intern.Supervisor.Satisfaction+Overall.Cultural.Satisfaction+Overall.Diff.Salary+Overall.Intern.Wage.view+Overall.receive.ft+overall.ft.accept.or.reject+Overall.pref.loc+overall.diff.loc+overall.diff.role+overall.recom.supervisor+overall.recom.TI,
#                   data = train, 
#                 family = binomial('logit'))
# 
# colnames(train)
# summary(modelxxx)
# 
# coeftest(modelxxx, vcov. = vcovHC)

attributes(pc)
pc$center
pc$x
print(pc)
summary(pc)

# orthogonality of PC's
pairs.panels(pc$x,
             gap=0,
             bg=c("red", "yellow")[train$Resolve_Accept._Decline._Undecided],
             pch=21)
#helps get rid of multicolinearity prob
#In statistics, multicollinearity (also collinearity) is a phenomenon in which one predictor variable in a multiple regression model can be linearly predicted from the others with a substantial degree of accuracy


#Bi Plot
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = train$Resolve_Accept._Decline._Undecided,
              ellipse = T,
              circle = T,
              ellipse.prob = 0.68)
g <- g+scale_color_discrete(name='')
g <- g+theme(legend.direction = 'horizontal',
             legend.position = 'top')
print(g)

#Prediction with principal Components
trg <- predict(pc, train)

trg <- data.frame(trg, train[12])


tst <- predict(pc, test)
tst <- data.frame(tst, test[12])


#multinomial log regression
trg$Resolve_Accept._Decline._Undecided <- relevel(
  trg$Resolve_Accept._Decline._Undecided, ref = '0'
)


mymodel <- multinom(Resolve_Accept._Decline._Undecided~
                      PC1+PC2+PC3+PC4+PC5, trg)

summary(mymodel)

#Misclassification for train
p <- predict(mymodel, trg)
tab <- table(p,trg$Resolve_Accept._Decline._Undecided)
tab

1-sum(diag(tab))/sum(tab)


#Misclassification for test
pt <- predict(mymodel, tst)
tab.test <- table(pt, tst$Resolve_Accept._Decline._Undecided)
tab.test

1-sum(diag(tab.test))/sum(tab.test)







