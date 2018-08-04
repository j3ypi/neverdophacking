
library(tidyverse)
library(here)
library(rio)
library(broom)
library(glmnet)
library(remef)

data.manip.df <- import(here("Data", "daten_lasso.csv"))

# baseline model
base_model <- lm(MERCHANDISE ~ SPOSTMIN, data = data.manip.df)
tidy(base_model)
glance(base_model)

#################################
# LASSO Regression
#################################

set.seed(123)

#Analysis-------------------------------------------------------
#idea: 1. we select relevant variables through a lasso regression and run diferent regression models
#      2. Brute force
#---covariance matrix checken

#1. -------------------------------------------------------

#split dataset
x<-as.matrix(data.manip.df[,-2])
y<-data.manip.df[,2]
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

#model- Select variables through LASSO
lambda <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
bestlam <- 3.5
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)

#save all nonzero coefficients in dataframe m
lasso.coef.list<-as.list(lasso.coef)
n<-1
c<-2
m<-data.frame(data.manip.df$MERCHANDISE)
for(i in lasso.coef.list){
  if(i!=0){
    
    m<-cbind(m,data.manip.df[,n])
    colnames(m)[c]<-(names(data.manip.df)[n])
    c<-c+1
  }
  n<-n+1
}

#delete dublicates
m[,1:2]<-NULL

fit<-(lm(MERCHANDISE~.,m))
summary(fit)
coef(summary(fit))["SPOSTMIN", 4] #-p-value

#############
# 0.0132773 3
#############

# 0.008009868 123






# Task 539564053 failed: Unhandled Exception: Child Task 539564059 error: Unhandled Exception: {"message":"VolumeDriver.Mount: Error mounting volume_name='volume-278079': Failed to map image='96e46ef0-b5ca-4315-a6ea-cb5594291079': Error running command: 'sudo -u root rbd nbd map







