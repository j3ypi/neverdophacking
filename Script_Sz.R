#Script Simon P-Hacking-Challenge
library(modelr)
library(glmnet)

setwd("C:\\Users\\smzim\\OneDrive\\Dokumente\\Programm\\Github\\neverdophacking")
data.df<-read.csv(".\\Data\\data.csv")

#Data exploration-------------------------------------------------------
summary(data.df) 
sapply(data.df, class)
str(data.df)
capture.output(str(data.df[,names(Filter(is.factor, data.manip.df))]), file = "vars.txt")

#data manipulation-------------------------------------------------------
data.manip.df<-data.df
#DATETIME/DATE/YEAR - Date dismiss for first analysis
data.manip.df$DATETIME<-NULL
data.manip.df$DATE<-NULL
data.manip.df$YEAR<-NULL
data.manip.df$WDWTICKETSEASON<-NULL #dublicate



#convert INSESSION variables to variables without percent

INSESSIONnames<-c("INSESSION", "INSESSION_ENROLLMENT", "INSESSION_WDW", "INSESSION_DLR", "INSESSION_SQRT_WDW", "INSESSION_SQRT_DLR", "INSESSION_CALIFORNIA", "INSESSION_DC", "INSESSION_CENTRAL_FL", "INSESSION_DRIVE1_FL","INSESSION_DRIVE2_FL", "INSESSION_DRIVE_CA", "INSESSION_FLORIDA", "INSESSION_MARDI_GRAS", "INSESSION_MIDWEST", "INSESSION_NY_NJ", "INSESSION_NY_NJ_PA", "INSESSION_NEW_ENGLAND", "INSESSION_NEW_JERSEY", "INSESSION_NOTHWEST", "INSESSION_PLANES", "INSESSION_SOCAL", "INSESSION_SOUTHWEST")
for(i in INSESSIONnames){
  data.manip.df[ ,i] <- as.numeric(sub("%", "", x=data.manip.df[ , i]))
}


#write function to convert factors to dummies
factodum<-function(variable,dataset){
  help.df<-dataset[,c(variable,"MERCHANDISE")]
  numboflev<-nlevels(help.df[,1])
  help.df<-model_matrix(dataset,~help.df[,1]-1)
  numbofcol<-ncol(dataset)
  dataset<-cbind(dataset,help.df)
  
  
  #rename variables
  for(i in 1:numboflev){
    colnames(dataset)[numbofcol+i] <- paste(variable,i)
  }
  dataset[,variable]<-NULL
  
  assign('dataset',dataset, envir=.GlobalEnv)
}

#factornames<-c("HOLIDAY","SEASON","WDW_TICKET_SEASON","WDWEVENTN","WDWRACEN","WDWSEASON", "MKEVENTN", "EPEVENTN", "HSEVENTN", "HOLIDAYJ") - fehler wenn NA in der Spalte drinnen ist glaub ich
factornames<-c("HOLIDAY","SEASON","WDW_TICKET_SEASON","WDWSEASON")

for(i in factornames){
  factodum(i,data.manip.df)
  data.manip.df<-dataset
}
#delete unnecessary variables
factornames<-names(Filter(is.factor, data.manip.df))
for(i in factornames){
  data.manip.df[,i]<-NULL
}

#deleted all variables with less than 50% variables
#create table with percentage NA
data.manip.na.table.df<-data.manip.df
for(i in 1:ncol(data.manip.na.table.df)){
  data.manip.na.table.df[1,i]<-(sum(is.na(data.manip.df[,i]))/nrow(data.manip.na.table.df))
}

data.manip.na.table.df<-data.manip.na.table.df[1,]
rownames(data.manip.na.table.df)<-"Na%"
head(data.manip.na.table.df)

#delete if there is more than x% na - variable to hack p afterwards through grid search
x<-0.2

for(i in 1: ncol(data.manip.na.table.df)){
  if(data.manip.na.table.df[,i]>x){
    data.manip.df[,i]<-NULL
  }
}

write.csv(data.manip.df,".\\Data\\Manipulated_Data.csv")

#Analysis-------------------------------------------------------
#idea: 1. we select relevant variables through a lasso regression and run diferent regression models
#      2. Brute force
#---covariance matrix checken

#1. -------------------------------------------------------

#train dataset
x<-data.manip.df[,-2]
y<-data.manip.df[,2]
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]


#select vars to keep with lasso
na_index <- is.na(y)

#we do have NAs hence PLS does not work
lambda <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x[!na_index,], y[!na_index], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)

