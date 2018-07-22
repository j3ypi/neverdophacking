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

#baseline model - 0.114 = p
summary(lm(data.manip.df$MERCHANDISE~data.manip.df$SPOSTMIN,data.manip.df))

#delete if there is more than x na - variable to hack p afterwards through grid search
x<-100

data.manip.df<-data.manip.df[colSums(!is.na(data.manip.df)) > x]
data.manip.df<-data.manip.df[complete.cases(data.manip.df),]
write.csv(data.manip.df,".\\Data\\Manipulated_Data.csv")

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
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)

#save all nonzero coefficients
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


m[,1:2]<-NULL

summary(lm(MERCHANDISE~.,m))
#bin zu Faul die namen anzugleichen aber wir haben jetzt p=0.089446