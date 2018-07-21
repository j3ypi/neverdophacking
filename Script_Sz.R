#Script Simon P-Hacking-Challenge
library(modelr)

setwd("C:\\Users\\smzim\\OneDrive\\Dokumente\\Programm\\Github\\neverdophacking")
data.df<-read.csv(".\\Data\\data.csv")

#Data exploration
summary(data.df) 
sapply(data.df, class)
str(data.df)

#data manipulation
data.manip.df<-data.df
#DATETIME/DATE/YEAR - Date dismiss for first analysis
data.manip.df$DATETIME<-NULL
data.manip.df$DATE<-NULL
data.manip.df$YEAR<-NULL

#write function to cobert factors to dummies
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
  
  assign('dataset',dataset, envir=.GlobalEnv)
}

#WDW_TICKET_SEASON - 3 Factors transform to dummy
factodum("WDW_TICKET_SEASON",data.manip.df)
data.manip.df<-dataset
#SEASON - 17 Factors transform to dummy


#deleted alö variables with less than 50% variables

