##################
# neverdophacking
##################
library(tidyverse)
library(here)
library(rio)
library(broom)
library(magrittr)
library(modelr)
library(glmnet)

source("factordum.R")
data.df <- import(here("Data", "data.csv"), setclass = "tbl")

# Data exploration-------------------------------------------------------
summary(data.df) 
sapply(data.df, class)
str(data.df)
# capture.output(str(data.df[,names(Filter(is.factor, data.manip.df))]), file = "vars.txt")

# Data manipulation-------------------------------------------------------
data.manip.df <- data.df %>% 
  mutate_if(is_character, as_factor)
# erstes mal fuer mich, dass Faktoren tatsaechlich von Nutzen sind

# DATETIME/DATE/YEAR - Date dismiss for first analysis
data.manip.df$DATETIME <- NULL
data.manip.df$DATE <- NULL
data.manip.df$YEAR <- NULL
data.manip.df$WDWTICKETSEASON <- NULL #dublicate

# Convert INSESSION variables to variables without percent sign into numeric
percent <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}

data.manip.df %<>% 
  mutate_at(vars(starts_with("INSESSION")), funs(percent)) %>% 
  as.data.frame()

factornames <- data.manip.df %>%
  select_if(is.factor) %>% 
  names() 

# Function to convert factors to dummies
# (funktioniert nur mit data.frame, nicht tibbles)
data.manip.df <- factodum(data.manip.df, factornames)

#delete unnecessary variables
factornames <- names(Filter(is.factor, data.manip.df))

for (i in factornames) {
  data.manip.df[ ,i] <- NULL
}

# deleted all variables with less than 50% variables
# create table with percentage NA
# Preis fuer laengsten Datensatznamen? <3 Aber Nice gemacht
data.manip.na.table.df <- data.manip.df
for (i in 1:ncol(data.manip.na.table.df)) {
  data.manip.na.table.df[1, i] <- (sum(is.na(data.manip.df[ ,i])) / nrow(data.manip.na.table.df))
}

data.manip.na.table.df <- data.manip.na.table.df[1,]
rownames(data.manip.na.table.df) <- "Na%"
head(data.manip.na.table.df)

# baseline model
base_model <- lm(MERCHANDISE ~ SPOSTMIN, data = data.manip.df)
tidy(base_model)
glance(base_model)

# delete if there is more than x% na - variable to hack p afterwards through grid search
# funktionierte vorher nicht
x <- 0.5

missings <- colnames(data.manip.na.table.df[ ,data.manip.na.table.df > x])

data.manip.df %>% 
  select(-missings) %>% 
  as_tibble()

# data.manip.df <- data.manip.df[colSums(!is.na(data.manip.df)) > 0]

export(data.manip.df, here("Data", "Manipulated_Data.csv"))

#Analysis-------------------------------------------------------
#idea: 1. we select relevant variables through a lasso regression and run diferent regression models
#      2. Brute force
#---covariance matrix checken

#1. -------------------------------------------------------

# Split dataset
x <- as.matrix(data.manip.df[ ,-2])
y <- data.manip.df[ ,2]
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
ytest <- y[test]

isna <- is.na(y)
# Model
LASSO1 <- glmnet(x[!isna,], y[!isna], "gaussian", alpha = 1)
vars <- predict(LASSO1, type = 'coefficients', s = 0.01)
# nicht vollstaendig bin muede
lasso_model <- lm(MERCHANDISE ~ SPOSTMIN + INSESSION_DRIVE1_FL + AKEMHMORN + AKEMHMTOM + AKHOURSEMH + HSHOURSEMHTOM + 
                    WDWMINTEMP + WEATHER_WDWPRECIP+ CAPACITYLOST_MK,data.manip.df, data = data.manip.df)
tidy(lasso_model)
glance(lasso_model)