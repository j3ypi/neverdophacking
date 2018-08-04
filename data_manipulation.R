
library(tidyverse)
library(here)
library(rio)
library(magrittr)
library(modelr)
library(lubridate)

source("factordum.R")
daten <- import(here("Data", "data.csv"), setclass = "tbl")
daten <- import("data.csv", setclass = "tbl")

percent <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}

daten %<>% 
  mutate_at(vars(starts_with("INSESSION")), funs(percent)) 

###################
# Data prep LASSO
###################

daten_lasso <- daten %>% 
  mutate_if(is_character, as_factor) %>% 
  select(-DATETIME, -DATE, -YEAR, -WDWTICKETSEASON) %>% 
  as.data.frame()

factornames <- c("HOLIDAY", "SEASON", "WDW_TICKET_SEASON", "WDWSEASON")

for (i in factornames) {
  factodum(i, daten_lasso)
  daten_lasso <- dataset
}

#delete unnecessary variables
factornames <- names(Filter(is.factor, daten_lasso))
for (i in factornames) {
  daten_lasso[,i] <- NULL
}


data.manip.na.table.df <- daten_lasso
for (i in 1:ncol(data.manip.na.table.df)) {
  data.manip.na.table.df[1, i] <- (sum(is.na(daten_lasso[ ,i])) / nrow(data.manip.na.table.df))
}

data.manip.na.table.df <- data.manip.na.table.df[1,]
rownames(data.manip.na.table.df) <- "Na%"
head(data.manip.na.table.df)

missings <- colnames(data.manip.na.table.df[ ,data.manip.na.table.df > 0.2])

daten_lasso %<>% 
  select(-missings) %>% 
  na.omit() 

export(daten_lasso, here("Data", "daten_lasso.csv"))

###################
# Data prep EFA
###################

daten %<>% 
  as.data.frame()

data.manip.na.table.df <- daten
for (i in 1:ncol(data.manip.na.table.df)) {
  data.manip.na.table.df[1, i] <- (sum(is.na(daten[ ,i])) / nrow(data.manip.na.table.df))
}

data.manip.na.table.df <- data.manip.na.table.df[1,]
rownames(data.manip.na.table.df) <- "Na%"
head(data.manip.na.table.df)

missings <- colnames(data.manip.na.table.df[ ,data.manip.na.table.df > 0.5])

daten %<>% 
  select(-missings) %>% 
  as_tibble()

daten %<>% 
  mutate(DATETIME = str_extract(DATETIME, "\\s(.*)"),
         DATETIME = str_replace(DATETIME, "\\s", ""),
         DATETIME = hms(DATETIME),
         DATE = mdy(DATE)) 

export(daten, here("Data", "daten_manip.rda"))


