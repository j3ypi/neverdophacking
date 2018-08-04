
library(tidyverse)
library(here)
library(rio)
library(magrittr)
library(broom)
library(modelr)

source("factordum.R")
# 
# daten <- import(here("Data", "daten_manip.rda"), setclass = "tbl")

daten <- import(here("Data", "daten_lasso.csv"), setclass = "tbl")

daten_int <- daten %>% 
  select_if(is.numeric)

model <- lm(MERCHANDISE ~ ., data = daten_int)
tidy(model)
# 0.0534

daten <- import(here("Data", "daten_lasso.csv"), setclass = "tbl")

mke <- daten %>% 
  select(starts_with("MKE")) %>% 
  names()

daten %<>% 
  select(-HOLIDAYM, -INSESSION_SQRT_WDW, -INSESSION_SQRT_DLR, -INSESSION_CALIFORNIA,
         -INSESSION_DC, -INSESSION_CENTRAL_FL, -INSESSION_DRIVE2_FL, -INSESSION_DRIVE_CA,
         -INSESSION_FLORIDA, -INSESSION_NY_NJ, -INSESSION_NY_NJ_PA, -INSESSION_NEW_ENGLAND,
         -INSESSION_NEW_JERSEY, -INSESSION_NOTHWEST, -INSESSION_PLANES, -INSESSION_SOCAL,
         -INSESSION_SOUTHWEST, -mke, -MKHOURSEMHTOM, -HSEMHMYEST, -HSEMHEVE, -HSEMHETOM)

fac <- factanal(daten[ ,1:37], factors = 4, rotation = "varimax")

fac$loadings

print(fac, digits = 2, cutoff = .3, sort = T)

fac %>% 
  print(digits = 2, cutoff = .3, sort = T)

fac %>% 
  tidy() %>% 
  summarise(test = sum(fl1))

library(psy)

scree.plot(fac$correlation)

names(daten[ ,38])
