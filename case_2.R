alldata<- read.csv("C:/Ajay/eLab/compu.csv")
alldata
is.na(alldata)

library(tidyverse)
library(magrittr)
# Impute missing values with mean
data_imputed_1 <- alldata %>%
  mutate(prcc_f_imputed = if_else(is.na(prcc_f), mean(prcc_f, na.rm = TRUE), prcc_f))

data_imputed_2 <- alldata %>%
  mutate(prcc_c_imputed = if_else(is.na(prcc_c), mean(prcc_c, na.rm = TRUE), prcc_c))

data<-alldata %>% filter(is.na(prcc_f)|is.na(prcc_c))
