rm(list=ls())
help("read_excel")
#dataset
setwd("C:/Ajay/eLab")
d<- read.delim(file="sicdatas.txt")
which(is.na(d))

###Part 1
#table of firms in SIC Code
td<-data.frame(d$gvkey,d$sic)
td <-unique(td)
table(td$`d.sic`)
#select Indrustries (eating places)
library(dplyr)

ds <- filter(d, sic == 5812)
which(is.na(ds))
unique(ds$conml)

###Part2
library(readxl)
#od<-read_excel("/Users/admin/Desktop/eLab/owndata.xlsx")
od1<-read_excel("C:/Ajay/eLab/case2.xlsx")
combd1<- left_join(ds, od1, by = c("gvkey"))

###Part3
## correct tt variable
library(dplyr)
combd1 <- combd1 %>%
  mutate(tiktok = ifelse( fyear<year, 0,tiktok))

##check outliers and missing values
library(dlookr)
library(flextable)
diagnose_numeric(combd1)%>% flextable()
diagnose(combd1)%>%flextable()
which(is.na(combd1$act))
##correct missing values
combd1$tiktok<- ifelse(is.na(combd1$tiktok), 0, combd1$tiktok)

#act
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_act = mean(act, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$act <- ifelse(is.na(combd1$act), combd1$mean_act, combd1$act)

combd1$mean_act <- NULL

#at
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_at = mean(at, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$at <- ifelse(is.na(combd1$at), combd1$mean_at, combd1$at)

combd1$mean_at.x<- NULL
combd1$mean_at.y<-NULL

#ceq
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_ceq = mean(ceq, na.rm = TRUE))

combd1 <- left_join(combd, df_mean, by = "gvkey")

combd1$ceq <- ifelse(is.na(combd1$ceq), combd1$mean_ceq, combd1$ceq)

combd1$mean_ceq <- NULL

#csho at the end I changed the rest of na to 0 bc those company may be private or we dont have any informaton about it
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_csho = mean(csho, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$csho <- ifelse(is.na(combd1$csho), combd1$mean_csho, combd1$csho)

combd1$mean_csho <- NULL

combd1$csho <- ifelse(is.na(combd1$csho), 0, combd1$csho)

#dltt
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_dltt = mean(dltt, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$dltt <- ifelse(is.na(combd1$dltt), combd1$mean_dltt, combd1$dltt)

combd1$mean_dltt <- NULL

#emp at the end I changed rest of Na into median of all variable because of the lack of a data
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_emp = mean(emp, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$emp <- ifelse(is.na(combd1$emp), combd1$mean_emp, combd1$emp)

combd1$mean_emp <- NULL

combd1$emp <- ifelse(is.na(combd1$emp), median(combd1$emp,na.rm = TRUE), combd$emp)

#lct
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_lct = mean(lct, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$lct <- ifelse(is.na(combd1$lct), combd1$mean_lct, combd1$lct)

combd1$mean_lct <- NULL


#ni
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_ni = mean(ni, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$ni <- ifelse(is.na(combd1$ni), combd1$mean_ni, combd1$ni)

combd1$mean_ni <- NULL

#sale
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_sale= mean(sale, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$sale <- ifelse(is.na(combd1$sale), combd1$mean_sale, combd1$sale)

combd1$mean_sale <- NULL

#xad at the end I changed the rest of na to 0 bc the cost may be not relevant so they are 0 or close to 0
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_xad = mean(xad, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$xad <- ifelse(is.na(combd1$xad), combd1$mean_xad, combd1$xad)
combd1$mean_xad <- NULL
combd1$mean_xad.x <- NULL
combd1$mean_xad.y <- NULL

combd1$xad <- ifelse(is.na(combd1$xad), 0, combd1$xad)

#xsga at the end I changed rest of Na into median of all variable because of the lack of a data
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_xsga = mean(xsga, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$xsga <- ifelse(is.na(combd1$xsga), combd1$mean_xsga, combd1$xsga)

combd1$mean_xsga <- NULL

combd1$xsga <- ifelse(is.na(combd1$xsga), median(combd1$xsga,na.rm = TRUE), combd1$xsga)

#prcc_f at the end I changed rest of na into 0 bc it may mean that company is just not public
df_mean <- combd1 %>% 
  group_by(gvkey) %>% 
  summarize(mean_prcc_f= mean(prcc_f, na.rm = TRUE))

combd1 <- left_join(combd1, df_mean, by = "gvkey")

combd1$prcc_f <- ifelse(is.na(combd1$prcc_f), combd1$mean_prcc_f, combd1$prcc_f)

combd1$mean_prcc_f <- NULL

combd1$prcc_f <- ifelse(is.na(combd1$prcc_f), 0, combd1$prcc_f)

##make new Variables of Interest
combd1$tobinsq <- (combd1$at+(combd1$csho*combd1$prcc_f)-combd1$ceq)/combd1$at
combd1$ros<- combd1$ni/combd1$sale
combd1$ai<- combd1$xad/combd1$sale
combd1$mi<-combd1$xsga/combd1$at
combd1$fs<-log(combd1$emp)
combd1$fliq<- combd1$act/combd1$lct
combd1$flev<-combd1$dltt/combd1$at

##check and correct variables of interest
diagnose_numeric(combd1)%>%flextable()
diagnose(combd1)%>%flextable()

combd1$tobinsq<- ifelse(combd1$tobinsq== "Inf",0,combd1$tobinsq)
combd1$ros<- ifelse(combd1$ros== "Inf",0,combd1$ros)
combd1$ros<- ifelse(combd1$ros== "-Inf",0,combd1$ros)
combd1$ai<- ifelse(combd1$ai== "Inf",0,combd1$ai)
combd1$ai<- ifelse(is.na(combd1$ai),0,combd1$ai)
combd1$mi<- ifelse(combd1$mi== "Inf",0,combd1$mi)
combd1$fs<- ifelse(combd1$fs== "-Inf",0,combd1$fs)
combd1$flev<-ifelse(is.na(combd1$flev),0,combd1$flev)

combd1$tobinsq<-NULL
combd1$ros<-NULL
combd1$ai<-NULL
combd1$mi<-NULL
combd1$fs<-NULL
combd1$flev<-NULL
combd1$fliq<-NULL


