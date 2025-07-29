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
od<-read_excel("C:/Ajay/eLab/case2.xlsx")
combd<- left_join(ds, od, by = c("gvkey","conml"))

###Part3
## correct tt variable
combd <- combd %>%
  mutate(
    tt = ifelse(fyear < ttyear, 0, tt)
  )
combd$ttyear<-NULL

##check outliers and missing values
library(dlookr)
library(flextable)
diagnose_numeric(combd)%>% flextable()
diagnose(combd)%>%flextable()
which(is.na(combd$prcc_f))

##correct missing values
combd$tt<- ifelse(is.na(combd$tt), 0, combd$tt)

#correcting missing values with mean of company observation 
#act
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_act = mean(act, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$act <- ifelse(is.na(combd$act), combd$mean_act, combd$act)

combd$mean_act <- NULL

#at
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_at = mean(at, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$at <- ifelse(is.na(combd$at), combd$mean_at, combd$at)

combd$mean_at <- NULL

#ceq
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_ceq = mean(ceq, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")
comdb<- left_join(combd, df_mean, arrange)

combd$ceq <- ifelse(is.na(combd$ceq), combd$mean_ceq, combd$ceq)

combd$mean_ceq <- NULL

#csho
df_mean <- combd %>%   
group_by(gvkey) %>% 
  summarize(mean_csho = mean(csho, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$csho <- ifelse(is.na(combd$csho), combd$mean_csho, combd$csho)

combd$mean_csho <- NULL


#dltt
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_dltt = mean(dltt, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$dltt <- ifelse(is.na(combd$dltt), combd$mean_dltt, combd$dltt)

combd$mean_dltt <- NULL

#emp 
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_emp = mean(emp, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$emp <- ifelse(is.na(combd$emp), combd$mean_emp, combd$emp)

combd$mean_emp <- NULL

#lct
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_lct = mean(lct, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$lct <- ifelse(is.na(combd$lct), combd$mean_lct, combd$lct)

combd$mean_lct <- NULL

#ni
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_ni = mean(ni, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$ni <- ifelse(is.na(combd$ni), combd$mean_ni, combd$ni)

combd$mean_ni <- NULL

#sale
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_sale= mean(sale, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$sale <- ifelse(is.na(combd$sale), combd$mean_sale, combd$sale)

combd$mean_sale <- NULL

#xad 
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_xad = mean(xad, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$xad <- ifelse(is.na(combd$xad), combd$mean_xad, combd$xad)

combd$mean_xad <- NULL

#xsga 
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_xsga = mean(xsga, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$xsga <- ifelse(is.na(combd$xsga), combd$mean_xsga, combd$xsga)

combd$mean_xsga <- NULL

#prcc_f 
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_prcc_f= mean(prcc_f, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")

combd$prcc_f <- ifelse(is.na(combd$prcc_f), combd$mean_prcc_f, combd$prcc_f)

combd$mean_prcc_f <- NULL

# Correct imputing na
combd$xad <- ifelse(is.na(combd$xad), 0, combd$xad) #because those variables are close to zero

#data found on the internet
combd[448:452,"emp"]<-1.065 
combd[463:465,"emp"]<-5
combd[487,"emp"]<-30
combd[488,"emp"]<-37

combd$xsga<-ifelse(is.na(combd$xsga),0,combd$xsga) #most of data found on internet is close to zero

which(is.na(combd$csho))
which(is.na(combd$prcc_f))
combd<- combd[-c(68,69,103:108,232:234,275:284,375,376,403:408,431,432,475:483,549,558,559),] #those comapnies don't have any values for csho or prcc_f

library(flextable)

##make new Variables of Interest
combd$tobinsq <- (combd$at+(combd$csho*combd$prcc_f)-combd$ceq)/combd$at
combd$ros<- combd$ni/combd$sale
combd$ai<- combd$xad/combd$sale
combd$mi<-combd$xsga/combd$at
combd$fs<-log(combd$emp)
combd$fliq<- combd$act/combd$lct
combd$flev<-combd$dltt/combd$at

##check and correct variables of interest
diagnose_numeric(combd)%>%flextable()
diagnose(combd)%>%flextable()

combd$emp<-ifelse(combd$emp==0,0.001,combd$emp)
combd$fs<-log(combd$emp)

#Some companies only invest in restaurant that's why there sales are 0 so we will remove them
combd <- combd[!grepl("bowmo", combd$conml, ignore.case = TRUE), ]
combd <- combd[!grepl("cordia", combd$conml, ignore.case = TRUE), ]
combd <- combd[!grepl("Grey Fox", combd$conml, ignore.case = TRUE), ]
combd <- combd[!grepl("DBUB", combd$conml, ignore.case = TRUE), ]

#deltaco must have missing sales in 2014, also burgerifi in 2018 and 2019 so we will correct them with the mean of this company sales
df_mean <- combd %>% 
  group_by(gvkey) %>% 
  summarize(mean_sale = mean(sale, na.rm = TRUE))

combd <- left_join(combd, df_mean, by = "gvkey")
combd$sale <- ifelse(combd$sale==0, combd$mean_sale, combd$sale)
combd$mean_sale <- NULL

combd$ros<- combd$ni/combd$sale
combd$ai<- combd$xad/combd$sale

library(tibble)
##create new dataset with variables of interest
vi<- tibble(combd$flev,combd$fliq,combd$fs,combd$mi,combd$ai,combd$ros,combd$tobinsq,combd$tt,combd$sic,combd$conml,combd$gvkey,combd$fyear)
vi1<-tibble(combd$flev,combd$fliq,combd$fs,combd$mi,combd$ai,combd$ros,combd$tobinsq,combd$tt,combd$sic,combd$conml,combd$gvkey,combd$fyear)
colnames(vi) <- gsub("combd.", "", colnames(vi))
vi$sic<- 1
library(ggplot2)
diagnose_numeric(vi)%>%flextable()
diagnose(vi)%>%flextable()

#check outliers in Variables of Interest
z_scores <- as.data.frame(sapply(vi[,c(1:7,11)], scale))
outliers <- which(apply(z_scores, 1, function(x) any(abs(x) > 3)))
outlier_data <- vi[,c(1:7,11)][outliers, ]
outlier_data$row_number <- outliers
outlier_data$variables <- apply(z_scores[outliers, ], 1, function(x) paste(names(x)[abs(x) > 3], collapse = ", "))
outlier_data
table(outlier_data$variables)

#Winsorize variables with outliers
trim_level <- 0.05 #we are choosing 0.05 percent because the amount of outliers is not that big
library(DescTools)
winsorize <- function(x, trim_level) {
  q_lower <- quantile(x, trim_level)
  q_upper <- quantile(x, 1-trim_level)
  ifelse(x < q_lower, q_lower, ifelse(x > q_upper, q_upper, x))
}


library(robustbase)
vi$flev.w<- winsorize(vi$flev,trim_level)
vi$fliq.w<- winsorize(vi$fliq,trim_level)
vi$fs.w<- winsorize(vi$fs,trim_level)
vi$mi.w<- winsorize(vi$mi,trim_level)
vi$ai.w<- winsorize(vi$ai,trim_level)
vi$ros.w<- winsorize(vi$ros,trim_level)
vi$tobinsq.w<- winsorize(vi$tobinsq,trim_level)



##summarize variables 
diagnose_numeric(vi)%>%
  select(-"outlier")%>%
  flextable()
diagnose(vi)%>%flextable()

##Visualize 
#histograms
library(ggplot2)

ggplot(vi, aes(x = flev.w)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "flev", y = "Frequency") +
  ggtitle("flevw")
ggplot(vi, aes(x = flev)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "flev", y = "Frequency") +
  ggtitle("flev")

ggplot(vi, aes(x = fliq.w)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "fliq", y = "Frequency") +
  ggtitle("fliqw")
ggplot(vi, aes(x = fliq)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "fliq", y = "Frequency") +
  ggtitle("fliq")

install.packages("ggplot2")
library(ggplot2)
ggplot(vi, aes(x = fs.w)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "fs", y = "Frequency") +
  ggtitle("fsw")
ggplot(vi, aes(x = fs)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "fs", y = "Frequency") +
  ggtitle("fs")

ggplot(vi, aes(x = mi.w)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "mi", y = "Frequency") +
  ggtitle("miw")
ggplot(vi, aes(x = mi)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "mi", y = "Frequency") +
  ggtitle("mi")

ggplot(vi, aes(x = ai.w)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "ai", y = "Frequency") +
  ggtitle("aiw")
ggplot(vi, aes(x = ai)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "ai", y = "Frequency") +
  ggtitle("ai")

ggplot(vi, aes(x = ros.w)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "ros", y = "Frequency") +
  ggtitle("ROSw")
ggplot(vi, aes(x = ros)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "ros", y = "Frequency") +
  ggtitle("ROS")

ggplot(vi, aes(x = tobinsq.w)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "tobinsq", y = "Frequency") +
  ggtitle("Tobinsqw")
ggplot(vi, aes(x = tobinsq)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(x = "tobinsq", y = "Frequency") +
  ggtitle("Tobinsq")

#by looking at histograms we decide to work on winsorized data 
#tt and years
vi$fyear<-as.factor(vi$fyear)
vi$tt<-as.factor(vi$tt)
ggplot(vi, aes(x = fyear, fill = tt)) +
  geom_bar(color="white")

# to compare data looking histogram filled with  tt variable we should look since 2019 because from this year tik tok occur in our dataset

ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = flev.w, fill = tt)) +
geom_histogram(color="white",position = "fill")

ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = fliq.w, fill = tt)) +
  geom_histogram(color="white",position = "fill")

ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = fs.w, fill = tt)) +
  geom_histogram(color="white",position = "fill")

ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = mi.w, fill = tt)) + 
  geom_histogram(color="white",position = "fill")

ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = ai.w, fill = tt)) + 
  geom_histogram(color="white",position = "fill")

ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = ros.w, fill = tt)) + 
  geom_histogram(color="white",position = "fill") 

ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = tobinsq.w, fill = tt)) +
  geom_histogram(color="white",position = "fill")


#size of firm, we can seperate observations to small mid and big companies
breaks<- c(-7,1.01,3.44,6.3)
vi$groups <- cut(vi$fs, breaks = breaks, labels = c("SMALL", "MID", "BIG"))
table(vi$groups)


ggplot(vi%>% filter(fyear==c(2019:2022)), aes(x = tt, fill= groups)) +
  geom_bar()

ggplot(vi, aes(x = flev.w, fill= groups)) +
  geom_histogram()

ggplot(vi, aes(x = fliq.w, fill= groups)) +
  geom_histogram()

ggplot(vi, aes(x = mi.w, fill= groups)) +  #small companies invest more of they money on marketing 
  geom_histogram()

ggplot(vi, aes(x = ai.w, fill= groups)) +  # same for advertising 
  geom_histogram()

ggplot(vi, aes(x = ros.w, fill= groups)) + # Most of the companies with negative Return on Sales are small companies 
  geom_histogram()


ggplot(vi, aes(x = tobinsq.w, fill= groups)) +
  geom_histogram()


vi$groups<-NULL

##make correlation table
library(corrplot)

vi$fyear<- as.numeric(vi$fyear)

corrplot(cor(vi[,c(1:8,12)]), method = "color", type = "upper", order = "hclust", 
         tl.col = "black", addCoef.col = "black", 
         addCoefasPercent = TRUE, diag = FALSE)

###Part 4

## Aggregate
vi$tt<-as.numeric(vi$tt)
vi$tt<-ifelse(vi$tt==1,0,1) # I run this code because after changing into numeric it change it from 0 and 1 to 1 and 2

csd <- vi %>%
  group_by(gvkey) %>%
  summarise(across(.cols = c(1:18), .fns = mean))

##make regression model
#Independent variables are winsorized and dependent are no
Tmodel <- lm(tobinsq~tt+mi.w+ai.w+flev.w+fliq.w+fs.w ,data=csd) # it is mention to necesserly put sic codes but we have only one industry so we will not put this variable since it doesnt make sense
summary(Tmodel) # Tobinsq has to much correlation with Ros
plot(Tmodel) # check assumptions

Rmodel <- lm(ros~tt+mi.w+ai.w+flev.w+fliq.w+fs.w ,data=csd)
summary(Rmodel)
plot(Rmodel)

##Estimate the model.
#i. First discuss the quality of the regression models (e.g. F-value, R- square).
#ii. Interpret the results of each variable.
#iii. Compare the two models (one with tobin’s q and one with ROS as dependent
 #                            variable) and reflect on the differences.
#iv. Based on your regression results, give an answer to the question “What type
#of firms perform better?”
#v. Based on your regression results, would you recommend firms to invest in
#marketing?
  #vi. Based on your regression results, would you recommend firms to add an
#online sales channel/offer worldwide shipping /have an app/be present on tiktok?

##Reflect on the regression model. Can you think of ways to improve the model?
 
###Part 5

## Check data
length(unique(vi$gvkey))
table(vi$gvkey)

##make a models
Tmodelx <- lm(tobinsq~tt+mi.w+ai.w+flev.w+fliq.w+fs.w ,data=vi)
summary(Tmodelx)
plot(Tmodelx)

Rmodelx <- lm(ros~tt+mi.w+ai.w+flev.w+fliq.w+fs.w ,data=vi)
summary(Rmodelx)
plot(Rmodelx)

##Lead Variables
library(DataCombine)
install.packages("DataCombine", dependencies = TRUE)

vi <- slide(data = vi, Var = "tobinsq", TimeVar = "fyear", GroupVar = "gvkey",
                   NewVar = 'leadtobinsq', slideBy = +1, keepInvalid = TRUE, reminder = TRUE)
vi$leadtobinsq<-ifelse(is.na(vi$leadtobinsq),vi$tobinsq,vi$leadtobinsq)

vi <- slide(data = vi, Var = "ros", TimeVar = "fyear", GroupVar = "gvkey",
            NewVar = 'leadros', slideBy = +1, keepInvalid = TRUE, reminder = TRUE)
vi$leadros<-ifelse(is.na(vi$leadros),vi$ros,vi$leadros)

##Lag Variables
vi <- slide(data = vi, Var = "mi.w", TimeVar = "fyear", GroupVar = "gvkey",
            NewVar = 'lagmi', slideBy = -1, keepInvalid = TRUE, reminder = TRUE)
vi$lagmi<-ifelse(is.na(vi$lagmi),vi$mi.w,vi$lagmi)

vi <- slide(data = vi, Var = "ai.w", TimeVar = "fyear", GroupVar = "gvkey",
            NewVar = 'lagai', slideBy = -1, keepInvalid = TRUE, reminder = TRUE)
vi$lagai<-ifelse(is.na(vi$lagai),vi$ai.w,vi$lagai)


##Make new models
Tmodely <- lm(leadtobinsq~tt+lagmi+lagai+flev.w+fliq.w+fs.w ,data=vi)
summary(Tmodely)
plot(Tmodely)

Rmodely <- lm(leadros~tt+lagmi+lagai+flev.w+fliq.w+fs.w ,data=vi)
summary(Rmodely)
plot(Rmodely)