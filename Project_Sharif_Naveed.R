##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Title: Forecasting Market Share for Kaiser's National Account Book of Business
##### Class: Research Methods, Spring 2018
##### Professor: Wesley Blundell

### load libraries
library(tidyverse)
library(lubridate)
library(forcats)
library(zoo)
library(ggplot2)
library(colorspace)
library(grid)
library(data.table)
library(corrplot)
library(plm)
library(Formula)
library(Ecdat)
library(forecast)
library(stargazer)
library(lmtest)
library(Hmisc)

### load dataset
NationalAccount_Maindata <- read.csv("C:\\Users\\Naveed\\Desktop\\Education\\CSUEB\\MS_ECON\\MS_ECON_Spring_2018\\ECON 6896 Research Methods\\Project\\KP_NationalAccount_Forecast.csv", stringsAsFactors = TRUE)
attach(NationalAccount_Maindata)
options(scipen=999)
str(NationalAccount_Maindata, list.len=200)
colnames(NationalAccount_Maindata)

### define the dependent variable
NationalAccount_Maindata$Demand <- ((log(PenRate)-(1-log(PenRate))))

### create AR(1)
NationalAccount_Maindata <- data.frame(NationalAccount_Maindata %>% 
                                         group_by(RegionAccountNumber) %>% 
                                         mutate(Demand_lag1 = lag(Demand)))

### set the data as panel data
NationalAccount_Maindata <- pdata.frame(NationalAccount_Maindata, index = c("RegionAccountNumber","EffectiveDateYear"))

# summary of different regression models used
### AR(0) Pooled OLS
AR_0_Pooled <- plm(Demand~
                   +Trend
                   +IndustryDescription_RetailTrade
                   +IndustryDescription_HealthCareandSocialAssistance
                   +L_MembAveAge
                   +FCR
                   +propOwnedHomes
                   +propBlackHisp
                   +propCollEducated
                   +NumOfCarriers
                   +KPNumOfUniqueProducts
                   +CompoffersHSAorHRA
                   +CompRateMean
                   ,data = NationalAccount_Maindata, model = "pooling")

summary(AR_0_Pooled)

AR_0_Pooled_Fitted <- data.frame(fitted = AR_0_Pooled$model[[1]] - AR_0_Pooled$residuals)
head(AR_0_Pooled_Fitted,10)

z_adj <- select(NationalAccount_Maindata,RegionAccountNumber,EffectiveDateYear
                ,Demand
                ,Trend
                ,IndustryDescription_RetailTrade
                ,IndustryDescription_HealthCareandSocialAssistance
                ,L_MembAveAge
                ,FCR
                ,propOwnedHomes
                ,propBlackHisp
                ,propCollEducated
                ,NumOfCarriers
                ,KPNumOfUniqueProducts
                ,CompoffersHSAorHRA
                ,CompRateMean)

RMSFE <- filter(z_adj, !is.na(CompRateMean) & !is.na(Demand)) %>%
  cbind(.,AR_0_Pooled_Fitted$fitted) %>%
  mutate(xxx = (Demand - AR_0_Pooled_Fitted$fitted)^2)

options(digits=5)
(mean(RMSFE$xxx)^0.5)
(mean(RMSFE$xxx)^0.5)/abs(mean(AR_0_Pooled_Fitted$fitted))

### AR(0) Random Effects
AR_0_Random <- plm(Demand~
                   +Trend
                   +IndustryDescription_RetailTrade
                   +IndustryDescription_HealthCareandSocialAssistance
                   +L_MembAveAge
                   +FCR
                   +propOwnedHomes
                   +propBlackHisp
                   +propCollEducated
                   +NumOfCarriers
                   +KPNumOfUniqueProducts
                   +CompoffersHSAorHRA
                   +CompRateMean
                   ,data = NationalAccount_Maindata, model = "random")

summary(AR_0_Random)

AR_0_Random_Fitted <- data.frame(fitted = AR_0_Random$model[[1]] - AR_0_Random$residuals)
head(AR_0_Random_Fitted,10)

z_adj <- select(NationalAccount_Maindata,RegionAccountNumber,EffectiveDateYear
                ,Demand
                ,Trend
                ,IndustryDescription_RetailTrade
                ,IndustryDescription_HealthCareandSocialAssistance
                ,L_MembAveAge
                ,FCR
                ,propOwnedHomes
                ,propBlackHisp
                ,propCollEducated
                ,NumOfCarriers
                ,KPNumOfUniqueProducts
                ,CompoffersHSAorHRA
                ,CompRateMean)

RMSFE <- filter(z_adj, !is.na(CompRateMean) & !is.na(Demand)) %>%
  cbind(.,AR_0_Random_Fitted$fitted) %>%
  mutate(xxx = (Demand - AR_0_Random_Fitted$fitted)^2)

options(digits=5)
(mean(RMSFE$xxx)^0.5)
(mean(RMSFE$xxx)^0.5)/abs(mean(AR_0_Random_Fitted$fitted))

### AR(1) Random Effects
AR_1_Random <- plm(Demand~
                   +Demand_lag1 # p(1)
                   +Trend
                   +IndustryDescription_RetailTrade
                   +IndustryDescription_HealthCareandSocialAssistance
                   +L_MembAveAge
                   +FCR
                   +propOwnedHomes
                   +propBlackHisp
                   +propCollEducated
                   +NumOfCarriers
                   +KPNumOfUniqueProducts
                   +CompoffersHSAorHRA
                   +CompRateMean
                   ,data = NationalAccount_Maindata, model = "random")

summary(AR_1_Random)

AR_1_Random_Fitted <- data.frame(fitted = AR_1_Random$model[[1]] - AR_1_Random$residuals)
head(AR_1_Random_Fitted,10)

z_adj <- select(NationalAccount_Maindata,RegionAccountNumber,EffectiveDateYear
                ,Demand
                ,Demand_lag1
                ,Trend
                ,IndustryDescription_RetailTrade
                ,IndustryDescription_HealthCareandSocialAssistance
                ,L_MembAveAge
                ,FCR
                ,propOwnedHomes
                ,propBlackHisp
                ,propCollEducated
                ,NumOfCarriers
                ,KPNumOfUniqueProducts
                ,CompoffersHSAorHRA
                ,CompRateMean)

RMSFE <- filter(z_adj, !is.na(CompRateMean) & !is.na(Demand_lag1)) %>%
  cbind(.,AR_1_Random_Fitted$fitted) %>%
  mutate(xxx = (Demand - AR_1_Random_Fitted$fitted)^2)

options(digits=5)
(mean(RMSFE$xxx)^0.5)
(mean(RMSFE$xxx)^0.5)/abs(mean(AR_1_Random_Fitted$fitted))

### ADL(1,1) Random Effects
ADL_11_Random <- plm(Demand~
                     +Demand_lag1 # p(1)
                     +Trend
                     +IndustryDescription_RetailTrade
                     +IndustryDescription_HealthCareandSocialAssistance
                     +L_MembAveAge
                     +FCR
                     +propOwnedHomes
                     +propBlackHisp
                     +propCollEducated
                     +NumOfCarriers
                     +KPNumOfUniqueProducts
                     +CompoffersHSAorHRA
                     +CompRateMean
                     +CompRateMean_lag1 # q(1)
                     ,data = NationalAccount_Maindata, model = "random")

summary(ADL_11_Random)

ADL_11_Random_Fitted <- data.frame(fitted = ADL_11_Random$model[[1]] - ADL_11_Random$residuals)
head(ADL_11_Random_Fitted,10)

z_adj <- select(NationalAccount_Maindata,RegionAccountNumber,EffectiveDateYear
                ,Demand
                ,Demand_lag1
                ,Trend
                ,IndustryDescription_RetailTrade
                ,IndustryDescription_HealthCareandSocialAssistance
                ,L_MembAveAge
                ,FCR
                ,propOwnedHomes
                ,propBlackHisp
                ,propCollEducated
                ,NumOfCarriers
                ,KPNumOfUniqueProducts
                ,CompoffersHSAorHRA
                ,CompRateMean
                ,CompRateMean_lag1)

RMSFE <- filter(z_adj, !is.na(CompRateMean_lag1) & !is.na(Demand_lag1)) %>%
  cbind(.,ADL_11_Random_Fitted$fitted) %>%
  mutate(xxx = (Demand - ADL_11_Random_Fitted$fitted)^2)

options(digits=5)
(mean(RMSFE$xxx)^0.5)
(mean(RMSFE$xxx)^0.5)/abs(mean(ADL_11_Random_Fitted$fitted))

### EDA
# time series plot of mean demand 
filter(NationalAccount_Maindata, EffectiveDateYear == 2012) %>%
  summarize(mean=mean(Demand), sd=sd(Demand))

filter(NationalAccount_Maindata, EffectiveDateYear == 2013) %>%
  summarize(mean=mean(Demand), sd=sd(Demand))

filter(NationalAccount_Maindata, EffectiveDateYear == 2014) %>%
  summarize(mean=mean(Demand), sd=sd(Demand))

filter(NationalAccount_Maindata, EffectiveDateYear == 2015) %>%
  summarize(mean=mean(Demand), sd=sd(Demand))

filter(NationalAccount_Maindata, EffectiveDateYear == 2016) %>%
  summarize(mean=mean(Demand), sd=sd(Demand))

filter(NationalAccount_Maindata, EffectiveDateYear == 2017) %>%
  summarize(mean=mean(Demand), sd=sd(Demand))

filter(NationalAccount_Maindata, EffectiveDateYear == 2018) %>%
  summarize(mean=mean(Demand), sd=sd(Demand))

Year <- c(2012:2018)
Demand_Mean <- c(-4.1028,-4.1175,-4.1702,-4.1254,-4.3306,-4.3908,-4.4867)
Demand_Stdv <- c(1.7341,1.7351,1.7624,1.694,1.7215,1.7759,1.7786)
d <- data.frame(Year,Demand_Mean,Demand_Stdv)

plot(d$Year, d$Demand_Mean, type="n")
with (
  data = d
  , expr = errbar(Year, Demand_Mean, Demand_Mean+Demand_Stdv, Demand_Mean-Demand_Stdv, add=F, pch=1, cap=.015, log="x")
)
lines(Year,Demand_Mean)

plot(cbind(Year,Demand_Mean),main="Change in Demand Over Time")
lines(Year,Demand_Mean)

# summary statistics of variables used
NationalAccount_Maindata_Stargazer <- select(NationalAccount_Maindata
                                             ,Demand
                                             ,IndustryDescription_RetailTrade
                                             ,IndustryDescription_HealthCareandSocialAssistance
                                             ,L_MembAveAge
                                             ,FCR
                                             ,propOwnedHomes
                                             ,propBlackHisp
                                             ,propCollEducated
                                             ,NumOfCarriers
                                             ,KPNumOfUniqueProducts
                                             ,CompoffersHSAorHRA
                                             ,CompRateMean
                                             ,KPRateMean)

stargazer(NationalAccount_Maindata_Stargazer
          ,type = "text"
          ,title = "Table 1: Summary Statistics"
          ,out = "table1.txt"
          ,digits = 2)

# summary of Regressions Results used
stargazer(AR_0_Pooled
          ,AR_0_Random
          ,AR_1_Random
          ,ADL_11_Random
          ,type = "text"
          ,dep.var.labels = c("Demand for Kaiser Permanente")
          ,title = "Table 2: Regression Results"
          ,out = "model.txt"
          ,digits = 2
          ,column.labels = c("AR(0) Pooled"
                             ,"AR(O) RE"
                             ,"AR(1) RE"
                             ,"ADL(1,1) RE")
          ,covariate.labels = c("Demand Lag1"
                                ,"Trend"
                                ,"Industry: Retail"
                                ,"Industry: Health Care"
                                ,"Log(Member Average Age)"
                                ,"Family Content Ratio"
                                ,"Proportion Owned Homes"
                                ,"Proportion Black/Hispanic"
                                ,"Proportion Colledge Educ"
                                ,"# of Carriers"
                                ,"# of KP Unique Products"
                                ,"Competitor Offers HSA/HRA"
                                ,"Avg Competitor Rate"
                                ,"Avg Competitor Rate Lag1")
          ,notes = c("RMSFE: AR(0) Pooled = 1.61 | AR(0) RE = .55 | AR(1) RE = .44 | ADL(1,1) RE = .37")
          ,notes.align = c("l"))

### forecast marketshare for 2019 based on the following counterfactuals
### (1) add 2 new KP products, where one of them is an HSA/HRA (HDHP) product
### (2) multiply competitor offers HSA/HRA by 0, so that we are at parity. That way competitor does not have an advantage
### (3) assume Competitor mean rate is $100 more expensive 
# get coefficients from the ADL_11_Random
coefs <- ADL_11_Random$coefficients

# get the fitted values
Forecast_1 <- filter(z_adj, !is.na(CompRateMean_lag1) & !is.na(Demand_lag1)) %>%
  cbind(.,ADL_11_Random_Fitted$fitted)

# replace demand lag1 by current year demand
Forecast_1$Demand_lag1 <- Forecast_1$Demand

# replace Compratemean lag1 by current year Compratemean
Forecast_1$CompRateMean_lag1 <- Forecast_1$CompRateMean

# apply counterfactuals (1), (2), and (3)
Forecast_1$fitted_counterfactual <- coefs[1]+coefs[2]*Forecast_1[,4]+coefs[3]*Forecast_1[,5]+coefs[4]*Forecast_1[,6]+coefs[5]*Forecast_1[,7]+coefs[6]*Forecast_1[,8]+coefs[7]*Forecast_1[,9]+coefs[8]*Forecast_1[,10]+coefs[9]*Forecast_1[,11]+coefs[10]*Forecast_1[,12]+coefs[11]*Forecast_1[,13]+coefs[12]*(Forecast_1[,14]+2)+coefs[13]*(Forecast_1[,15]*0)+coefs[14]*(Forecast_1[,16]+100)+coefs[15]*Forecast_1[,17]

# calculate market shares, for each year (including counterfactual) and plot the trend
# I calculated the fitted values market share with the counterfactual 
# versus the actual market share with the counterfactual
MarketShare_2013 <- filter(Forecast_1, EffectiveDateYear == 2013) 
MarketShare_2013 <- exp(MarketShare_2013$`ADL_11_Random_Fitted$fitted`)/(1+exp(MarketShare_2013$`ADL_11_Random_Fitted$fitted`))
mean(MarketShare_2013)

MarketShare_2014 <- filter(Forecast_1, EffectiveDateYear == 2014) 
MarketShare_2014 <- exp(MarketShare_2014$`ADL_11_Random_Fitted$fitted`)/(1+exp(MarketShare_2014$`ADL_11_Random_Fitted$fitted`))
mean(MarketShare_2014)

MarketShare_2015 <- filter(Forecast_1, EffectiveDateYear == 2015) 
MarketShare_2015 <- exp(MarketShare_2015$`ADL_11_Random_Fitted$fitted`)/(1+exp(MarketShare_2015$`ADL_11_Random_Fitted$fitted`))
mean(MarketShare_2015)

MarketShare_2016 <- filter(Forecast_1, EffectiveDateYear == 2016) 
MarketShare_2016 <- exp(MarketShare_2016$`ADL_11_Random_Fitted$fitted`)/(1+exp(MarketShare_2016$`ADL_11_Random_Fitted$fitted`))
mean(MarketShare_2016)

MarketShare_2017 <- filter(Forecast_1, EffectiveDateYear == 2017) 
MarketShare_2017 <- exp(MarketShare_2017$`ADL_11_Random_Fitted$fitted`)/(1+exp(MarketShare_2017$`ADL_11_Random_Fitted$fitted`))
mean(MarketShare_2017)

MarketShare_2018 <- filter(Forecast_1, EffectiveDateYear == 2018) 
MarketShare_2018 <- exp(MarketShare_2018$`ADL_11_Random_Fitted$fitted`)/(1+exp(MarketShare_2018$`ADL_11_Random_Fitted$fitted`))
mean(MarketShare_2018)

MarketShare_F2019 <- filter(Forecast_1, EffectiveDateYear == 2018) 
MarketShare_F2019 <- exp(Forecast_1$fitted_counterfactual)/(1+exp(Forecast_1$fitted_counterfactual))
mean(MarketShare_F2019)

# plot with the counterfactual
Year <- c(2013:2019)
KP_MarketShare <- c(.034913,.034867,.034264,.031483,.030796,.031156,.035642)
d <- data.frame(Year,KP_MarketShare)

plot(cbind(Year,KP_MarketShare),main="Change in KP MarketShare Over Time")
lines(Year,KP_MarketShare)

# evaluating fixed vs pooled vs random
### run models (FE,RE,Pooled)
random_model <- plm(Demand~
                      +Demand_lag1 # p(1)
                    +Trend
                    +IndustryDescription_RetailTrade
                    +IndustryDescription_HealthCareandSocialAssistance
                    +L_MembAveAge
                    +FCR
                    +propOwnedHomes
                    +propBlackHisp
                    +propCollEducated
                    +NumOfCarriers
                    +KPNumOfUniqueProducts
                    +CompoffersHSAorHRA
                    +CompRateMean
                    +CompRateMean_lag1 # q(1)
                    ,data = NationalAccount_Maindata, model = "random")

# will be using the RE model
# based on the assumptions of RE/FE, and tests, below, on poolability
summary(random_model)

# Calculate and evaluate RMFSE (the lower the better) 
random_fitted <- data.frame(fitted = random_model$model[[1]] - random_model$residuals)
head(random_fitted,10)

z_adj <- select(NationalAccount_Maindata,RegionAccountNumber,EffectiveDateYear
                ,Demand
                ,Demand_lag1
                ,Trend
                ,IndustryDescription_RetailTrade
                ,IndustryDescription_HealthCareandSocialAssistance
                ,L_MembAveAge
                ,FCR
                ,propOwnedHomes
                ,propBlackHisp
                ,propCollEducated
                ,NumOfCarriers
                ,KPNumOfUniqueProducts
                ,CompoffersHSAorHRA
                ,CompRateMean
                ,CompRateMean_lag1)

RMSFE <- filter(z_adj, !is.na(CompRateMean_lag1) & !is.na(Demand_lag1)) %>%
  cbind(.,random_fitted$fitted) %>%
  mutate(xxx = (Demand - random_fitted$fitted)^2)

options(digits=5)
(mean(RMSFE$xxx)^0.5)
(mean(RMSFE$xxx)^0.5)/abs(mean(random_fitted$fitted))

### FE
within_model <- plm(Demand~
                      +Demand_lag1 # p(1)
                    +Trend
                    +IndustryDescription_RetailTrade
                    +IndustryDescription_HealthCareandSocialAssistance
                    +L_MembAveAge
                    +FCR
                    +propOwnedHomes
                    +propBlackHisp
                    +propCollEducated
                    +NumOfCarriers
                    +KPNumOfUniqueProducts
                    +CompoffersHSAorHRA
                    +CompRateMean
                    +CompRateMean_lag1 # q(1)
                    ,data = NationalAccount_Maindata, model = "within", effect = "twoways")

summary(within_model)

### Pooled
pooled_model <- plm(Demand~
                      +Demand_lag1 # p(1)
                    +Trend
                    +IndustryDescription_RetailTrade
                    +IndustryDescription_HealthCareandSocialAssistance
                    +L_MembAveAge
                    +FCR
                    +propOwnedHomes
                    +propBlackHisp
                    +propCollEducated
                    +NumOfCarriers
                    +KPNumOfUniqueProducts
                    +CompoffersHSAorHRA
                    +CompRateMean
                    +CompRateMean_lag1 # q(1)
                    ,data = NationalAccount_Maindata, model = "pooling")

summary(pooled_model)

### hausman test
### if pvalue is less than .05 than reject the null hypothesis for fixed effect
phtest(within_model,random_model)

### pooling test
### if pvalue is less than .05 then reject the null hypothesis for pooling
pooltest(pooled_model,within_model)

### Durbin Watson test
### test of serial correlation for (the idiosyncratic component of) the errors in panel models
### model has serial correlation. Therefore will need to adust SE
pwtest(pooled_model)

### heteroskedasticity auto correlation
### adjusting standard errors, which by default computes SEs clustered by group
coeftest(random_model,vcov=function(x) vcovHC(x,method="white1",type="HC1"))

###  plot the relationship between fitted and actuals. evaluate the correlation between fitted and actural 
ll <- filter(z_adj, !is.na(CompRateMean_lag1) & !is.na(Demand_lag1)) %>%
  cbind(.,random_fitted$fitted) 

ggplot(data = ll) +
  geom_point(mapping = aes(`random_fitted$fitted`,Demand)) +
  geom_smooth(mapping = aes(`random_fitted$fitted`,Demand)) +
  scale_x_continuous(limits = c(-10.0,0)) +
  scale_y_continuous(limits = c(-10.0,0)) +
  ggtitle("Relationship Between Fit and Actuals") +
  labs(x="Adds Fitted",y="Adds Actuals")

cov(ll$`random_fitted$fitted`,ll$Demand)/(sd(ll$`random_fitted$fitted`)*sd(ll$Demand))

