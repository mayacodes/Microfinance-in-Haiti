#---------------------------------------------
#         CODE 4: Creating results tables
#---------------------------------------------
# title: "Age moderation of the effect of microfinance on IPV, relationship power, and transactional sex"
# author: "Maya Luetke"
# date: "April 1, 2019"
# last update: "December 14, 2019"
#---------------------------------------------


#---------------------------------------------
#         Clean environment & double-check latest R versions
#---------------------------------------------
#rm(list=ls())
#R.Version()
#---------------------------------------------


#---------------------------------------------
#         Set working directory
#---------------------------------------------
getwd()
setwd('C:\\users\\...\\Data') #For school computer
#setwd("/Users/.../Data") #For laptop computer
#setwd("/Users/.../Data") #For laptop computer
getwd()
#---------------------------------------------


#---------------------------------------------
#         Import data
#---------------------------------------------
mydata <- read.csv(file="data1.2.20.csv", sep=",")
#---------------------------------------------


#---------------------------------------------
#         Install/load packages
#---------------------------------------------
#Load packages & libraries
#install.packages('xtable')
#install.packages('lavaan')
#install.packages('semPlot')
#---------------------------------------------


#---------------------------------------------
#         Look at data 
#---------------------------------------------
str(mydata)
names(mydata)
#---------------------------------------------


#---------------------------------------------
#         REcode trans
#---------------------------------------------
#table(mydata$trans, exclude=FALSE)
#mydata$trans[mydata$sex_partner>0 & mydata$trans==1] <- 1
#mydata$trans[mydata$sex_partner>0 & mydata$trans==0] <- 0                
#table(mydata$trans, exclude=FALSE)
#---------------------------------------------


#---------------------------------------------
#     Check coding of transactional sex -- it's good!
#---------------------------------------------
#transactional sex: money_sex + grocery_clothes_sex + gifts_money_sex;
table(mydata$trans3)
mydata$trans3[(mydata$money_sex==1 | mydata$grocery_clothes_sex==1) & mydata$sex_partner>0 & mydata$gifts_money_sex==1] <- 1
table(mydata$trans3)
mydata$trans3[(mydata$money_sex==1 | mydata$grocery_clothes_sex==1) & mydata$sex_partner>0 & mydata$gifts_money_sex==2] <- 0
table(mydata$trans3)
mydata$trans3[mydata$money_sex==2 & mydata$grocery_clothes_sex==2 & mydata$sex_partner>0] <- 0
table(mydata$trans3, exclude=FALSE)
mydata$trans3[mydata$sex_partner==0] <- NA
table(mydata$trans3, exclude=FALSE)
mydata$trans3[((mydata$money_sex==98|mydata$money_sex==99) | (mydata$grocery_clothes_sex==98|mydata$grocery_clothes_sex==99)) & (mydata$gifts_money_sex==98|mydata$gifts_money_sex==99)] <- NA
table(mydata$trans3, exclude=FALSE)
#mydata$trans3[mydata$sex_partner>0 & (mydata$money_sex!=1 & mydata$grocery_clothes_sex!=1) & (mydata$gifts_money_sex!=1)] <- 98
#---------------------------------------------


#---------------------------------------------
#         Check some variables before modeling
#---------------------------------------------
#install.packages("gmodels")
library(gmodels)
CrossTable(mydata$rel_powercat,mydata$agecat)
chisq.test(mydata$rel_powercat,mydata$agecat)

table(mydata$dur12m)
t.test(mydata$dur12m, mydata$age)
hist(mydata$age)
library(psych)
describe(mydata$age)
table(mydata$dur12m)
table(mydata$agecat)
#---------------------------------------------


#---------------------------------------------
#
#            UNADJUSTED MODELS
#
#---------------------------------------------

#---------------------------------------------
#
#         Moderation 1: IPV in last 12 months
#
#---------------------------------------------
table(mydata$ipv12)
#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$dur12m*mydata$agecat
mod.1 <- logbin(ipv12 ~ dur12m + agecat + int, data=mydata)
summary(mod.1)
exp(mod.1$coefficients)
exp(confint(mod.1))

mod.tot <- logbin(ipv12 ~ dur12m, data=mydata)
summary(mod.tot)
mod.tot.output <- cbind(round(exp(mod.tot$coefficients),2), round(exp(confint(mod.tot)),2), round(summary(mod.tot)$coefficients[,4],2))
colnames(mod.tot.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.tot.output) <-c("Intercept","Total")
mod.tot.output

#Make data subsets in order to run later analyses
agecat_1 <- subset(mydata, agecat==1)
agecat_0 <- subset(mydata, agecat==0)

#Among OLDER (agecat=1)
mod.agecat_1 <- logbin(ipv12 ~ dur12m, data=agecat_1) 
mod.agecat_1.output <- cbind(round(exp(mod.agecat_1$coefficients),2), round(exp(confint(mod.agecat_1)),2), round(summary(mod.agecat_1)$coefficients[,4],2))
colnames(mod.agecat_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.agecat_1.output) <-c("Intercept","Agecat=1")
mod.agecat_1.output
#Among YOUNGER (agecat=0)
mod.agecat_0 <- logbin(ipv12 ~ dur12m, data=agecat_0) 
mod.agecat_0.output <- cbind(round(exp(mod.agecat_0$coefficients),2), round(exp(confint(mod.agecat_0)),2), round(summary(mod.agecat_0)$coefficients[,4],2))
colnames(mod.agecat_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.agecat_0.output) <-c("Intercept","Agecat=0")
mod.agecat_0.output
#print results
ipv.mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],mod.agecat_1.output[2,])
row.names(ipv.mod.byage) <-c("total", "agecat=0", "agecat=1") 
ipv.mod.byage
#---------------------------------------------


#---------------------------------------------
#
#         Moderation 2: Relationship power
#
#---------------------------------------------
table(mydata$rel_powercat)
hist(mydata$rel_power)
#install.packages("psych")
library(psych)
describe(mydata$rel_power)

mydata$rel_powercat2[mydata$rel_power>=0 & mydata$rel_power<25] <- 1 #LOW
mydata$rel_powercat2[mydata$rel_power>=25 & mydata$rel_power<=36] <- 0 #HIGH
table(mydata$rel_powercat2,exclude = FALSE)

rel_power_data <- subset(mydata, sex_partner>0)
#Test to see if there is significant moderation--Wald p-value for interation (int)
rel_power_data$int<-rel_power_data$dur12m*rel_power_data$agecat
mod.1 <- logbin(rel_powercat2 ~ dur12m + agecat + int, data=rel_power_data)
summary(mod.1)
exp(mod.1$coefficients)
exp(confint(mod.1))

mod.tot <- logbin(rel_powercat2 ~ dur12m, data=rel_power_data)
summary(mod.tot)
mod.tot.output <- cbind(round(exp(mod.tot$coefficients),2), round(exp(confint(mod.tot)),2), round(summary(mod.tot)$coefficients[,4],2))
colnames(mod.tot.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.tot.output) <-c("Intercept","Total")
mod.tot.output
#Make data subsets in order to run later analyses
relp_agecat_1 <- subset(rel_power_data, agecat==1)
relp_agecat_0 <- subset(rel_power_data, agecat==0)

#Among OLDER (agecat=1)
mod.agecat_1 <- logbin(rel_powercat2 ~ dur12m, data=relp_agecat_1) 
mod.agecat_1.output <- cbind(round(exp(mod.agecat_1$coefficients),2), round(exp(confint(mod.agecat_1)),2), round(summary(mod.agecat_1)$coefficients[,4],2))
colnames(mod.agecat_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.agecat_1.output) <-c("Intercept","Agecat=1")
mod.agecat_1.output
#Among YOUNGER (agecat=0)
mod.agecat_0 <- logbin(rel_powercat2 ~ dur12m, data=relp_agecat_0) 
mod.agecat_0.output <- cbind(round(exp(mod.agecat_0$coefficients),2), round(exp(confint(mod.agecat_0)),2), round(summary(mod.agecat_0)$coefficients[,4],2))
colnames(mod.agecat_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.agecat_0.output) <-c("Intercept","Agecat=0")
mod.agecat_0.output
#print results
relp.mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],mod.agecat_1.output[2,])
row.names(relp.mod.byage) <-c("total", "agecat=0", "agecat=1") 
relp.mod.byage
#---------------------------------------------


#---------------------------------------------
#
#         Moderation 3: Transactional sex
#
#---------------------------------------------
table(mydata$trans, exclude=FALSE)
160+112
#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$dur12m*mydata$agecat
mod.1 <- logbin(trans ~ dur12m + agecat + int, data=mydata)
summary(mod.1)
exp(mod.1$coefficients)
exp(confint(mod.1))

mod.tot <- logbin(trans ~ dur12m, data=mydata)
summary(mod.tot)
mod.tot.output <- cbind(round(exp(mod.tot$coefficients),2), round(exp(confint(mod.tot)),2), round(summary(mod.tot)$coefficients[,4],2))
colnames(mod.tot.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.tot.output) <-c("Intercept","Total")
mod.tot.output
#Make data subsets in order to run later analyses
agecat_1 <- subset(mydata, agecat==1)
agecat_0 <- subset(mydata, agecat==0)

#Among OLDER (agecat=1)
mod.agecat_1 <- logbin(trans ~ dur12m, data=agecat_1) 
mod.agecat_1.output <- cbind(round(exp(mod.agecat_1$coefficients),2), round(exp(confint(mod.agecat_1)),2), round(summary(mod.agecat_1)$coefficients[,4],2))
colnames(mod.agecat_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.agecat_1.output) <-c("Intercept","Agecat=1")
mod.agecat_1.output
#Among YOUNGER (agecat=0)
mod.agecat_0 <- logbin(trans ~ dur12m, data=agecat_0) 
mod.agecat_0.output <- cbind(round(exp(mod.agecat_0$coefficients),2), round(exp(confint(mod.agecat_0)),2), round(summary(mod.agecat_0)$coefficients[,4],2))
colnames(mod.agecat_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
row.names(mod.agecat_0.output) <-c("Intercept","Agecat=0")
mod.agecat_0.output
#print results
trans.mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],mod.agecat_1.output[2,])
row.names(trans.mod.byage) <-c("total", "agecat=0", "agecat=1") 
trans.mod.byage
#---------------------------------------------


#---------------------------------------------
#
#            ADJUSTED MODELS
#
#---------------------------------------------
table(mydata$marital3)
table(mydata$educ_level3) 
hist(mydata$assets)
mydata$log_assets <- log(mydata$assets)
hist(mydata$log_assets)
table(mydata$assets_Q)

mydata$assets01[mydata$assets_Q==1 | mydata$assets_Q==2] <- 0 #LOW
mydata$assets01[mydata$assets_Q==3 | mydata$assets_Q==4] <- 1 #HIGH
table(mydata$assets01,exclude=FALSE)
table(mydata$assetsq,exclude=FALSE)

#---------------------------------------------
#
#         Moderation 1: IPV in last 12 months
#
#---------------------------------------------
table(mydata$ipv12)

#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$dur12m*mydata$agecat

m1 <- glm(formula = ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3 + agecat + int, family = "poisson", data=mydata)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
r.est.r <- round(r.est,2)
r.est.r


m1 <- glm(formula = ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=mydata)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.tot.output <- round(r.est,2)
mod.tot.output

#Make data subsets in order to run later analyses
agecat_1 <- subset(mydata, agecat==1)
agecat_0 <- subset(mydata, agecat==0)

#Among OLDER (agecat=1)
#mod.agecat_1 <- logbin(ipv12 ~ dur12m + assets + marital3 + educ_level3, data=agecat_1, start=0) 
#mod.agecat_1.output <- cbind(round(exp(mod.agecat_1$coefficients),2), round(exp(confint(mod.agecat_1)),2), round(summary(mod.agecat_1)$coefficients[,4],6))
#colnames(mod.agecat_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
#mod.agecat_1.output

m1 <- glm(formula = ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=agecat_1)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.agecat_1.output <- round(r.est,2)
mod.agecat_1.output
#Among YOUNGER (agecat=0)
m1 <- glm(formula = ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=agecat_0)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.agecat_0.output <- round(r.est,2)
mod.agecat_0.output
#print results
a.ipv.mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],mod.agecat_1.output[2,])
row.names(a.ipv.mod.byage) <-c("total", "agecat=0", "agecat=1")
colnames(a.ipv.mod.byage) <- c("PR", "Lower CI", "Upper CI", "P-value")
a.ipv.mod.byage
#---------------------------------------------


#---------------------------------------------
#
#         Moderation 2: Relationship power
#
#---------------------------------------------
table(mydata$rel_powercat)
hist(mydata$rel_power)
describe(mydata$rel_power)

mydata$rel_powercat2[is.na(mydata$rel_power)] <- NA
mydata$rel_powercat2[mydata$rel_power>=0 & mydata$rel_power<25] <- 1 #LOW
mydata$rel_powercat2[mydata$rel_power>=25 & mydata$rel_power<=36] <- 0 #HIGH
table(mydata$rel_powercat2,exclude = FALSE)

rel_power_data <- subset(mydata, sex_partner>0)
#Test to see if there is significant moderation--Wald p-value for interation (int)
rel_power_data$int<-rel_power_data$dur12m*rel_power_data$agecat
#mod.1 <- logbin(rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3 + agecat + int, data=rel_power_data)
#summary(mod.1)
#exp(mod.1$coefficients)
#exp(confint(mod.1))

m1 <- glm(formula = rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3 + agecat + int, family = "poisson", data=rel_power_data)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
r.est.r <- round(r.est,2)
r.est.r

m1 <- glm(formula = rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=rel_power_data)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.tot.output <- round(r.est,2)
mod.tot.output

#Make data subsets in order to run later analyses
relp_agecat_1 <- subset(rel_power_data, agecat==1)
relp_agecat_0 <- subset(rel_power_data, agecat==0)

#Among OLDER (agecat=1)
m1 <- glm(formula = rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=relp_agecat_1)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.agecat_1.output <- round(r.est,2)
mod.agecat_1.output

#Among YOUNGER (agecat=0)
m1 <- glm(formula = rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data = relp_agecat_0)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.agecat_0.output <- round(r.est,2)
mod.agecat_0.output

#print results
a.relp.mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],mod.agecat_1.output[2,])
row.names(a.relp.mod.byage) <-c("total", "agecat=0", "agecat=1")
colnames(a.relp.mod.byage) <- c("PR", "Lower CI", "Upper CI", "P-value")
a.relp.mod.byage
#---------------------------------------------


#---------------------------------------------
#
#         Moderation 3: Transactional sex
#
#---------------------------------------------
table(mydata$ipv12)

#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$dur12m*mydata$agecat

m1 <- glm(formula = trans ~ dur12m + factor(assets_Q) + marital3 + educ_level3 + agecat + int, family = "poisson", data=mydata)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
r.est.r <- round(r.est,2)
r.est.r


m1 <- glm(formula = trans ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=mydata)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.tot.output <- round(r.est,2)
mod.tot.output

#Make data subsets in order to run later analyses
agecat_1 <- subset(mydata, agecat==1)
agecat_0 <- subset(mydata, agecat==0)

#Among OLDER (agecat=1)
#mod.agecat_1 <- logbin(ipv12 ~ dur12m + assets + marital3 + educ_level3, data=agecat_1, start=0) 
#mod.agecat_1.output <- cbind(round(exp(mod.agecat_1$coefficients),2), round(exp(confint(mod.agecat_1)),2), round(summary(mod.agecat_1)$coefficients[,4],6))
#colnames(mod.agecat_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
#mod.agecat_1.output

m1 <- glm(formula = trans ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=agecat_1)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.agecat_1.output <- round(r.est,2)
mod.agecat_1.output
#Among YOUNGER (agecat=0)
m1 <- glm(formula = trans ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=agecat_0)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
mod.agecat_0.output <- round(r.est,2)
mod.agecat_0.output
#print results
a.trans.mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],mod.agecat_1.output[2,])
row.names(a.trans.mod.byage) <-c("total", "agecat=0", "agecat=1")
colnames(a.trans.mod.byage) <- c("PR", "Lower CI", "Upper CI", "P-value")
a.trans.mod.byage
#---------------------------------------------


#---------------------------------------------
#         Remove extra/not needed datasets
#---------------------------------------------
remove(agecat_1,agecat_0)
#---------------------------------------------


#---------------------------------------------
#         TABLE 2
#---------------------------------------------
#unadjusted
ipv.mod.byage
relp.mod.byage
trans.mod.byage
#adjusted
a.ipv.mod.byage
a.relp.mod.byage
a.trans.mod.byage
#---------------------------------------------




















































#---------------------------------------------
#
#            ADJUSTED MODELS
#
#---------------------------------------------
table(mydata$marital3)
table(mydata$educ_level3) 
hist(mydata$assets)
mydata$log_assets <- log(mydata$assets)
hist(mydata$log_assets)
table(mydata$assets_Q)

mydata$assets01[mydata$assets_Q==1 | mydata$assets_Q==2] <- 0 #LOW
mydata$assets01[mydata$assets_Q==3 | mydata$assets_Q==4] <- 1 #HIGH
table(mydata$assets01,exclude=FALSE)
table(mydata$assetsq,exclude=FALSE)

#---------------------------------------------
#
#         Moderation 1: IPV in last 12 months
#
#---------------------------------------------
table(mydata$ipv12)

#Test to see if there is significant moderation--Wald p-value for interation (int)
mydata$int<-mydata$dur12m*mydata$agecat
mod.1 <- logbin(ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3 + agecat + int, data=mydata)
summary(mod.1)
exp(mod.1$coefficients)
exp(confint(mod.1))

mod.tot <- logbin(ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, data=mydata)
summary(mod.tot)
mod.tot.output <- cbind(round(exp(mod.tot$coefficients),2), round(exp(confint(mod.tot)),2), round(summary(mod.tot)$coefficients[,4],6))
colnames(mod.tot.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
mod.tot.output

#Make data subsets in order to run later analyses
agecat_1 <- subset(mydata, agecat==1)
agecat_0 <- subset(mydata, agecat==0)

#Among OLDER (agecat=1)
#mod.agecat_1 <- logbin(ipv12 ~ dur12m + assets + marital3 + educ_level3, data=agecat_1, start=0) 
#mod.agecat_1.output <- cbind(round(exp(mod.agecat_1$coefficients),2), round(exp(confint(mod.agecat_1)),2), round(summary(mod.agecat_1)$coefficients[,4],6))
#colnames(mod.agecat_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
#mod.agecat_1.output

m1 <- glm(formula = ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data=agecat_1)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
r.est.r <- round(r.est,2)
r.est.r
#Among YOUNGER (agecat=0)
mod.agecat_0 <- logbin(ipv12 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, data=agecat_0, linear) 
mod.agecat_0.output <- cbind(round(exp(mod.agecat_0$coefficients),2), round(exp(confint(mod.agecat_0)),2), round(summary(mod.agecat_0)$coefficients[,4],6))
colnames(mod.agecat_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
mod.agecat_0.output
#print results
a.ipv.mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],r.est.r[2,])
row.names(a.ipv.mod.byage) <-c("total", "agecat=0", "agecat=1") 
a.ipv.mod.byage
#---------------------------------------------



#---------------------------------------------
#
#         Moderation 2: Relationship power
#
#---------------------------------------------
table(mydata$rel_powercat)
hist(mydata$rel_power)
describe(mydata$rel_power)

mydata$rel_powercat2[is.na(mydata$rel_power)] <- NA
mydata$rel_powercat2[mydata$rel_power>=0 & mydata$rel_power<25] <- 1 #LOW
mydata$rel_powercat2[mydata$rel_power>=25 & mydata$rel_power<=36] <- 0 #HIGH
table(mydata$rel_powercat2,exclude = FALSE)

rel_power_data <- subset(mydata, sex_partner>0)
#Test to see if there is significant moderation--Wald p-value for interation (int)
rel_power_data$int<-rel_power_data$dur12m*rel_power_data$agecat
#mod.1 <- logbin(rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3 + agecat + int, data=rel_power_data)
#summary(mod.1)
#exp(mod.1$coefficients)
#exp(confint(mod.1))

m1 <- glm(formula = rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3 + agecat + int, family = "poisson", data=rel_power_data)
#install.packages("sandwich")
library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(PR= exp(coef(m1)), #"Robust SE" = std.err
               "Lower CI" = exp(coef(m1) - 1.96 * std.err),
               "Upper CI" = exp(coef(m1) + 1.96 * std.err), "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE))
r.est.r <- round(r.est,2)
r.est.r

mod.tot <- logbin(rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, data=rel_power_data)
summary(mod.tot)
mod.tot.output <- cbind(round(exp(mod.tot$coefficients),2), round(exp(confint(mod.tot)),2), round(summary(mod.tot)$coefficients[,4],2))
colnames(mod.tot.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
mod.tot.output

#Make data subsets in order to run later analyses
relp.agecat_1 <- subset(rel_power_data, agecat==1)
relp.agecat_0 <- subset(rel_power_data, agecat==0)

#Among OLDER (agecat=1)
mod.agecat_1 <- logbin(rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, data=relp.agecat_1) 
mod.agecat_1.output <- cbind(round(exp(mod.agecat_1$coefficients),2), round(exp(confint(mod.agecat_1)),2), round(summary(mod.agecat_1)$coefficients[,4],6))
colnames(mod.agecat_1.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
mod.agecat_1.output

m2 <- glm(formula = rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data = relp.agecat_1)
#install.packages("sandwich")
library(sandwich)
cov.m2 <- vcovHC(m2, type="HC0")
std.err <- sqrt(diag(cov.m2))
r.est <- cbind(Estimate= exp(coef(m2)), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m2)/std.err), lower.tail=FALSE),
               LL = exp(coef(m2) - 1.96 * std.err),
               UL = exp(coef(m2) + 1.96 * std.err))
r.est
4.919075e-01

#Among YOUNGER (agecat=0)
#mod.agecat_0 <- logbin(rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, data=relp.agecat_0) 
#mod.agecat_0.output <- cbind(round(exp(mod.agecat_0$coefficients),2), round(exp(confint(mod.agecat_0)),2), round(summary(mod.agecat_0)$coefficients[,4],6))
#colnames(mod.agecat_0.output) <- c("PR", "Lower CI", "Upper CI", "P-value")
#row.names(mod.agecat_0.output) <-c("Intercept","Agecat=0")
#mod.agecat_0.output

m2 <- glm(formula = rel_powercat2 ~ dur12m + factor(assets_Q) + marital3 + educ_level3, family = "poisson", data = agecat_0)
#install.packages("sandwich")
library(sandwich)
cov.m2 <- vcovHC(m2, type="HC0")
std.err <- sqrt(diag(cov.m2))
r.est <- cbind(Estimate= exp(coef(m2)), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m2)/std.err), lower.tail=FALSE),
               LL = exp(coef(m2) - 1.96 * std.err),
               UL = exp(coef(m2) + 1.96 * std.err))
r.est
4.919075e-01

#print results
mod.byage <- rbind(mod.tot.output[2,],mod.agecat_0.output[2,],mod.agecat_1.output[2,])
row.names(mod.byage) <-c("total", "agecat=0", "agecat=1") 
mod.byage
#---------------------------------------------






#remove datasets
remove(agecat_1,agecat_0)



#---------------------------------------------
#         TABLE 2
#---------------------------------------------
#unadjusted
ipv.mod.byage
relp.mod.byage
trans.mod.byage
#adjusted
a.ipv.mod.byage
