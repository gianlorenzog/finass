graphics.off()
rm(list=ls())

setwd("/Users/gianlorenzo/Desktop/FINASS/Advanced Statistics for finance/Project/Data")
# install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(purrr)
library(tidyverse)
library(data.table)
library(car)
library(ggstatsplot)
library(lmtest)
library(zoo)
library(moments)
library(tseries)
# library(rstatix)
library(MASS)
library(gap)
library(xtable)
library(stargazer)

data <- read.delim("datBI.txt", header = TRUE, sep = ";")
# Dataset
df <- 
  data.frame(data$DN3001, data$DI2000, data$PA0200, data$HB0100,
             data$PG0100, data$HG0700, data$HD1300, data$PE0700) #data$HG0400
df <- na.omit(df)
# rename columns
newnames <- c("net.wealth", "gross.income","education", "residence.size",
              "employee.income", "inc.norm", "mutual.funds", "time.job")  #"fin.inv"
setnames(df, colnames(df), new = newnames)

###### EXPLORATORY ANALYSIS ######

#summary dataset
summary(df)
str(df)

### Net wealth
summary(df$net.wealth)
sd(df$net.wealth)
skewness(df$net.wealth)
kurtosis(df$net.wealth)
hist(df$net.wealth)

# Gross Price density distribution
ggplot(df)+
  geom_density(aes(x= df$net.wealth), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(df$net.wealth),
                            sd = sd(df$net.wealth)))+
  labs(y = "Density", x = "Net Wealth")

# Q-Q plot
par(mfrow=c(1,1))
qqnorm(df$net.wealth) # Empirical
qqline(df$net.wealth, col="red") # Theoretical

# Shapiro-Wilk test: H0 - normality of data
shapiro.test(df$net.wealth) # Rejection of H0 -> net.wealth is not Normal

# Jarque-Bera tes: H0 - normality of data
jarque.bera.test(df$net.wealth) # Rejection of H0 -> net.wealth is not Normal

boxplot(df$net.wealth)


### gross.income
summary(df$gross.income)
sd(df$gross.income)
skewness(df$gross.income)
kurtosis(df$gross.income)
hist(df$gross.income)

# Gross Price density distribution
ggplot(df)+
  geom_density(aes(x= df$gross.income), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(df$gross.income),
                            sd = sd(df$gross.income)))+
  labs(y = "Density", x = "Net Wealth")

# Q-Q plot
par(mfrow=c(1,1))
qqnorm(df$gross.income) # Empirical
qqline(df$gross.income, col="red") # Theoretical

# Shapiro-Wilk test: H0 - normality of data
shapiro.test(df$gross.income) # Rejection of H0 -> gross.income is not Normal

# Jarque-Bera tes: H0 - normality of data
jarque.bera.test(df$gross.income) # Rejection of H0 -> gross.income  is not Normal

boxplot(df$gross.income)
boxplot(df$gross.income)$out


### residence.size
summary(df$residence.size)
sd(df$residence.size)
skewness(df$residence.size)
kurtosis(df$residence.size)
hist(df$residence.size)

# Gross Price density distribution
ggplot(df)+
  geom_density(aes(x= df$residence.size), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(df$residence.size),
                            sd = sd(df$residence.size)))+
  labs(y = "Density", x = "Net Wealth")

# Q-Q plot
par(mfrow=c(1,1))
qqnorm(df$residence.size) # Empirical
qqline(df$residence.size, col="red") # Theoretical

# Shapiro-Wilk test: H0 - normality of data
shapiro.test(df$residence.size) # Rejection of H0 -> residence.size is not Normal

# Jarque-Bera tes: H0 - normality of data
jarque.bera.test(df$residence.size) # Rejection of H0 -> residence.size  is not Normal

boxplot(df$residence.size)


### time.job
summary(df$time.job)
sd(df$time.job)
skewness(df$time.job)
kurtosis(df$time.job)
hist(df$time.job)

# Gross Price density distribution
ggplot(df)+
  geom_density(aes(x= df$time.job), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(df$time.job),
                            sd = sd(df$time.job)))+
  labs(y = "Density", x = "Net Wealth")

# Q-Q plot
par(mfrow=c(1,1))
qqnorm(df$time.job) # Empirical
qqline(df$time.job, col="red") # Theoretical

# Shapiro-Wilk test: H0 - normality of data
shapiro.test(df$time.job) # Rejection of H0 -> time.job is not Normal

# Jarque-Bera tes: H0 - normality of data
jarque.bera.test(df$time.job) # Rejection of H0 -> time.job  is not Normal

boxplot(df$time.job)
###########################################


plot(df$gross.income,df$net.wealth)
lines(panel.smooth(df$gross.income,df$net.wealth))

plot(df$residence.size,sqrt(df$net.wealth))
lines(panel.smooth(df$residence.size,df$net.wealth))

plot(df$time.job,sqrt(df$net.wealth))
lines(panel.smooth(df$time.job,df$net.wealth))

summary(lm(net.wealth ~ gross.income, data = df))
summary(lm(net.wealth ~ residence.size, data = df))


#-------------------------------------------------------------------------------
# Correlation Matrix
cor_var <- df[,c(1,2,4,8)]
#View(cor_var)
cor_mat <- cor(cor_var, method = "spearman")
round(cor_mat, 2)

# scatterplot for relationship between variables
pairs(df, upper.panel = panel.smooth)

# selection of numerical variables
sel = df[,c(1,2,4,8)]
pairs(sel, upper.panel = panel.smooth)

#-------------------------------------------------------------------------------
# MODELS ESTIMATION

# M1: complete model:
mod1<-lm(net.wealth ~ gross.income + as.character(education) + residence.size
         + employee.income + as.character(inc.norm) + mutual.funds + time.job, data = df, x=T, y=T)
summary(mod1)

# M2: NO education:
mod2 <- update(mod1, .~. -as.character(education))
summary(mod2)
# compare 2 nested model full vs reduced
anova(mod2,mod1) # restricted model
lrtest(mod1,mod2)

# M3: No education No income:
mod3<- update(mod2, .~. -as.character(inc.norm))
summary(mod3)
anova(mod3, mod2)

# M4: No mutual funds:
mod4<- update(mod2, .~. -mutual.funds)
summary(mod4)
anova(mod4, mod2) # restricted model

# M5: No income No mutual funds:
mod5<- update(mod2, .~. -mutual.funds)
summary(mod5)
anova(mod5, mod2) # restricted

# M6: gross.income power two
mod6<- update(mod1, .~. + I(gross.income^2))
summary(mod6)
anova(mod2, mod6) # larger model

# M7: gross.income power three
mod7<- update(mod2, .~.  + I(gross.income^2) + I(gross.income^3))
summary(mod7)

# M8: power two on residence size
mod8<- update(mod2, .~. + I(residence.size^2))
summary(mod8)

# M9: residence size power three
mod9<- update(mod2, .~. + I(residence.size^2) + I(residence.size^3))
summary(mod9)

#M10: power two on residence size and power two on gross.income 
mod10<- update(mod6, .~. + I(residence.size^2))
summary(mod10)
anova(mod6, mod10) # larger model

#M11: power 2 on residence size and power 3 on gross.income 
mod11<- update(mod10, .~. + I(gross.income^3))
summary(mod11)
anova(mod10, mod11) # larger model

#M12
mod12<- update(mod11, .~. - as.character(inc.norm))
summary(mod12)
anova(mod12, mod11) # restricted model

#M13: power two on time.job
mod13<- update(mod11, .~. + I(time.job^2))
summary(mod13)
anova(mod11, mod13) # larger

#M14: exp  on time.job
mod14<- update(mod11, .~. + I(exp(time.job)))
summary(mod14)
anova(mod11, mod14) # restricted

#M15
mod15<- update(mod13, .~. -as.character(inc.norm))
summary(mod15)
anova(mod15, mod13) # restricted

#M16 -18
mod16<- update(mod2, .~. -gross.income + log(gross.income))
summary(mod16)

mod17<- update(mod16, .~. -residence.size + log(residence.size))
summary(mod17)

mod18<- update(mod17, .~. -time.job + log(time.job)) # error
summary(mod18)


# RESET TEST:
resettest(mod1, power = 2:3, type = c("fitted"))
resettest(mod2, power = 2:3, type = c("fitted"))
resettest(mod3, power = 2:3, type = c("fitted"))
resettest(mod4, power = 2:3, type = c("fitted"))
resettest(mod5, power = 2:3, type = c("fitted"))
resettest(mod6, power = 2:3, type = c("fitted"))
resettest(mod7, power = 2:3, type = c("fitted"))
resettest(mod8, power = 2:3, type = c("fitted"))
resettest(mod9, power = 2:3, type = c("fitted"))
resettest(mod10, power = 2:3, type = c("fitted"))
resettest(mod11, power = 2:3, type = c("fitted"))
resettest(mod12, power = 2:3, type = c("fitted"))
resettest(mod13, power = 2:3, type = c("fitted"))
resettest(mod14, power = 2:3, type = c("fitted"))
resettest(mod15, power = 2:3, type = c("fitted"))
resettest(mod16, power = 2:3, type = c("fitted"))
resettest(mod17, power = 2:3, type = c("fitted"))
# p-value < 2.2e-16


# let's consider other type of interactions between the independent variables
# M19: complete model:
mod19<-lm(net.wealth ~ gross.income* employee.income + as.character(education)
          + mutual.funds + time.job, data = df, x=T)
summary(mod19)

# M20: NO education:
mod20 <- update(mod19, .~. -as.character(education))
summary(mod20)

# M21: complete model:
mod21<-lm(update(mod20, .~. + as.character(inc.norm)))
summary(mod21)

# M22: No mutual funds:
mod22<- update(mod20, .~. -mutual.funds + gross.income*mutual.funds)
summary(mod4)
anova(mod22, mod20) # restricted model

# M23: complete model:
mod23<-lm(update(mod20, .~. -mutual.funds+ as.character(inc.norm)))
summary(mod23)
#############
resettest(mod19, power = 2:3, type = c("fitted"))
resettest(mod20, power = 2:3, type = c("fitted"))
resettest(mod21, power = 2:3, type = c("fitted"))
resettest(mod22, power = 2:3, type = c("fitted"))
resettest(mod23, power = 2:3, type = c("fitted"))
# p-value < 2.2e-16

#-------------------------------------------------------------------------------
# Transformations of the dependent variable (Y)

# Let's now consider the following relationships:
plot(df$gross.income,log(df$net.wealth))
plot(df$gross.income,1/(df$net.wealth))

plot(df$gross.income,sqrt(df$net.wealth))
lines(panel.smooth(df$gross.income,sqrt(df$net.wealth)))

plot(df$residence.size,sqrt(df$net.wealth))
lines(panel.smooth(df$residence.size,sqrt(df$net.wealth)))

plot(df$time.job,sqrt(df$net.wealth))
lines(panel.smooth(df$time.job,sqrt(df$net.wealth)))

#--------
### transform y into log(y)
any(df$net.wealth==0)
mod<-lm(log(net.wealth+1) ~ gross.income + as.character(education) + residence.size
        + employee.income + as.character(inc.norm) + mutual.funds + time.job, data = df, x=T)
summary(mod)

# alternative
log_y <- log(df$net.wealth)
df$log_y = log_y
df <- na.omit(df)
mod<-lm(log_y ~ gross.income + as.character(education) + residence.size
        + employee.income + as.character(inc.norm) + mutual.funds + time.job, data = df, x=T)
summary(mod)

########
# Logs do not resolve our problems, so we move on with other type of transformations
########

#--------
# Note: in case you run lines 353 to 358, the re-run lines 26 to 33
### Square root of Y
mean(df$net.wealth)
# mean Y = 315999.3
sd(df$net.wealth)
# sd Y = 687861.8
# sigma/mu = 2.176783
# it seems that there is a proportionality between Y sd and Y mean
ysqr <- sqrt(df$net.wealth)
df$ysqr = ysqr
df <- na.omit(df)

m1_sqr<-lm(ysqr ~ gross.income + as.character(education) + residence.size
           + employee.income + as.character(inc.norm) + mutual.funds + time.job, data = df, x=T)
summary(m1_sqr)

m2_sqr<-lm(ysqr ~ gross.income + as.character(education) + residence.size
           + employee.income + mutual.funds + time.job, data = df, x=T)
summary(m2_sqr)
anova(m2_sqr, m1_sqr) #restricted

resettest(m1_sqr, power = 2:3, type = c("fitted"))
resettest(m2_sqr, power = 2:3, type = c("fitted")) # slightly improve compared to all the previous models
drop1(m2_sqr, test="F")

#-------------------------------------------------------------------------------
# set mod = choosen model
mod=m2_sqr
xtable(mod)
##

#-------------------------------------------------------------------------------
### RESIDUALS ANALYSIS ###
par(mfrow=c(1,1))
resid<-residuals(mod)
t.test(resid)
shapiro.test(resid)
qqnorm(scale(resid))
abline(0,1)

model<-formula(mod)
bptest(model,data=df)
dwtest(model,data=df)

confint(mod)
coeftest(mod)

yfit<-fitted(mod)

## Homoschedasticity
# if there is homoschedasticity the inclination of the line should be zero
summary(lm(abs(resid) ~ yfit))
## serial correlation
# independence if there is no pattern
n<-length(resid)
plot(resid[-n], resid[-1])
dwtest(mod)
#
## Let's check more in detail:
# Index plots:
par(mfrow=c(1,3))
plot(mod$residuals, ylab='Raw residuals', xlab='Observation index',
     main='Index plot of raw residuals') 
abline(h=0, col='red')
plot(rstandard(mod), ylab='Standardized residuals', xlab='Observation index',
     main='Index plot of standardized residuals')
abline(h=0, col='red')
plot(rstudent(mod), ylab='Externally studentized residuals', xlab='Observation index',
     main='Index plot of studentized residuals') 
abline(h=0, col='red')
#
# scatterplots vs fitted values:
par(mfrow=c(1,2))
plot(mod$fitted.values, mod$residuals, ylab='Residuals', xlab='fitted values',
     main='Residuals vs fitted values') 
abline(h=0, col='red')
plot(mod$fitted.values, rstandard(mod), ylab='Standardized Residuals', xlab='fitted values',
     main='Standardized residuals vs fitted values') 
abline(h=0, col='red')
#
# scatter plots vs each covariate (are there some non-linearities?):
head(mod$x) 
par(mfrow=c(3,2))
for (i in c(2:6)){
  plot(mod$x[,i], rstandard(mod), ylab='Standardized Residuals', xlab=colnames(mod$x)[i],
       main=paste('Standardized residuals vs',colnames(mod$x)[i], sep=' ') )
}

#squared residuals
Sq.res<-resid(mod)^2
par(mfrow=c(1,1))
plot(Sq.res~fitted(mod), xlab=expression(paste('estimated values',hat('y'), sep=' ')),
     ylab=expression(paste('squared residuals',hat('e')^2, sep=' ')))


# We can obtain leverages by the function hat (it stands for hatvalues) applied to the model.matrix:
lev<-hat(model.matrix(mod))
plot(lev,ylab="Leverages",main="Index plot of Leverages") 
lev.t<-2*ncol(model.matrix(mod))/nrow(model.matrix(mod)) # threshold leverage, twice the average level
abline(h=lev.t, col='red')
# units with high leverage:
h.l<-cbind(which(lev > lev.t),lev[c(which(lev > lev.t))]) #there are 25 cases with high leverage
h.l
#
# Cook's distances are given by the following function:
ckd<-cooks.distance(mod)
ckd
plot(ckd, main="Cook distance")
abline(h=4/length(ckd), col='green')
d.inf<-ckd<= 4/length(ckd)
table(d.inf) # influential observations 
#

qqPlot(mod, main = "Q-Q Plot for Standardized residuals, model", col = "darkgrey")


# let's try to see how many of these are outliers:
print(outl<-outlierTest(mod))

# let's then try to exclude all influential observations:
m2 = update(mod, subset = ckd<=4/length(ckd)) 
summary(m2)
plot(m2)

#Normality of residuals
summary(m2$residuals)
skewness(m2$residuals)
kurtosis(m2$residuals)
# skewness(m4$residuals) = 0.1891498
# kurtosis(m4$residuals) = 2.706209
plot(m2, which = 2) # almost normal
shapiro.test(resid(m2))
ks.test(m2$res, "pnorm") 
jarque.bera.test(m2$residuals)

# Histogram + normal curve
h <- hist(m2$residuals, col = "blue", xlab = "Residuals", freq=F, nclass = 50)
xfit <- seq(min(m2$residuals), max(m2$residuals), length=50)
yfit <- dnorm(xfit, mean=mean(m2$residuals), sd=sd(m2$residuals))
lines(xfit, yfit, col="red", lwd=2)

# VIF
vif(m2)
cor(m2$model[,c(1,2,5,7)])

#HOMOSCEDASTICITY
# Residuals vs fitted - we should have randomly located points
plot(m2, which=1)

# Breusch-Pagan test (it yeilds the rejection of H0, thus there is Heteroscedasticity)
#-> H0: homoscedastic residuals
bptest(m2, studentize = TRUE)
bptest(m2, studentize = FALSE)

resid<-residuals(m2)
t.test(resid)
yfit<-fitted(m2)
summary(lm(abs(resid) ~ yfit))

#serial correlation
n<-length(resid)
plot(resid[-n], resid[-1])
dwtest(m2)

# Underfitting, check the autocorrelation of the excluded variable
dwtest(m2, order.by=df$inc.norm[ckd<(4/length(ckd))])


#-------------------------------------------------------------------------------
# Weighted Least Squares Regression (in order to solve heteroscedasticity)
# define weights to use
wt <- 1/lm(abs(mod$residuals) ~ mod$fitted.values)$fitted.values^2

# weighted least squares regression
wls_model <-lm(ysqr ~ gross.income + as.character(education) + residence.size
               + employee.income + mutual.funds + time.job, data = df, weights=wt)
summary(wls_model)
xtable(wls_model)
plot(wls_model)

bptest(wls_model, studentize = T)

# We can obtain leverages by the function hat (it stands for hatvalues) applied to the model.matrix:
lev<-hat(model.matrix(wls_model))
plot(lev,ylab="Leverages",main="Index plot of Leverages") 
lev.t<-2*ncol(model.matrix(wls_model))/nrow(model.matrix(wls_model)) # threshold leverage, twice the average level
abline(h=lev.t, col='red')
# units with high leverage:
h.l<-cbind(which(lev > lev.t),lev[c(which(lev > lev.t))]) #there are 25 cases with high leverage
h.l
#
# Cook's distances are given by the following function:
ckd<-cooks.distance(wls_model)
ckd
plot(ckd, main="Cook distance")
abline(h=4/length(ckd), col='green')
d.inf<-ckd<= 4/length(ckd)
table(d.inf) # 148 influential observations 
#
qqPlot(wls_model, main = "Q-Q Plot for Standardized residuals, model", col = "darkgrey")

# let's try to see how many of these are outliers:
print(outl<-outlierTest(wls_model))

# let's then try to exclude all influential observations:
wls2 = update(wls_model, subset = ckd<=4/length(ckd)) 
summary(wls2)
xtable(wls2)
plot(wls2)

#RESIDUALS ANALYSIS
summary(wls2$residuals)
skewness(wls2$residuals)
kurtosis(wls2$residuals)
shapiro.test(wls2$residuals)
jarque.bera.test(wls2$residuals)

resettest(wls2, power = 2:3, type = c("fitted"))

# Overfitting, we use the Variance Inflation Factor (VIF) to check whether or not there is multicollinearity
vif(wls2)

### HETEROSKEDASTICITY ###
# Breusch-Pagan test
bptest(wls2, studentize = T)  # the residuals are homoskedastic
plot(wls2)


# DW test
# Error in dwtest(wls2) : weighted regressions are not supported
### RESIDUALS ANALYSIS ###
par(mfrow=c(1,1))
resid<-residuals(wls2)
t.test(resid)
shapiro.test(resid)
qqnorm(scale(resid))
abline(0,1)

model<-formula(wls2)
bptest(model,data=df)
dwtest(model,data=df)

yfit<-fitted(wls2)

# Homoschedasticity
summary(lm(abs(resid) ~ yfit))
#serial correlation
n<-length(resid)
plot(resid[-n], resid[-1])


#------------------------------------------------------------
# WLS using transformations of the independent variables
df$log.time.job = log(df$time.job)
df <- df[!is.infinite(rowSums(df)),]

m3_sqr<-lm(ysqr ~ log(gross.income) + as.character(education) + log(residence.size)
           + employee.income + mutual.funds + log.time.job, data = df, x=T)
summary(m3_sqr)

mod=m3_sqr
wt <- 1/lm(abs(mod$residuals) ~ mod$fitted.values)$fitted.values^2

# weighted least squares regression
wls_model <-lm(ysqr ~ log(gross.income) + as.character(education) + log(residence.size)
               + employee.income + mutual.funds + log.time.job, data = df, weights=wt)
summary(wls_model)
xtable(wls_model)
plot(wls_model)

# We can obtain leverages by the function hat (it stands for hatvalues) applied to the model.matrix:
lev<-hat(model.matrix(wls_model))
plot(lev,ylab="Leverages",main="Index plot of Leverages") 
lev.t<-2*ncol(model.matrix(wls_model))/nrow(model.matrix(wls_model)) # threshold leverage, twice the average level
abline(h=lev.t, col='red')
# units with high leverage:
h.l<-cbind(which(lev > lev.t),lev[c(which(lev > lev.t))]) #there are 25 cases with high leverage
h.l
#
# Cook's distances are given by the following function:
ckd<-cooks.distance(wls_model)
ckd
plot(ckd, main="Cook distance")
abline(h=4/length(ckd), col='green')
d.inf<-ckd<= 4/length(ckd)
table(d.inf) # 148 influential observations 
#
qqPlot(wls_model, main = "Q-Q Plot for Standardized residuals, model", col = "darkgrey")

# let's try to see how many of these are outliers:
print(outl<-outlierTest(wls_model))

# let's then try to exclude all influential observations:
wls2 = update(wls_model, subset = ckd<=4/length(ckd)) 
summary(wls2)
xtable(wls2)
plot(wls2)

#RESIDUALS ANALYSIS
summary(wls2$residuals)
skewness(wls2$residuals)
kurtosis(wls2$residuals)
shapiro.test(wls2$residuals)
jarque.bera.test(wls2$residuals)

resettest(wls2, power = 2:3, type = c("fitted"))

# Overfitting, we use the Variance Inflation Factor (VIF) to check whether or not there is multicollinearity
vif(wls2)

### HETEROSKEDASTICITY ###
# Breusch-Pagan test
bptest(wls2, studentize = T)  # the residuals are homoskedastic
plot(wls2)


