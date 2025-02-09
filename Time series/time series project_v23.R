
setwd("/Users/gianlorenzo/Desktop/FINASS/Time Series/Project/")

graphics.off()
rm(list = ls())

# used until now
library(readxl)
library(xts)
library(highfrequency)
library(tseries)
library(ggplot2)

library(fBasics)
library(rugarch)
library(rumidas)

library(FinTS)
library(MCS)
library(psych)
library(car)

library(forecast)

###
library(stargazer)
library(broom)
library(xtable)
###

energy_data <- read_excel("energydata_covid.xlsx", sheet = 1, skip = 4)
covid_data <- read_excel("energydata_covid.xlsx", sheet = 2, skip = 4)

energy_data <- na.omit(energy_data)
covid_data <- na.omit(covid_data)

class(energy_data)
class(covid_data)

##################### time series transformation (transform data.frame into xts)
Date_energy<-strptime(energy_data$...1, "%Y-%m-%d",tz="GMT")
energy_i<-as.xts(energy_data[,2:ncol(energy_data)],Date_energy)

range(time(energy_i))

Date_covid<-strptime(covid_data$...1, "%Y-%m-%d",tz="GMT")
covid_i<-as.xts(covid_data[,2:ncol(covid_data)],Date_covid)

range(time(covid_i))

### plot
par(mfrow=c(2,3))
plot(energy_i$WTI, grid.col = NA)
plot(energy_i$Europe.Brent, grid.col = NA)
plot(energy_i$Heating.Oil, grid.col = NA)
plot(energy_i$Propane, grid.col = NA)
plot(energy_i$Gasoline, grid.col = NA)
plot(energy_i$Kerosene, grid.col = NA)

par(mfrow=c(1,1))
plot(covid_i$`Deaths USA`, grid.col = NA)

### ADF      H_0 = no stationarity
adf.test(energy_i$WTI, alternative= "stationary")
adf.test(energy_i$Europe.Brent, alternative= "stationary")
adf.test(energy_i$Heating.Oil, alternative= "stationary")
adf.test(energy_i$Propane, alternative= "stationary")
adf.test(energy_i$Gasoline, alternative= "stationary") # p-value = 0.02981
adf.test(energy_i$Kerosene, alternative= "stationary") # p-value = 0.02426

adf.test(covid_i$`Deaths USA`,alternative= "stationary")

# ACF for each series
par(mfrow=c(2,3))
acf(coredata(energy_i$WTI))
acf(coredata(energy_i$Europe.Brent))
acf(coredata(energy_i$Heating.Oil))
acf(coredata(energy_i$Propane))
acf(coredata(energy_i$Gasoline))
acf(coredata(energy_i$Kerosene))

# PACF for each series
par(mfrow=c(2,3))
pacf(coredata(energy_i$WTI))
pacf(coredata(energy_i$Europe.Brent))
pacf(coredata(energy_i$Heating.Oil))
pacf(coredata(energy_i$Propane))
pacf(coredata(energy_i$Gasoline))
pacf(coredata(energy_i$Kerosene))

# describe (summary statistics)
describe(energy_i$WTI)
describe(energy_i$Europe.Brent)
describe(energy_i$Heating.Oil)
describe(energy_i$Propane)
describe(energy_i$Gasoline)
describe(energy_i$Kerosene)
describe(covid_i$`Deaths USA`)


### log-returns ###
r_t<-makeReturns(energy_i)
N<-length(r_t$WTI)

r_t<- na.omit(r_t)

diff_covid <-makeReturns(covid_i)
L<-length(diff_covid)

diff_covid <- na.omit(diff_covid)

### Squared returns
r_t_sq <- r_t^2

### plot first differences
par(mfrow=c(2,3))
plot(r_t$WTI, grid.col = NA)
plot(r_t$Europe.Brent, grid.col = NA)
plot(r_t$Heating.Oil, grid.col = NA)
plot(r_t$Propane, grid.col = NA)
plot(r_t$Gasoline, grid.col = NA)
plot(r_t$Kerosene, grid.col = NA)

par(mfrow=c(1,1))
plot(diff_covid)

# ACF for each energy series
par(mfrow=c(2,3))
acf(coredata(r_t$WTI))
acf(coredata(r_t$Europe.Brent))
acf(coredata(r_t$Heating.Oil))
acf(coredata(r_t$Propane))
acf(coredata(r_t$Gasoline))
acf(coredata(r_t$Kerosene))

# PACF for returns of the energy series
par(mfrow=c(2,3))
pacf(coredata(r_t$WTI))
pacf(coredata(r_t$Europe.Brent))
pacf(coredata(r_t$Heating.Oil))
pacf(coredata(r_t$Propane))
pacf(coredata(r_t$Gasoline))
pacf(coredata(r_t$Kerosene))

# describe (summary statistics)
describe(r_t$WTI)
describe(r_t$Europe.Brent)
describe(r_t$Heating.Oil)
describe(r_t$Propane)
describe(r_t$Gasoline)
describe(r_t$Kerosene)
describe(covid_i$`Deaths USA`)

# ADF first differences
adf.test(r_t$WTI, alternative= "stationary")
adf.test(r_t$Europe.Brent, alternative= "stationary")
adf.test(r_t$Heating.Oil, alternative= "stationary")
adf.test(r_t$Propane, alternative= "stationary")
adf.test(r_t$Gasoline, alternative= "stationary")
adf.test(r_t$Kerosene, alternative= "stationary")

adf.test(diff_covid$`Deaths USA`,alternative= "stationary")

# ACF for squared returns of the energy series
par(mfrow=c(2,3))
acf(coredata(r_t_sq$WTI))
acf(coredata(r_t_sq$Europe.Brent))
acf(coredata(r_t_sq$Heating.Oil))
acf(coredata(r_t_sq$Propane))
acf(coredata(r_t_sq$Gasoline))
acf(coredata(r_t_sq$Kerosene))


###### QQNORM
par(mfrow=c(2,3))
qqnorm(r_t[,1], main = "WTI")
qqline(r_t[,1], col = 2, distribution= qnorm)

qqnorm(r_t[,2], main = "Europe Brent")
qqline(r_t[,2], col = 2, distribution= qnorm)

qqnorm(r_t[,3], main = "Heating Oil")
qqline(r_t[,3], col = 2, distribution= qnorm)

qqnorm(r_t[,4], main = "Propane")
qqline(r_t[,4], col = 2, distribution= qnorm)

qqnorm(r_t[,5], main = "Gasoline")
qqline(r_t[,5], col = 2, distribution= qnorm)

qqnorm(r_t[,6], main = "Kerosene")
qqline(r_t[,6], col = 2, distribution= qnorm)

# Skewness
skewness(r_t[,1])
skewness(r_t[,2])
skewness(r_t[,3])
skewness(r_t[,4])
skewness(r_t[,5])
skewness(r_t[,6])

# Excess of Kurtosis (so kurtosis -3)
kurtosis(r_t[,1])
kurtosis(r_t[,2])
kurtosis(r_t[,3])
kurtosis(r_t[,4])
kurtosis(r_t[,5])
kurtosis(r_t[,6])

# Jarque Bera test      H_0 : Normality. If p-value < 0.05 we reject H_0
jarque.bera.test(r_t[,1]) # WTI
jarque.bera.test(r_t[,2]) # Europe.Brent
jarque.bera.test(r_t[,3]) # Heating.Oil
jarque.bera.test(r_t[,4]) # Propane
jarque.bera.test(r_t[,5]) # Gasoline
jarque.bera.test(r_t[,6]) # Kerosene

jarque.bera.test(diff_covid$`Deaths USA`)


#test correlation (White Noise test) H_0 : No correlation
Box.test(r_t[,1],type="Ljung-Box",lag=20)
Box.test(r_t[,1],type="Box-Pierce",lag=20)

Box.test(r_t[,2],type="Ljung-Box",lag=20)
Box.test(r_t[,2],type="Box-Pierce",lag=20)

Box.test(r_t[,3],type="Ljung-Box",lag=20)
Box.test(r_t[,3],type="Box-Pierce",lag=20)

Box.test(r_t[,4],type="Ljung-Box",lag=20)
Box.test(r_t[,4],type="Box-Pierce",lag=20)

Box.test(r_t[,5],type="Ljung-Box",lag=20)
Box.test(r_t[,5],type="Box-Pierce",lag=20)

Box.test(r_t[,6],type="Ljung-Box",lag=20)
Box.test(r_t[,6],type="Box-Pierce",lag=20)

# squared returns
Box.test(r_t_sq[,1],type="Ljung-Box",lag=20) #p-value < 2.2e-16
Box.test(r_t_sq[,1],type="Box-Pierce",lag=20) #p-value < 2.2e-16

Box.test(r_t_sq[,2],type="Ljung-Box",lag=20)
Box.test(r_t_sq[,2],type="Box-Pierce",lag=20)

Box.test(r_t_sq[,3],type="Ljung-Box",lag=20)
Box.test(r_t_sq[,3],type="Box-Pierce",lag=20)

Box.test(r_t_sq[,4],type="Ljung-Box",lag=20)
Box.test(r_t_sq[,4],type="Box-Pierce",lag=20)

Box.test(r_t_sq[,5],type="Ljung-Box",lag=20)
Box.test(r_t_sq[,5],type="Box-Pierce",lag=20)

Box.test(r_t_sq[,6],type="Ljung-Box",lag=20)
Box.test(r_t_sq[,6],type="Box-Pierce",lag=20)


# Heteroscedasticity test H_0 : No heteroscedasticity # p-value < 0.05 we reject H_0                                    # VEDERE LAG PRECISO
ArchTest(r_t[,1], lag = 30)
ArchTest(r_t[,2], lag = 30)
ArchTest(r_t[,3], lag = 30)
ArchTest(r_t[,4], lag = 30)
ArchTest(r_t[,5], lag = 30)
ArchTest(r_t[,6], lag = 30)

#### ARCH TESTS ####
ArchTest(r_t$WTI, lags = 12)
ArchTest(r_t$Europe.Brent, lags = 12)
ArchTest(r_t$Heating.Oil, lags = 12)
ArchTest(r_t$Propane, lags = 12)
ArchTest(r_t$Gasoline, lags = 12)
ArchTest(r_t$Kerosene, lags = 12)
##########################

# We have an  ARCH effect, meaning that there is presence of heteroskedasticity

#significance of mean different from zero test. H_0 : true mean is equal zero
t.test(r_t[,1]) #p-value > 0.05 we do not reject H_O                                                                    # SERVE ??????


#Long Memory Test. 0 < H < 0.5 anti-persistent series. H = 0.5 RW series. 0.5 < H < 1 persistent series                 # SERVE ??????
Qu.test(r_t[,1], 3214, epsilon = 0.05)

# Simple R/S Hurst estimation < 0.5. Series do not show memory persistence
hurstexp(r_t$WTI, d = 50, display = TRUE)
hurstexp(r_t$Europe.Brent, d = 50, display = TRUE)
hurstexp(r_t$Heating.Oil, d = 50, display = TRUE)
hurstexp(r_t$Propane, d = 50, display = TRUE)
hurstexp(r_t$Gasoline, d = 50, display = TRUE)
hurstexp(r_t$Kerosene, d = 50, display = TRUE)


Qu.test(r_t_sq[,1], 3214, epsilon = 0.05)
hurstexp(r_t_sq[,1], d = 50, display = TRUE) # Simple R/S Hurst estimation > 0.5. Series slightly show memory persistence

########


###### GARCH #####
# GARCH models with different distribution errors:
# Normal, T-student, Skew Normal, Skew t, GED

## FITTING
# we define the variety of GARCH models we consider
spec.comp<-list()
models<- c("sGARCH","eGARCH","gjrGARCH")
distributions<-c("norm","std","snorm","sstd","ged","sged")
spec.comp<-list()
m<-c()
d<-c()
for(m in models) {
  for(d in distributions) {
    spec.comp[[paste(m,d,sep="-")]] <-
      ugarchspec(mean.model= list(armaOrder= c(0,0)),
                 variance.model=list(model=m, garchOrder= c(1,1)),
                 distribution.model=d)
  }
}

specifications<- names(spec.comp)
specifications

#Fit WTI data into the models
fitmod.WTI<- list()
for(s in specifications) {
  fitmod.WTI[[s]] <- ugarchfit(spec= spec.comp[[s]], r_t$WTI)
}
fitmod.WTI

#Fit Europe.Brent data into the models
fitmod.Europe.Brent<- list()
for(s in specifications) {
  fitmod.Europe.Brent[[s]] <- ugarchfit(spec= spec.comp[[s]], r_t$Europe.Brent)
}
fitmod.Europe.Brent

#Fit Heating.Oil data into the models
fitmod.Heating.Oil<- list()
for(s in specifications) {
  fitmod.Heating.Oil[[s]] <- ugarchfit(spec= spec.comp[[s]], r_t$Heating.Oil)
}
fitmod.Heating.Oil

#Fit Propane data into the models
fitmod.Propane<- list()
for(s in specifications) {
  fitmod.Propane[[s]] <- ugarchfit(spec= spec.comp[[s]], r_t$Propane)
}
fitmod.Propane

#Fit Gasoline data into the models
fitmod.Gasoline<- list()
for(s in specifications) {
  fitmod.Gasoline[[s]] <- ugarchfit(spec= spec.comp[[s]], r_t$Gasoline)
}
fitmod.Gasoline

#Fit Kerosene data into the models
fitmod.Kerosene<- list()
for(s in specifications) {
  fitmod.Kerosene[[s]] <- ugarchfit(spec= spec.comp[[s]], r_t$Kerosene)
}
fitmod.Kerosene


# Fit Diagnostic (pag 27 intro to the rugarch package pdf)                                                              # TO DO
# The summary method for the uGARCHfit object provides the parameters and their standard errors (and a robust version)
# inforcriteria 
# The nymblom test calculates the parameter stability test of Nyblom (1989), as well as the joint test.



########## Strcutural Breaks
library(strucchange)

sigma.s = fitmod.Kerosene$`sGARCH-norm`@fit$sigma
sigma.s = as.ts(sigma.s, start = first(time(r_t)), end = last(time(r_t)), frequency = 1)

test2 <- Fstats(sigma.s~1) #Gets a sequence of fstatistics for all possible 
# break points within the middle 70% of myts1
sigma.fs <- test2$Fstats #These are the fstats
bp.sigma <- breakpoints(sigma.s~1) #Gets the breakpoint based on the F-stats
plot(sigma.s) #plots the series myts1
lines(bp.sigma) #plots the break date implied by the sup F test
bd.sigma <- breakdates(bp.sigma) #Obtains the implied break data (2018.35, 
# referring to day 128 (0.35*365 = day number))
sctest(test2) #Obtains a p-value for the implied breakpoint
ci.sigma <- confint(bp.sigma) #95% CI for the location break date
plot(sigma.s, main = "Kerosene", ylab = "volatility")
lines(ci.sigma) #This shows the interval around the estimated break date
lines(bp.sigma, col = "green")


# ZIVOT ANDREWS                                                                                                         # TO DO
library(urca)
?ur.za





##########




# Perform the rolling forecasts

#### WTI ROLL ####
WTI.roll<-list()
for(s in specifications) {
  WTI.roll[[s]]<- ugarchroll(spec=spec.comp[[s]], data= r_t$WTI,
                             forecast.length= 80,
                             refit.every=1, refit.window= "moving")
}

WTI.roll
summary(WTI.roll)

#### Europe.Brent ROLL ####
Europe.Brent.roll<-list()
for(s in specifications) {
  Europe.Brent.roll[[s]]<- ugarchroll(spec=spec.comp[[s]], data= r_t$Europe.Brent,
                             forecast.length= 80,
                             refit.every=1, refit.window= "moving")
}

Europe.Brent.roll
summary(Europe.Brent.roll)

#### Heating.Oil ROLL ####
Heating.Oil.roll<-list()
for(s in specifications) {
  Heating.Oil.roll[[s]]<- ugarchroll(spec=spec.comp[[s]], data= r_t$Heating.Oil,
                             forecast.length= 80,
                             refit.every=1, refit.window= "moving")
}

Heating.Oil.roll
summary(Heating.Oil.roll)

#### Propane ROLL ####
Propane.roll<-list()
for(s in specifications) {
  Propane.roll[[s]]<- ugarchroll(spec=spec.comp[[s]], data= r_t$Propane,
                             forecast.length= 80,
                             refit.every=1, refit.window= "moving")
}

Propane.roll
summary(Propane.roll)

#### Gasoline ROLL ####
Gasoline.roll<-list()
for(s in specifications) {
  Gasoline.roll[[s]]<- ugarchroll(spec=spec.comp[[s]], data= r_t$Gasoline,
                             forecast.length= 80,
                             refit.every=1, refit.window= "moving")
}

Gasoline.roll
summary(Gasoline.roll)

#### Kerosene ROLL ####                                                                                                 
Kerosene.roll<-list()
for(s in specifications) {
  Kerosene.roll[[s]]<- ugarchroll(spec=spec.comp[[s]], data= r_t$Kerosene,
                             forecast.length= 80,
                             refit.every=1, refit.window= "moving")
}

Kerosene.roll
summary(Kerosene.roll)





# Extracting VaR from Rolling forecast #
####### VaR WTI #######
VaR.WTI=list()
for(s in specifications) {
  VaR.WTI[[s]]<-as.data.frame(WTI.roll[[s]], which="VaR")
}
VaR.WTI
str(VaR.WTI)

####### Europe.Brent #######
VaR.Europe.Brent=list()
for(s in specifications) {
  VaR.Europe.Brent[[s]]<-as.data.frame(Europe.Brent.roll[[s]], which="VaR")
}
VaR.Europe.Brent
str(VaR.Europe.Brent)

####### Heating.Oil #######
VaR.Heating.Oil=list()
for(s in specifications) {
  VaR.Heating.Oil[[s]]<-as.data.frame(Heating.Oil.roll[[s]], which="VaR")
}
VaR.Heating.Oil
str(VaR.Heating.Oil)

####### Propane #######
VaR.Propane=list()
for(s in specifications) {
  VaR.Propane[[s]]<-as.data.frame(Propane.roll[[s]], which="VaR")
}
VaR.Propane
str(VaR.Propane)

####### Gasoline #######
VaR.Gasoline=list()
for(s in specifications) {
  VaR.Gasoline[[s]]<-as.data.frame(Gasoline.roll[[s]], which="VaR")
}
VaR.Gasoline
str(VaR.Gasoline)

####### Kerosene #######
VaR.Kerosene=list()
for(s in specifications) {
  VaR.Kerosene[[s]]<-as.data.frame(Kerosene.roll[[s]], which="VaR")
}
VaR.Kerosene
str(VaR.Kerosene)







###### GARCH MIDAS #####
# Estimate GARCH models with an observed variable at weekly frequency (Covid deaths)

#### Time lapse between dates (for mv_into_mat)
# diff_time <- as.numeric(difftime(first(r_t_est), first(covid_i), units = "day"))
# (diff_time < 0 | ((diff_time - K * num_obs_k) < 0))
difftime(strptime("2020-08-25", format = "%Y-%m-%d"), strptime("2020-03-02", format = "%Y-%m-%d"), units="days")


r_t_est <- r_t['2020-08-15/2021-06-04']                                                                                 # K = 24
K<- 24 # number of lagged realizations entering the long-run equation
covid_mv<-mv_into_mat(r_t_est$WTI,diff_covid,K=K,"weekly")

dim(covid_mv)
length(r_t_est$WTI)

# out of sample
n_oos = 80

# Fit the data into the models                                                                          # WTI
fit.gm_WTI <- c()
models<- c("GM","DAGM")
distributions<-c("norm","std")
skew<-c("YES", "NO")
m<-c()
d<-c()
s<-c()
for(m in models) {
  for(d in distributions) {
    for(s in skew) {
      fit.gm_WTI[[paste(m,s,d,sep="-")]] <- ugmfit(model=m, skew=s,distribution=d,
                                                   daily_ret=r_t_est$WTI,mv_m=covid_mv, K=K,
                                                   out_of_sample=n_oos)
    }
  }
}

specifications2<- names(fit.gm_WTI)
specifications2

fit.gm_oos$`GM-YES-norm`$est_vol_oos
summary.rumidas(fit.gm_oos$`GM-YES-norm`)


# Fit the data into the models                                                                         # Europe.Brent
covid_mv2 <-mv_into_mat(r_t_est$Europe.Brent,diff_covid,K=K,"weekly")

fit.gm_Europe.Brent <- c()
models<- c("GM","DAGM")
distributions<-c("norm","std")
skew<-c("YES", "NO")
m<-c()
d<-c()
s<-c()
for(m in models) {
  for(d in distributions) {
    for(s in skew) {
      fit.gm_Europe.Brent[[paste(m,s,d,sep="-")]] <- ugmfit(model=m, skew=s,distribution=d,
                                                   daily_ret=r_t_est$Europe.Brent,mv_m=covid_mv2 ,K=K,
                                                   out_of_sample=n_oos)
    }
  }
}


# Fit the data into the models                                                                         # Heating.Oil
covid_mv3 <-mv_into_mat(r_t_est$Heating.Oil ,diff_covid,K=K,"weekly")

fit.gm_Heating.Oil <- c()
models<- c("GM","DAGM")
distributions<-c("norm","std")
skew<-c("YES", "NO")
m<-c()
d<-c()
s<-c()
for(m in models) {
  for(d in distributions) {
    for(s in skew) {
      fit.gm_Heating.Oil[[paste(m,s,d,sep="-")]] <- ugmfit(model=m, skew=s,distribution=d,
                                                   daily_ret=r_t_est$Heating.Oil,mv_m=covid_mv3, K=K,
                                                   out_of_sample=n_oos)
    }
  }
}


# Fit the data into the models                                                                         # Propane
covid_mv4 <-mv_into_mat(r_t_est$Propane ,diff_covid,K=K,"weekly")

fit.gm_Propane <- c()
models<- c("GM","DAGM")
distributions<-c("norm","std")
skew<-c("YES", "NO")
m<-c()
d<-c()
s<-c()
for(m in models) {
  for(d in distributions) {
    for(s in skew) {
      fit.gm_Propane[[paste(m,s,d,sep="-")]] <- ugmfit(model=m, skew=s,distribution=d,
                                                   daily_ret=r_t_est$Propane,mv_m=covid_mv4, K=K,
                                                   out_of_sample=n_oos)
    }
  }
}


# Fit the data into the models                                                                         # Gasoline
covid_mv5 <-mv_into_mat(r_t_est$Gasoline ,diff_covid,K=K,"weekly")

fit.gm_Gasoline <- c()
models<- c("GM","DAGM")
distributions<-c("norm","std")
skew<-c("YES", "NO")
m<-c()
d<-c()
s<-c()
for(m in models) {
  for(d in distributions) {
    for(s in skew) {
      fit.gm_Gasoline[[paste(m,s,d,sep="-")]] <- ugmfit(model=m, skew=s,distribution=d,
                                                   daily_ret=r_t_est$Gasoline,mv_m=covid_mv5, K=K,
                                                   out_of_sample=n_oos)
    }
  }
}


# Fit the data into the models                                                                         # Kerosene
covid_mv6 <-mv_into_mat(r_t_est$Kerosene ,diff_covid,K=K,"weekly")

fit.gm_Kerosene <- c()
models<- c("GM","DAGM")
distributions<-c("norm","std")
skew<-c("YES", "NO")
m<-c()
d<-c()
s<-c()
for(m in models) {
  for(d in distributions) {
    for(s in skew) {
      fit.gm_Kerosene[[paste(m,s,d,sep="-")]] <- ugmfit(model=m, skew=s,distribution=d,
                                                   daily_ret=r_t_est$Kerosene,mv_m=covid_mv6, K=K,
                                                   out_of_sample=n_oos)
    }
  }
}




#########################                                                                                               # TO DO
library(PerformanceAnalytics)

VaR.calc <- function(x){
  x * (qdist(distribution = "norm" , p = 0.01))
}

roll.cond_vol = c()
for(j in specifications2){
  roll.cond_vol[[j]] <- apply.rolling(fit.gm_WTI[[j]]$est_vol_oos, width = 1, trim = TRUE, gap = 1, by = 1, FUN = "VaR.calc")
}
#########################




#############################
#      VaR Garch MIDAS     #                                                                                            
############################
# VaR Garch Midas = conditional vol * quantile distribution
# Creating VaR object list in R as that coming from the ugarchroll

gm_std = specifications2[c(3, 4, 7, 8)]
gm_norm = specifications2[- c(3, 4, 7, 8)]


#### VaR for GM models with normal dist ####                                                             # WTI
# VaR 1% GM norm
VaR.WTI.gm.99 = c()
for(j in gm_norm){
  VaR.WTI.gm.99[[j]] = as.data.frame(fit.gm_WTI[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.01)))
}
VaR.WTI.gm.99

# rename V1 into alpha(1%)
for(j in gm_norm){
  names(VaR.WTI.gm.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM norm
VaR.WTI.gm.95 = c()
for(j in gm_norm){
  VaR.WTI.gm.95[[j]] = as.data.frame(fit.gm_WTI[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.05)))
}
VaR.WTI.gm.95

for(j in gm_norm){
  VaR.WTI.gm.99[[j]]$`alpha(5%)` <- VaR.WTI.gm.95[[j]]$V1
}

for(j in gm_norm){
  VaR.WTI.gm.99[[j]]$realized <- as.double(tail(r_t_est$WTI, n_oos))
}

# rename VaR.WTI.gm.99 into VaR.WTI.gm_norm
VaR.WTI.gm_norm = VaR.WTI.gm.99
rm(VaR.WTI.gm.99, VaR.WTI.gm.95)


#### VaR for GM models with std dist ####
# vector with shapes for std
shapes <- c(fit.gm_WTI[["GM-YES-std"]]$rob_coef_mat[7,1],
            fit.gm_WTI[["GM-NO-std"]]$rob_coef_mat[6,1],
            fit.gm_WTI[["DAGM-YES-std"]]$rob_coef_mat[9,1],
            fit.gm_WTI[["DAGM-NO-std"]]$rob_coef_mat[8,1])

# VaR 1% GM std
VaR.WTI.gm_std.99= c()
for(ss in shapes){
  for(j in gm_std){
    VaR.WTI.gm_std.99[[j]] = as.data.frame(fit.gm_WTI[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.01)))
  }
}
VaR.WTI.gm_std.99

# rename V1 into alpha(1%)
for(j in gm_std){
  names(VaR.WTI.gm_std.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM std
VaR.WTI.gm_std.95 = c()
for(ss in shapes){
  for(j in gm_std){
    VaR.WTI.gm_std.95[[j]] = as.data.frame(fit.gm_WTI[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.05)))
  }
}
VaR.WTI.gm_std.95

for(j in gm_std){
  VaR.WTI.gm_std.99[[j]]$`alpha(5%)` <- VaR.WTI.gm_std.95[[j]]$V1
}

for(j in gm_std){
  VaR.WTI.gm_std.99[[j]]$realized <- as.double(tail(r_t_est$WTI, n_oos))
}

# rename VaR.WTI.gm_std.99 into VaR.WTI.gm
VaR.WTI.gm_std = VaR.WTI.gm_std.99
rm(VaR.WTI.gm_std.99, VaR.WTI.gm_std.95)

# Combine the 2 VaR gm list into 1
VaR.WTI.gm = c(VaR.WTI.gm_norm, VaR.WTI.gm_std)
rm(VaR.WTI.gm_norm, VaR.WTI.gm_std) # remove obejcts we don't need anymore

# Merging the 2 VaR obeject, rember to extract VaR from ugarchroll
VaR.WTI = c(VaR.WTI, VaR.WTI.gm)
rm(VaR.WTI.gm)
#####







#### VaR for GM models with normal dist ####                                                             # Europe.Brent
# VaR 1% GM norm
VaR.Europe.Brent.gm.99 = c()
for(j in gm_norm){
  VaR.Europe.Brent.gm.99[[j]] = as.data.frame(fit.gm_Europe.Brent[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.01)))
}
VaR.Europe.Brent.gm.99

# rename V1 into alpha(1%)
for(j in gm_norm){
  names(VaR.Europe.Brent.gm.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM norm
VaR.Europe.Brent.gm.95 = c()
for(j in gm_norm){
  VaR.Europe.Brent.gm.95[[j]] = as.data.frame(fit.gm_Europe.Brent[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.05)))
}
VaR.Europe.Brent.gm.95

for(j in gm_norm){
  VaR.Europe.Brent.gm.99[[j]]$`alpha(5%)` <- VaR.Europe.Brent.gm.95[[j]]$V1
}

for(j in gm_norm){
  VaR.Europe.Brent.gm.99[[j]]$realized <- as.double(tail(r_t_est$Europe.Brent, n_oos))
}

# rename VaR.Europe.Brent.gm.99 into VaR.Europe.Brent.gm_norm
VaR.Europe.Brent.gm_norm = VaR.Europe.Brent.gm.99
rm(VaR.Europe.Brent.gm.99, VaR.Europe.Brent.gm.95)


#### VaR for GM models with std dist ####
# vector with shapes for std
shapes <- c(fit.gm_Europe.Brent[["GM-YES-std"]]$rob_coef_mat[7,1],
            fit.gm_Europe.Brent[["GM-NO-std"]]$rob_coef_mat[6,1],
            fit.gm_Europe.Brent[["DAGM-YES-std"]]$rob_coef_mat[9,1],
            fit.gm_Europe.Brent[["DAGM-NO-std"]]$rob_coef_mat[8,1])

# VaR 1% GM std
VaR.Europe.Brent.gm_std.99= c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Europe.Brent.gm_std.99[[j]] = as.data.frame(fit.gm_Europe.Brent[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.01)))
  }
}
VaR.Europe.Brent.gm_std.99

# rename V1 into alpha(1%)
for(j in gm_std){
  names(VaR.Europe.Brent.gm_std.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM std
VaR.Europe.Brent.gm_std.95 = c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Europe.Brent.gm_std.95[[j]] = as.data.frame(fit.gm_Europe.Brent[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.05)))
  }
}
VaR.Europe.Brent.gm_std.95

for(j in gm_std){
  VaR.Europe.Brent.gm_std.99[[j]]$`alpha(5%)` <- VaR.Europe.Brent.gm_std.95[[j]]$V1
}

for(j in gm_std){
  VaR.Europe.Brent.gm_std.99[[j]]$realized <- as.double(tail(r_t_est$Europe.Brent, n_oos))
}

# rename VaR.Europe.Brent.gm_std.99 into VaR.Europe.Brent.gm
VaR.Europe.Brent.gm_std = VaR.Europe.Brent.gm_std.99
rm(VaR.Europe.Brent.gm_std.99, VaR.Europe.Brent.gm_std.95)

# Combine the 2 VaR gm list into 1
VaR.Europe.Brent.gm = c(VaR.Europe.Brent.gm_norm, VaR.Europe.Brent.gm_std)
rm(VaR.Europe.Brent.gm_norm, VaR.Europe.Brent.gm_std) # remove obejcts we don't need anymore

# Merging the 2 VaR obeject, rember to extract VaR from ugarchroll
VaR.Europe.Brent = c(VaR.Europe.Brent, VaR.Europe.Brent.gm)
rm(VaR.Europe.Brent.gm)
#####






#### VaR for GM models with normal dist ####                                                             # Heating.Oil
# VaR 1% GM norm
VaR.Heating.Oil.gm.99 = c()
for(j in gm_norm){
  VaR.Heating.Oil.gm.99[[j]] = as.data.frame(fit.gm_Heating.Oil[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.01)))
}
VaR.Heating.Oil.gm.99

# rename V1 into alpha(1%)
for(j in gm_norm){
  names(VaR.Heating.Oil.gm.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM norm
VaR.Heating.Oil.gm.95 = c()
for(j in gm_norm){
  VaR.Heating.Oil.gm.95[[j]] = as.data.frame(fit.gm_Heating.Oil[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.05)))
}
VaR.Heating.Oil.gm.95

for(j in gm_norm){
  VaR.Heating.Oil.gm.99[[j]]$`alpha(5%)` <- VaR.Heating.Oil.gm.95[[j]]$V1
}

for(j in gm_norm){
  VaR.Heating.Oil.gm.99[[j]]$realized <- as.double(tail(r_t_est$Heating.Oil, n_oos))
}

# rename VaR.Heating.Oil.gm.99 into VaR.Heating.Oil.gm_norm
VaR.Heating.Oil.gm_norm = VaR.Heating.Oil.gm.99
rm(VaR.Heating.Oil.gm.99, VaR.Heating.Oil.gm.95)


#### VaR for GM models with std dist ####
# vector with shapes for std
shapes <- c(fit.gm_Heating.Oil[["GM-YES-std"]]$rob_coef_mat[7,1],
            fit.gm_Heating.Oil[["GM-NO-std"]]$rob_coef_mat[6,1],
            fit.gm_Heating.Oil[["DAGM-YES-std"]]$rob_coef_mat[9,1],
            fit.gm_Heating.Oil[["DAGM-NO-std"]]$rob_coef_mat[8,1])

# VaR 1% GM std
VaR.Heating.Oil.gm_std.99= c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Heating.Oil.gm_std.99[[j]] = as.data.frame(fit.gm_Heating.Oil[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.01)))
  }
}
VaR.Heating.Oil.gm_std.99

# rename V1 into alpha(1%)
for(j in gm_std){
  names(VaR.Heating.Oil.gm_std.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM std
VaR.Heating.Oil.gm_std.95 = c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Heating.Oil.gm_std.95[[j]] = as.data.frame(fit.gm_Heating.Oil[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.05)))
  }
}
VaR.Heating.Oil.gm_std.95

for(j in gm_std){
  VaR.Heating.Oil.gm_std.99[[j]]$`alpha(5%)` <- VaR.Heating.Oil.gm_std.95[[j]]$V1
}

for(j in gm_std){
  VaR.Heating.Oil.gm_std.99[[j]]$realized <- as.double(tail(r_t_est$Heating.Oil, n_oos))
}

# rename VaR.Heating.Oil.gm_std.99 into VaR.Heating.Oil.gm
VaR.Heating.Oil.gm_std = VaR.Heating.Oil.gm_std.99
rm(VaR.Heating.Oil.gm_std.99, VaR.Heating.Oil.gm_std.95)

# Combine the 2 VaR gm list into 1
VaR.Heating.Oil.gm_std
VaR.Heating.Oil.gm_norm
VaR.Heating.Oil.gm = c(VaR.Heating.Oil.gm_norm, VaR.Heating.Oil.gm_std)
rm(VaR.Heating.Oil.gm_norm, VaR.Heating.Oil.gm_std) # remove obejcts we don't need anymore

# Merging the 2 VaR obeject, rember to extract VaR from ugarchroll
VaR.Heating.Oil = c(VaR.Heating.Oil, VaR.Heating.Oil.gm)
rm(VaR.Heating.Oil.gm)
#####





#### VaR for GM models with normal dist ####                                                             # Propane
# VaR 1% GM norm
VaR.Propane.gm.99 = c()
for(j in gm_norm){
  VaR.Propane.gm.99[[j]] = as.data.frame(fit.gm_Propane[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.01)))
}
VaR.Propane.gm.99

# rename V1 into alpha(1%)
for(j in gm_norm){
  names(VaR.Propane.gm.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM norm
VaR.Propane.gm.95 = c()
for(j in gm_norm){
  VaR.Propane.gm.95[[j]] = as.data.frame(fit.gm_Propane[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.05)))
}
VaR.Propane.gm.95

for(j in gm_norm){
  VaR.Propane.gm.99[[j]]$`alpha(5%)` <- VaR.Propane.gm.95[[j]]$V1
}

for(j in gm_norm){
  VaR.Propane.gm.99[[j]]$realized <- as.double(tail(r_t_est$Propane, n_oos))
}

# rename VaR.Propane.gm.99 into VaR.Propane.gm_norm
VaR.Propane.gm_norm = VaR.Propane.gm.99
rm(VaR.Propane.gm.99, VaR.Propane.gm.95)


#### VaR for GM models with std dist ####
# vector with shapes for std
shapes <- c(fit.gm_Propane[["GM-YES-std"]]$rob_coef_mat[7,1],
            fit.gm_Propane[["GM-NO-std"]]$rob_coef_mat[6,1],
            fit.gm_Propane[["DAGM-YES-std"]]$rob_coef_mat[9,1],
            fit.gm_Propane[["DAGM-NO-std"]]$rob_coef_mat[8,1])

# VaR 1% GM std
VaR.Propane.gm_std.99= c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Propane.gm_std.99[[j]] = as.data.frame(fit.gm_Propane[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.01)))
  }
}
VaR.Propane.gm_std.99

# rename V1 into alpha(1%)
for(j in gm_std){
  names(VaR.Propane.gm_std.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM std
VaR.Propane.gm_std.95 = c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Propane.gm_std.95[[j]] = as.data.frame(fit.gm_Propane[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.05)))
  }
}
VaR.Propane.gm_std.95

for(j in gm_std){
  VaR.Propane.gm_std.99[[j]]$`alpha(5%)` <- VaR.Propane.gm_std.95[[j]]$V1
}

for(j in gm_std){
  VaR.Propane.gm_std.99[[j]]$realized <- as.double(tail(r_t_est$Propane, n_oos))
}

# rename VaR.Propane.gm_std.99 into VaR.Propane.gm
VaR.Propane.gm_std = VaR.Propane.gm_std.99
rm(VaR.Propane.gm_std.99, VaR.Propane.gm_std.95)

# Combine the 2 VaR gm list into 1
VaR.Propane.gm = c(VaR.Propane.gm_norm, VaR.Propane.gm_std)
rm(VaR.Propane.gm_norm, VaR.Propane.gm_std) # remove obejcts we don't need anymore

# Merging the 2 VaR obeject, rember to extract VaR from ugarchroll
VaR.Propane = c(VaR.Propane, VaR.Propane.gm)
rm(VaR.Propane.gm)
#####






#### VaR for GM models with normal dist ####                                                             # Gasoline
# VaR 1% GM norm
VaR.Gasoline.gm.99 = c()
for(j in gm_norm){
  VaR.Gasoline.gm.99[[j]] = as.data.frame(fit.gm_Gasoline[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.01)))
}
VaR.Gasoline.gm.99

# rename V1 into alpha(1%)
for(j in gm_norm){
  names(VaR.Gasoline.gm.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM norm
VaR.Gasoline.gm.95 = c()
for(j in gm_norm){
  VaR.Gasoline.gm.95[[j]] = as.data.frame(fit.gm_Gasoline[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.05)))
}
VaR.Gasoline.gm.95

for(j in gm_norm){
  VaR.Gasoline.gm.99[[j]]$`alpha(5%)` <- VaR.Gasoline.gm.95[[j]]$V1
}

for(j in gm_norm){
  VaR.Gasoline.gm.99[[j]]$realized <- as.double(tail(r_t_est$Gasoline, n_oos))
}

# rename VaR.Gasoline.gm.99 into VaR.Gasoline.gm_norm
VaR.Gasoline.gm_norm = VaR.Gasoline.gm.99
rm(VaR.Gasoline.gm.99, VaR.Gasoline.gm.95)


#### VaR for GM models with std dist ####
# vector with shapes for std
shapes <- c(fit.gm_Gasoline[["GM-YES-std"]]$rob_coef_mat[7,1],
            fit.gm_Gasoline[["GM-NO-std"]]$rob_coef_mat[6,1],
            fit.gm_Gasoline[["DAGM-YES-std"]]$rob_coef_mat[9,1],
            fit.gm_Gasoline[["DAGM-NO-std"]]$rob_coef_mat[8,1])

# VaR 1% GM std
VaR.Gasoline.gm_std.99= c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Gasoline.gm_std.99[[j]] = as.data.frame(fit.gm_Gasoline[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.01)))
  }
}
VaR.Gasoline.gm_std.99

# rename V1 into alpha(1%)
for(j in gm_std){
  names(VaR.Gasoline.gm_std.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM std
VaR.Gasoline.gm_std.95 = c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Gasoline.gm_std.95[[j]] = as.data.frame(fit.gm_Gasoline[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.05)))
  }
}
VaR.Gasoline.gm_std.95

for(j in gm_std){
  VaR.Gasoline.gm_std.99[[j]]$`alpha(5%)` <- VaR.Gasoline.gm_std.95[[j]]$V1
}

for(j in gm_std){
  VaR.Gasoline.gm_std.99[[j]]$realized <- as.double(tail(r_t_est$Gasoline, n_oos))
}

# rename VaR.Gasoline.gm_std.99 into VaR.Gasoline.gm
VaR.Gasoline.gm_std = VaR.Gasoline.gm_std.99
rm(VaR.Gasoline.gm_std.99, VaR.Gasoline.gm_std.95)

# Combine the 2 VaR gm list into 1
VaR.Gasoline.gm = c(VaR.Gasoline.gm_norm, VaR.Gasoline.gm_std)
rm(VaR.Gasoline.gm_norm, VaR.Gasoline.gm_std) # remove obejcts we don't need anymore

# Merging the 2 VaR obeject, rember to extract VaR from ugarchroll
VaR.Gasoline = c(VaR.Gasoline, VaR.Gasoline.gm)
rm(VaR.Gasoline.gm)
#####





#### VaR for GM models with normal dist ####                                                             # Kerosene
# VaR 1% GM norm
VaR.Kerosene.gm.99 = c()
for(j in gm_norm){
  VaR.Kerosene.gm.99[[j]] = as.data.frame(fit.gm_Kerosene[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.01)))
}
VaR.Kerosene.gm.99

# rename V1 into alpha(1%)
for(j in gm_norm){
  names(VaR.Kerosene.gm.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM norm
VaR.Kerosene.gm.95 = c()
for(j in gm_norm){
  VaR.Kerosene.gm.95[[j]] = as.data.frame(fit.gm_Kerosene[[j]]$est_vol_oos * (qdist(distribution = "norm" , p = 0.05)))
}
VaR.Kerosene.gm.95

for(j in gm_norm){
  VaR.Kerosene.gm.99[[j]]$`alpha(5%)` <- VaR.Kerosene.gm.95[[j]]$V1
}

for(j in gm_norm){
  VaR.Kerosene.gm.99[[j]]$realized <- as.double(tail(r_t_est$Kerosene, n_oos))
}

# rename VaR.Kerosene.gm.99 into VaR.Kerosene.gm_norm
VaR.Kerosene.gm_norm = VaR.Kerosene.gm.99
rm(VaR.Kerosene.gm.99, VaR.Kerosene.gm.95)


#### VaR for GM models with std dist ####
# vector with shapes for std
shapes <- c(fit.gm_Kerosene[["GM-YES-std"]]$rob_coef_mat[7,1],
            fit.gm_Kerosene[["GM-NO-std"]]$rob_coef_mat[6,1],
            fit.gm_Kerosene[["DAGM-YES-std"]]$rob_coef_mat[9,1],
            fit.gm_Kerosene[["DAGM-NO-std"]]$rob_coef_mat[8,1])

# VaR 1% GM std
VaR.Kerosene.gm_std.99= c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Kerosene.gm_std.99[[j]] = as.data.frame(fit.gm_Kerosene[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.01)))
  }
}
VaR.Kerosene.gm_std.99

# rename V1 into alpha(1%)
for(j in gm_std){
  names(VaR.Kerosene.gm_std.99[[j]]) = "alpha(1%)"
}

# VaR 5% GM std
VaR.Kerosene.gm_std.95 = c()
for(ss in shapes){
  for(j in gm_std){
    VaR.Kerosene.gm_std.95[[j]] = as.data.frame(fit.gm_Kerosene[[j]]$est_vol_oos * (qdist(distribution = "std", shape = ss, p = 0.05)))
  }
}
VaR.Kerosene.gm_std.95

for(j in gm_std){
  VaR.Kerosene.gm_std.99[[j]]$`alpha(5%)` <- VaR.Kerosene.gm_std.95[[j]]$V1
}

for(j in gm_std){
  VaR.Kerosene.gm_std.99[[j]]$realized <- as.double(tail(r_t_est$Kerosene, n_oos))
}

# rename VaR.Kerosene.gm_std.99 into VaR.Kerosene.gm
VaR.Kerosene.gm_std = VaR.Kerosene.gm_std.99
rm(VaR.Kerosene.gm_std.99, VaR.Kerosene.gm_std.95)

# Combine the 2 VaR gm list into 1
VaR.Kerosene.gm = c(VaR.Kerosene.gm_norm, VaR.Kerosene.gm_std)
rm(VaR.Kerosene.gm_norm, VaR.Kerosene.gm_std) # remove obejcts we don't need anymore

# Merging the 2 VaR obeject, rember to extract VaR from ugarchroll
VaR.Kerosene = c(VaR.Kerosene, VaR.Kerosene.gm)
rm(VaR.Kerosene.gm)
#####

rm(gm_std, gm_norm)
####



##############################
### Backtesting VaR                                                                                  
#############################

spec = c(specifications, specifications2)

################### WTI KUPIEC, CHRISTOFFSEN, DQ PER WTI 5 E 1 CONF.  ################
# install.packages("segMGarch")
library(segMGarch)

# VaR Backtest 99%                                                                                    # WTI
VaR.WTI_backtest.99 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.WTI[[s]]$realized, VaR = VaR.WTI[[s]]$`alpha(1%)`, alpha = .01, conf.level=0.99)
  VaR.WTI_backtest.99[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.WTI_backtest.99) = spec
colnames(VaR.WTI_backtest.99) = names(back)[c(6, 11)]
VaR.WTI_backtest.99

# VaR Backtest 95%
VaR.WTI_backtest.95 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.WTI[[s]]$realized, VaR = VaR.WTI[[s]]$`alpha(5%)`, alpha = .05, conf.level=0.95)
  VaR.WTI_backtest.95[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.WTI_backtest.95) = spec
colnames(VaR.WTI_backtest.95) = names(back)[c(6, 11)]
VaR.WTI_backtest.95


VaR.WTI_back = matrix(data = NA, nrow = length(spec), ncol = 4)
rownames(VaR.WTI_back ) = spec
colnames(VaR.WTI_back) = names(back)[c(6, 11, 6, 11)]

VaR.WTI_back[,1] = VaR.WTI_backtest.99[,1]
VaR.WTI_back[,2] = VaR.WTI_backtest.99[,2]
VaR.WTI_back[,3] = VaR.WTI_backtest.95[,1]
VaR.WTI_back[,4] = VaR.WTI_backtest.95[,2]
stargazer(VaR.WTI_back, summary = F)
xtable(VaR.WTI_back)

############### 
#Report given information about the VaR backtest with VaR 1%                                    # SOLO PER CONFRONTO
rep.WTI<- list()
for(j in spec) {
  rep.WTI[[j]]<- as.data.frame(report(WTI.roll[[j]], type= "VaR", VaR.alpha= 0.01, conf.level=0.99))
}
###############

DQ.WTI.99 = list()
for (s in spec) {
  DQ.WTI.99[[s]]<- DQtest(r_t$WTI, VaR= VaR.WTI[[s]]$`alpha(1%)`, VaR_level= 0.99)
}

DQ.WTI.95 = list()
for (s in spec) {
  DQ.WTI.95[[s]]<- DQtest(r_t$WTI, VaR= VaR.WTI[[s]]$`alpha(1%)`, VaR_level= 0.95)
}


library(GAS)
BacktestVaR(VaR.WTI$`sGARCH-norm`$realized, VaR.WTI$`sGARCH-norm`$`alpha(1%)`, 0.01, Lags = 4)






# VaR Backtest 99%                                                                                    # EUROPE BRENT
VaR.Europe.Brent_backtest.99 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Europe.Brent[[s]]$realized, VaR = VaR.Europe.Brent[[s]]$`alpha(1%)`, alpha = .01, conf.level=0.99)
  VaR.Europe.Brent_backtest.99[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Europe.Brent_backtest.99) = spec
colnames(VaR.Europe.Brent_backtest.99) = names(back)[c(6, 11)]
VaR.Europe.Brent_backtest.99

# VaR Backtest 95%
VaR.Europe.Brent_backtest.95 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Europe.Brent[[s]]$realized, VaR = VaR.Europe.Brent[[s]]$`alpha(5%)`, alpha = .05, conf.level=0.95)
  VaR.Europe.Brent_backtest.95[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Europe.Brent_backtest.95) = spec
colnames(VaR.Europe.Brent_backtest.95) = names(back)[c(6, 11)]
VaR.Europe.Brent_backtest.95


VaR.Europe.Brent_back = matrix(data = NA, nrow = length(spec), ncol = 4)
rownames(VaR.Europe.Brent_back ) = spec
colnames(VaR.Europe.Brent_back) = names(back)[c(6, 11, 6, 11)]

VaR.Europe.Brent_back[,1] = VaR.Europe.Brent_backtest.99[,1]
VaR.Europe.Brent_back[,2] = VaR.Europe.Brent_backtest.99[,2]
VaR.Europe.Brent_back[,3] = VaR.Europe.Brent_backtest.95[,1]
VaR.Europe.Brent_back[,4] = VaR.Europe.Brent_backtest.95[,2]
stargazer(VaR.Europe.Brent_back, summary = F)
xtable(VaR.Europe.Brent_back)



# VaR Backtest 1%                                                                           # Heating.Oil
VaR.Heating.Oil_backtest.99 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Heating.Oil[[s]]$realized, VaR = VaR.Heating.Oil[[s]]$`alpha(1%)`, alpha = .01, conf.level=0.99)
  VaR.Heating.Oil_backtest.99[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Heating.Oil_backtest.99) = spec
colnames(VaR.Heating.Oil_backtest.99) = names(back)[c(6, 11)]
VaR.Heating.Oil_backtest.99

# VaR Backtest 95%
VaR.Heating.Oil_backtest.95 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Heating.Oil[[s]]$realized, VaR = VaR.Heating.Oil[[s]]$`alpha(5%)`, alpha = .05, conf.level=0.95)
  VaR.Heating.Oil_backtest.95[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Heating.Oil_backtest.95) = spec
colnames(VaR.Heating.Oil_backtest.95) = names(back)[c(6, 11)]
VaR.Heating.Oil_backtest.95


VaR.Heating.Oil_back = matrix(data = NA, nrow = length(spec), ncol = 4)
rownames(VaR.Heating.Oil_back ) = spec
colnames(VaR.Heating.Oil_back) = names(back)[c(6, 11, 6, 11)]

VaR.Heating.Oil_back[,1] = VaR.Heating.Oil_backtest.99[,1]
VaR.Heating.Oil_back[,2] = VaR.Heating.Oil_backtest.99[,2]
VaR.Heating.Oil_back[,3] = VaR.Heating.Oil_backtest.95[,1]
VaR.Heating.Oil_back[,4] = VaR.Heating.Oil_backtest.95[,2]
stargazer(VaR.Heating.Oil_back, summary = F)
xtable(VaR.Heating.Oil_back)



# VaR Backtest 1%                                                                            # Propane
VaR.Propane_backtest.99 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Propane[[s]]$realized, VaR = VaR.Propane[[s]]$`alpha(1%)`, alpha = .01, conf.level=0.99)
  VaR.Propane_backtest.99[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Propane_backtest.99) = spec
colnames(VaR.Propane_backtest.99) = names(back)[c(6, 11)]
VaR.Propane_backtest.99

# VaR Backtest 95%
VaR.Propane_backtest.95 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Propane[[s]]$realized, VaR = VaR.Propane[[s]]$`alpha(5%)`, alpha = .05, conf.level=0.95)
  VaR.Propane_backtest.95[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Propane_backtest.95) = spec
colnames(VaR.Propane_backtest.95) = names(back)[c(6, 11)]
VaR.Propane_backtest.95


VaR.Propane_back = matrix(data = NA, nrow = length(spec), ncol = 4)
rownames(VaR.Propane_back ) = spec
colnames(VaR.Propane_back) = names(back)[c(6, 11, 6, 11)]

VaR.Propane_back[,1] = VaR.Propane_backtest.99[,1]
VaR.Propane_back[,2] = VaR.Propane_backtest.99[,2]
VaR.Propane_back[,3] = VaR.Propane_backtest.95[,1]
VaR.Propane_back[,4] = VaR.Propane_backtest.95[,2]
stargazer(VaR.Propane_back, summary = F)
xtable(VaR.Propane_back)




# VaR Backtest 1%                                                                           # Gasoline
VaR.Gasoline_backtest.99 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Gasoline[[s]]$realized, VaR = VaR.Gasoline[[s]]$`alpha(1%)`, alpha = .01, conf.level=0.99)
  VaR.Gasoline_backtest.99[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Gasoline_backtest.99) = spec
colnames(VaR.Gasoline_backtest.99) = names(back)[c(6, 11)]
VaR.Gasoline_backtest.99

# VaR Backtest 95%
VaR.Gasoline_backtest.95 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Gasoline[[s]]$realized, VaR = VaR.Gasoline[[s]]$`alpha(5%)`, alpha = .05, conf.level=0.95)
  VaR.Gasoline_backtest.95[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Gasoline_backtest.95) = spec
colnames(VaR.Gasoline_backtest.95) = names(back)[c(6, 11)]
VaR.Gasoline_backtest.95


VaR.Gasoline_back = matrix(data = NA, nrow = length(spec), ncol = 4)
rownames(VaR.Gasoline_back ) = spec
colnames(VaR.Gasoline_back) = names(back)[c(6, 11, 6, 11)]

VaR.Gasoline_back[,1] = VaR.Gasoline_backtest.99[,1]
VaR.Gasoline_back[,2] = VaR.Gasoline_backtest.99[,2]
VaR.Gasoline_back[,3] = VaR.Gasoline_backtest.95[,1]
VaR.Gasoline_back[,4] = VaR.Gasoline_backtest.95[,2]
stargazer(VaR.Gasoline_back, summary = F)
xtable(VaR.Gasoline_back)




# VaR Backtest 1%                                                                           # Kerosene
VaR.Kerosene_backtest.99 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Kerosene[[s]]$realized, VaR = VaR.Kerosene[[s]]$`alpha(1%)`, alpha = .01, conf.level=0.99)
  VaR.Kerosene_backtest.99[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Kerosene_backtest.99) = spec
colnames(VaR.Kerosene_backtest.99) = names(back)[c(6, 11)]
VaR.Kerosene_backtest.99

# VaR Backtest 95%
VaR.Kerosene_backtest.95 = matrix(NA, length(spec), 2)                                     
for(s in 1:length(spec)) {
  back = VaRTest(actual = VaR.Kerosene[[s]]$realized, VaR = VaR.Kerosene[[s]]$`alpha(5%)`, alpha = .05, conf.level=0.95)
  VaR.Kerosene_backtest.95[s,] = c(back$uc.LRp, back$cc.LRp)
}
rownames(VaR.Kerosene_backtest.95) = spec
colnames(VaR.Kerosene_backtest.95) = names(back)[c(6, 11)]
VaR.Kerosene_backtest.95


VaR.Kerosene_back = matrix(data = NA, nrow = length(spec), ncol = 4)
rownames(VaR.Kerosene_back ) = spec
colnames(VaR.Kerosene_back) = names(back)[c(6, 11, 6, 11)]

VaR.Kerosene_back[,1] = VaR.Kerosene_backtest.99[,1]
VaR.Kerosene_back[,2] = VaR.Kerosene_backtest.99[,2]
VaR.Kerosene_back[,3] = VaR.Kerosene_backtest.95[,1]
VaR.Kerosene_back[,4] = VaR.Kerosene_backtest.95[,2]
stargazer(VaR.Kerosene_back, summary = F)
xtable(VaR.Kerosene_back)





##############################
### LOSS & MCS                                                                                
#############################

###### LOSS WTI #########                                                                           # WTI
# Store di tutti gli alpha (Per LOSS, vedi sotto)
WTI.eval_01 <- list()
for(j in spec){
  WTI.eval_01[[j]] <- VaR.WTI[[j]]$`alpha(1%)`
}


WTI.eval_05 <- list()
for(j in spec){
  WTI.eval_05[[j]] <- VaR.WTI[[j]]$`alpha(5%)`
}

# Loss
WTI.Loss.01 <- do.call(cbind, lapply(spec,
                                     function(s) { LossVaR(tau= 0.01, realized= tail(r_t$WTI, n_oos), # /100
                                                           evaluated= WTI.eval_01[[s]])})) # /100
colnames(WTI.Loss.01) = spec
WTI.Loss.01

WTI.Loss.05<- do.call(cbind, lapply(spec,
                                    function(s) { LossVaR(tau= 0.05, realized= tail(r_t$WTI, n_oos), # /100                         
                                                          evaluated= WTI.eval_05[[s]])})) # /100
colnames(WTI.Loss.05)<- spec                                                                     
WTI.Loss.05

##### MCS PROCEDURES ######                                                                     
SSM.WTI<- MCSprocedure(Loss= WTI.Loss.01, alpha=0.01, B=5000,
                       statistic= "Tmax")
SSM.WTI

stargazer(SSM.WTI@show)
xtable(SSM.WTI@show)


SSM5.WTI<- MCSprocedure(Loss= WTI.Loss.05, alpha=0.05, B=5000,
                        statistic= "Tmax")
stargazer(SSM5.WTI@show)
xtable(SSM5.WTI@show, digits = 3)
######



###### LOSS Europe.Brent #########                                                                           # Europe.Brent
# Store di tutti gli alpha (Per LOSS, vedi sotto)
Europe.Brent.eval_01 <- list()
for(j in spec){
  Europe.Brent.eval_01[[j]] <- VaR.Europe.Brent[[j]]$`alpha(1%)`
}


Europe.Brent.eval_05 <- list()
for(j in spec){
  Europe.Brent.eval_05[[j]] <- VaR.Europe.Brent[[j]]$`alpha(5%)`
}

# Loss
Europe.Brent.Loss.01 <- do.call(cbind, lapply(spec,
                                     function(s) { LossVaR(tau= 0.01, realized= tail(r_t$Europe.Brent, n_oos), # /100
                                                           evaluated= Europe.Brent.eval_01[[s]])})) # /100
colnames(Europe.Brent.Loss.01) = spec
Europe.Brent.Loss.01

Europe.Brent.Loss.05<- do.call(cbind, lapply(spec,
                                    function(s) { LossVaR(tau= 0.05, realized= tail(r_t$Europe.Brent, n_oos), # /100                         
                                                          evaluated= Europe.Brent.eval_05[[s]])})) # /100
colnames(Europe.Brent.Loss.05)<- spec                                                                     
Europe.Brent.Loss.05

##### MCS PROCEDURES ######                                                                     
SSM.Europe.Brent<- MCSprocedure(Loss= Europe.Brent.Loss.01, alpha=0.01, B=5000,
                       statistic= "Tmax")
SSM.Europe.Brent

stargazer(SSM.Europe.Brent@show)
xtable(SSM.Europe.Brent@show, digits = 3)


SSM5.Europe.Brent<- MCSprocedure(Loss= Europe.Brent.Loss.05, alpha=0.05, B=5000,
                        statistic= "Tmax")
stargazer(SSM5.Europe.Brent@show)
xtable(SSM5.Europe.Brent@show, digits = 3)
######



###### LOSS Heating.Oil #########                                                                           # Heating.Oil
# Store di tutti gli alpha (Per LOSS, vedi sotto)
Heating.Oil.eval_01 <- list()
for(j in spec){
  Heating.Oil.eval_01[[j]] <- VaR.Heating.Oil[[j]]$`alpha(1%)`
}


Heating.Oil.eval_05 <- list()
for(j in spec){
  Heating.Oil.eval_05[[j]] <- VaR.Heating.Oil[[j]]$`alpha(5%)`
}

# Loss
Heating.Oil.Loss.01 <- do.call(cbind, lapply(spec,
                                              function(s) { LossVaR(tau= 0.01, realized= tail(r_t$Heating.Oil, n_oos), # /100
                                                                    evaluated= Heating.Oil.eval_01[[s]])})) # /100
colnames(Heating.Oil.Loss.01) = spec
Heating.Oil.Loss.01

Heating.Oil.Loss.05<- do.call(cbind, lapply(spec,
                                             function(s) { LossVaR(tau= 0.05, realized= tail(r_t$Heating.Oil, n_oos), # /100                         
                                                                   evaluated= Heating.Oil.eval_05[[s]])})) # /100
colnames(Heating.Oil.Loss.05)<- spec                                                                     
Heating.Oil.Loss.05

##### MCS PROCEDURES ######                                                                     
SSM.Heating.Oil<- MCSprocedure(Loss= Heating.Oil.Loss.01, alpha=0.01, B=5000,
                                statistic= "Tmax")
SSM.Heating.Oil

stargazer(SSM.Heating.Oil@show)
xtable(SSM.Heating.Oil@show)


SSM5.Heating.Oil<- MCSprocedure(Loss= Heating.Oil.Loss.05, alpha=0.05, B=5000,
                                 statistic= "Tmax")
stargazer(SSM5.Heating.Oil@show)
xtable(SSM5.Heating.Oil@show)
######



###### LOSS Propane #########                                                                           # Propane
# Store di tutti gli alpha (Per LOSS, vedi sotto)
Propane.eval_01 <- list()
for(j in spec){
  Propane.eval_01[[j]] <- VaR.Propane[[j]]$`alpha(1%)`
}


Propane.eval_05 <- list()
for(j in spec){
  Propane.eval_05[[j]] <- VaR.Propane[[j]]$`alpha(5%)`
}

# Loss
Propane.Loss.01 <- do.call(cbind, lapply(spec,
                                             function(s) { LossVaR(tau= 0.01, realized= tail(r_t$Propane, n_oos), # /100
                                                                   evaluated= Propane.eval_01[[s]])})) # /100
colnames(Propane.Loss.01) = spec
Propane.Loss.01

Propane.Loss.05<- do.call(cbind, lapply(spec,
                                            function(s) { LossVaR(tau= 0.05, realized= tail(r_t$Propane, n_oos), # /100                         
                                                                  evaluated= Propane.eval_05[[s]])})) # /100
colnames(Propane.Loss.05)<- spec                                                                     
Propane.Loss.05

##### MCS PROCEDURES ######                                                                     
SSM.Propane<- MCSprocedure(Loss= Propane.Loss.01, alpha=0.01, B=5000,
                               statistic= "Tmax")
SSM.Propane

stargazer(SSM.Propane@show)
xtable(SSM.Propane@show)


SSM5.Propane<- MCSprocedure(Loss= Propane.Loss.05, alpha=0.05, B=5000,
                                statistic= "Tmax")
stargazer(SSM5.Propane@show)
xtable(SSM5.Propane@show)
######



###### LOSS Gasoline #########                                                                           # Gasoline
# Store di tutti gli alpha (Per LOSS, vedi sotto)
Gasoline.eval_01 <- list()
for(j in spec){
  Gasoline.eval_01[[j]] <- VaR.Gasoline[[j]]$`alpha(1%)`
}


Gasoline.eval_05 <- list()
for(j in spec){
  Gasoline.eval_05[[j]] <- VaR.Gasoline[[j]]$`alpha(5%)`
}

# Loss
Gasoline.Loss.01 <- do.call(cbind, lapply(spec,
                                         function(s) { LossVaR(tau= 0.01, realized= tail(r_t$Gasoline, n_oos), # /100
                                                               evaluated= Gasoline.eval_01[[s]])})) # /100
colnames(Gasoline.Loss.01) = spec
Gasoline.Loss.01

Gasoline.Loss.05<- do.call(cbind, lapply(spec,
                                        function(s) { LossVaR(tau= 0.05, realized= tail(r_t$Gasoline, n_oos), # /100                         
                                                              evaluated= Gasoline.eval_05[[s]])})) # /100
colnames(Gasoline.Loss.05)<- spec                                                                     
Gasoline.Loss.05

##### MCS PROCEDURES ######                                                                     
SSM.Gasoline<- MCSprocedure(Loss= Gasoline.Loss.01, alpha=0.01, B=5000,
                           statistic= "Tmax")
SSM.Gasoline

stargazer(SSM.Gasoline@show)
xtable(SSM.Gasoline@show)


SSM5.Gasoline<- MCSprocedure(Loss= Gasoline.Loss.05, alpha=0.05, B=5000,
                            statistic= "Tmax")
stargazer(SSM5.Gasoline@show)
xtable(SSM5.Gasoline@show)
######



###### LOSS Kerosene #########                                                                           # Kerosene
# Store di tutti gli alpha (Per LOSS, vedi sotto)
Kerosene.eval_01 <- list()
for(j in spec){
  Kerosene.eval_01[[j]] <- VaR.Kerosene[[j]]$`alpha(1%)`
}


Kerosene.eval_05 <- list()
for(j in spec){
  Kerosene.eval_05[[j]] <- VaR.Kerosene[[j]]$`alpha(5%)`
}

# Loss
Kerosene.Loss.01 <- do.call(cbind, lapply(spec,
                                          function(s) { LossVaR(tau= 0.01, realized= tail(r_t$Kerosene, n_oos), # /100
                                                                evaluated= Kerosene.eval_01[[s]])})) # /100
colnames(Kerosene.Loss.01) = spec
Kerosene.Loss.01

Kerosene.Loss.05<- do.call(cbind, lapply(spec,
                                         function(s) { LossVaR(tau= 0.05, realized= tail(r_t$Kerosene, n_oos), # /100                         
                                                               evaluated= Kerosene.eval_05[[s]])})) # /100
colnames(Kerosene.Loss.05)<- spec                                                                     
Kerosene.Loss.05

##### MCS PROCEDURES ######                                                                     
SSM.Kerosene<- MCSprocedure(Loss= Kerosene.Loss.01, alpha=0.01, B=5000,
                            statistic= "Tmax")
SSM.Kerosene

stargazer(SSM.Kerosene@show)
xtable(SSM.Kerosene@show)


SSM5.Kerosene<- MCSprocedure(Loss= Kerosene.Loss.05, alpha=0.05, B=5000,
                             statistic= "Tmax")
stargazer(SSM5.Kerosene@show)
xtable(SSM5.Kerosene@show)
######


##### VaR aggregation #####

## VaR Aggregation 99%
#Exponential weights
t.stat<-SSM.WTI@show[,2] 
t.stat
t.stat<- as.numeric(t.stat)
t.stat

#Denominator
dif<- c()
delta_s<- c()
for(s in 1:length(t.stat)) {
  for(j in 1:length(t.stat)) {
    dif[j] <- (t.stat[j] - t.stat[s])
  }
  delta_s[s]<- min(dif)
}
exp.delta_s <- exp(delta_s)
denominator <- sum(exp.delta_s)


#Smallest t-statistic
i.1 <- which.min(t.stat)
i.1

#Numerator
numerator <- c()
for(i in 1:length(t.stat)) {
  numerator[i]<- (t.stat[i.1] - t.stat[i])
}
numerator <- exp(numerator)

weights <- numerator/denominator
weights

sum(weights)  #it should be equal one

#Apply weights to VaR 1%
exp.weight.VaR1<- VaR.WTI[[1]]$`alpha(1%)`*weights[1]+
  VaR.WTI[[2]]$`alpha(1%)`*weights[2]+
  VaR.WTI[[3]]$`alpha(1%)`*weights[3]+
  VaR.WTI[[4]]$`alpha(1%)`*weights[4]+
  VaR.WTI[[5]]$`alpha(1%)`*weights[5]+
  VaR.WTI[[6]]$`alpha(1%)`*weights[6]+
  VaR.WTI[[7]]$`alpha(1%)`*weights[7]+
  VaR.WTI[[8]]$`alpha(1%)`*weights[8]+
  VaR.WTI[[9]]$`alpha(1%)`*weights[9]+
  VaR.WTI[[10]]$`alpha(1%)`*weights[10]+
  VaR.WTI[[11]]$`alpha(1%)`*weights[11]+
  VaR.WTI[[12]]$`alpha(1%)`*weights[12]+
  VaR.WTI[[13]]$`alpha(1%)`*weights[13]+
  VaR.WTI[[14]]$`alpha(1%)`*weights[14]+
  VaR.WTI[[15]]$`alpha(1%)`*weights[15]+
  VaR.WTI[[16]]$`alpha(1%)`*weights[16]+
  VaR.WTI[[17]]$`alpha(1%)`*weights[17]+
  VaR.WTI[[18]]$`alpha(1%)`*weights[18]+
  VaR.WTI[[19]]$`alpha(1%)`*weights[19]+
  VaR.WTI[[20]]$`alpha(1%)`*weights[20]+
  VaR.WTI[[21]]$`alpha(1%)`*weights[21]+
  VaR.WTI[[22]]$`alpha(1%)`*weights[22]+
  VaR.WTI[[23]]$`alpha(1%)`*weights[23]+
  VaR.WTI[[24]]$`alpha(1%)`*weights[24]+
  VaR.WTI[[25]]$`alpha(1%)`*weights[25]+
  VaR.WTI[[26]]$`alpha(1%)`*weights[26]

exp.weight.VaR1
plot.ts(exp.weight.VaR1)

VaRTest(actual = tail(r_t$WTI, n_oos), VaR= exp.weight.VaR1, alpha = 0.01, conf.level = 0.99)
DQtest(tail(r_t$WTI, n_oos), VaR = exp.weight.VaR1, VaR_level = 0.99)
BacktestVaR(tail(r_t$WTI, n_oos), exp.weight.VaR1, 0.01, Lags = 4)


#Uniform weights 99%
weights = rep(1/length(t.stat), length(t.stat))

uniform.VaR1 <- VaR.WTI[[1]]$`alpha(1%)`*weights[1]+
  VaR.WTI[[2]]$`alpha(1%)`*weights[2]+
  VaR.WTI[[3]]$`alpha(1%)`*weights[3]+
  VaR.WTI[[4]]$`alpha(1%)`*weights[4]+
  VaR.WTI[[5]]$`alpha(1%)`*weights[5]+
  VaR.WTI[[6]]$`alpha(1%)`*weights[6]+
  VaR.WTI[[7]]$`alpha(1%)`*weights[7]+
  VaR.WTI[[8]]$`alpha(1%)`*weights[8]+
  VaR.WTI[[9]]$`alpha(1%)`*weights[9]+
  VaR.WTI[[10]]$`alpha(1%)`*weights[10]+
  VaR.WTI[[11]]$`alpha(1%)`*weights[11]+
  VaR.WTI[[12]]$`alpha(1%)`*weights[12]+
  VaR.WTI[[13]]$`alpha(1%)`*weights[13]+
  VaR.WTI[[14]]$`alpha(1%)`*weights[14]+
  VaR.WTI[[15]]$`alpha(1%)`*weights[15]+
  VaR.WTI[[16]]$`alpha(1%)`*weights[16]+
  VaR.WTI[[17]]$`alpha(1%)`*weights[17]+
  VaR.WTI[[18]]$`alpha(1%)`*weights[18]+
  VaR.WTI[[19]]$`alpha(1%)`*weights[19]+
  VaR.WTI[[20]]$`alpha(1%)`*weights[20]+
  VaR.WTI[[21]]$`alpha(1%)`*weights[21]+
  VaR.WTI[[22]]$`alpha(1%)`*weights[22]+
  VaR.WTI[[23]]$`alpha(1%)`*weights[23]+
  VaR.WTI[[24]]$`alpha(1%)`*weights[24]+
  VaR.WTI[[25]]$`alpha(1%)`*weights[25]+
  VaR.WTI[[26]]$`alpha(1%)`*weights[26]

uniform.VaR1
plot.ts(uniform.VaR1)

VaRTest(actual = tail(r_t$WTI, n_oos), VaR= uniform.VaR1, alpha = 0.01, conf.level = 0.99)
DQtest(tail(r_t$WTI, n_oos), VaR = uniform.VaR1, VaR_level = 0.99)
BacktestVaR(tail(r_t$WTI, n_oos), uniform.VaR1, 0.01, Lags = 4)


## VaR Aggregation 95%
#Exponential weights
t.stat<-SSM5.WTI@show[,2] 
t.stat<- as.numeric(t.stat)

#Denominator
dif<- c()
delta_s<- c()
for(s in 1:length(t.stat)) {
  for(j in 1:length(t.stat)) {
    dif[j] <- (t.stat[j] - t.stat[s])
  }
  delta_s[s]<- min(dif)
}
exp.delta_s <- exp(delta_s)
denominator <- sum(exp.delta_s)


#Smallest t-statistic
i.5 <- which.min(t.stat)
i.5

#Numerator
numerator <- c()
for(i in 1:length(t.stat)) {
  numerator[i]<- (t.stat[i.5] - t.stat[i])
}
numerator <- exp(numerator)

weights <- numerator/denominator
weights

sum(weights)  #it should be equal one

#Apply weights to VaR 5%
exp.weight.VaR5<- VaR.WTI[[1]]$`alpha(1%)`*weights[1]+
  VaR.WTI[[2]]$`alpha(5%)`*weights[2]+
  VaR.WTI[[3]]$`alpha(5%)`*weights[3]+
  VaR.WTI[[4]]$`alpha(5%)`*weights[4]+
  VaR.WTI[[5]]$`alpha(5%)`*weights[5]+
  VaR.WTI[[6]]$`alpha(5%)`*weights[6]+
  VaR.WTI[[7]]$`alpha(5%)`*weights[7]+
  VaR.WTI[[8]]$`alpha(5%)`*weights[8]+
  VaR.WTI[[9]]$`alpha(5%)`*weights[9]+
  VaR.WTI[[10]]$`alpha(5%)`*weights[10]+
  VaR.WTI[[11]]$`alpha(5%)`*weights[11]+
  VaR.WTI[[12]]$`alpha(5%)`*weights[12]+
  VaR.WTI[[13]]$`alpha(5%)`*weights[13]+
  VaR.WTI[[14]]$`alpha(5%)`*weights[14]+
  VaR.WTI[[15]]$`alpha(5%)`*weights[15]+
  VaR.WTI[[16]]$`alpha(5%)`*weights[16]+
  VaR.WTI[[17]]$`alpha(5%)`*weights[17]+
  VaR.WTI[[18]]$`alpha(5%)`*weights[18]+
  VaR.WTI[[19]]$`alpha(5%)`*weights[19]+
  VaR.WTI[[20]]$`alpha(5%)`*weights[20]+
  VaR.WTI[[21]]$`alpha(5%)`*weights[21]+
  VaR.WTI[[22]]$`alpha(5%)`*weights[22]+
  VaR.WTI[[23]]$`alpha(5%)`*weights[23]+
  VaR.WTI[[24]]$`alpha(5%)`*weights[24]+
  VaR.WTI[[25]]$`alpha(5%)`*weights[25]

exp.weight.VaR5
plot.ts(exp.weight.VaR5)

VaRTest(actual = tail(r_t$WTI, n_oos), VaR= exp.weight.VaR5, alpha = 0.05, conf.level = 0.95)
DQtest(tail(r_t$WTI, n_oos), VaR = exp.weight.VaR5, VaR_level = 0.95)
BacktestVaR(tail(r_t$WTI, n_oos), exp.weight.VaR5, 0.05, Lags = 4)

#Uniform weights 95%
weights = rep(1/length(t.stat), length(t.stat))

uniform.VaR5 <- VaR.WTI[[1]]$`alpha(1%)`*weights[1]+
  VaR.WTI[[2]]$`alpha(5%)`*weights[2]+
  VaR.WTI[[3]]$`alpha(5%)`*weights[3]+
  VaR.WTI[[4]]$`alpha(5%)`*weights[4]+
  VaR.WTI[[5]]$`alpha(5%)`*weights[5]+
  VaR.WTI[[6]]$`alpha(5%)`*weights[6]+
  VaR.WTI[[7]]$`alpha(5%)`*weights[7]+
  VaR.WTI[[8]]$`alpha(5%)`*weights[8]+
  VaR.WTI[[9]]$`alpha(5%)`*weights[9]+
  VaR.WTI[[10]]$`alpha(5%)`*weights[10]+
  VaR.WTI[[11]]$`alpha(5%)`*weights[11]+
  VaR.WTI[[12]]$`alpha(5%)`*weights[12]+
  VaR.WTI[[13]]$`alpha(5%)`*weights[13]+
  VaR.WTI[[14]]$`alpha(5%)`*weights[14]+
  VaR.WTI[[15]]$`alpha(5%)`*weights[15]+
  VaR.WTI[[16]]$`alpha(5%)`*weights[16]+
  VaR.WTI[[17]]$`alpha(5%)`*weights[17]+
  VaR.WTI[[18]]$`alpha(5%)`*weights[18]+
  VaR.WTI[[19]]$`alpha(5%)`*weights[19]+
  VaR.WTI[[20]]$`alpha(5%)`*weights[20]+
  VaR.WTI[[21]]$`alpha(5%)`*weights[21]+
  VaR.WTI[[22]]$`alpha(5%)`*weights[22]+
  VaR.WTI[[23]]$`alpha(5%)`*weights[23]+
  VaR.WTI[[24]]$`alpha(5%)`*weights[24]+
  VaR.WTI[[25]]$`alpha(5%)`*weights[25]

uniform.VaR5
plot.ts(uniform.VaR5)

VaRTest(actual = tail(r_t$WTI, n_oos), VaR= uniform.VaR5, alpha = 0.05, conf.level = 0.95)
DQtest(tail(r_t$WTI, n_oos), VaR = uniform.VaR5, VaR_level = 0.95)
BacktestVaR(tail(r_t$WTI, n_oos), uniform.VaR5, 0.05, Lags = 4)

####
rm(weights, i.1, i.5, numerator, denominator, dif, delta_s, exp.delta_s, t.stat)
####


###### VaR Violations Grafico
#package ggplot2
library(ggplot2)

realized = VaR.WTI$`sGARCH-norm`$realized

# VaR Violation sGarch norm 1%
qplot(y= VaR.WTI$`sGARCH-norm`$`alpha(1%)`, x= 1:80 , geom= 'line') +
  geom_point(aes(x=1:80, y= tail(r_t$WTI, n_oos), color= as.factor(tail(r_t$WTI, n_oos) < exp.weight.VaR1)), size= 2)+
  scale_color_manual(values= c('gray', 'red')) +
  labs(y= 'Daily Returns', x= 'VaR 0.01 with GARCH snorm')+ theme_light()+
  theme(legend.position = 'none')

# VaR Violation sGarch norm 5%
qplot(y= VaR.WTI$`sGARCH-norm`$`alpha(5%)`, x= 1:80 , geom= 'line') +
  geom_point(aes(x=1:80, y= tail(r_t$WTI, n_oos), color= as.factor(tail(r_t$WTI, n_oos) < exp.weight.VaR5)), size= 2)+
  scale_color_manual(values= c('gray', 'red')) +
  labs(y= 'Daily Returns', x= 'VaR 0.05 with GARCH snorm')+ theme_light()+
  theme(legend.position = 'none')


#### VaR Violation comparison
qplot(y = realized, x = 1:80 , geom = 'point') + geom_point(colour = 'lightgrey' , size = 2) + 
  geom_line(aes(y = exp.weight.VaR1 , x =1:80 ) , colour = 'red') +
  geom_line(aes(y= VaR.WTI$`eGARCH-std`$`alpha(1%)`,x =1:80), colour = "blue")+
  theme_light() + labs(x = '' , y = 'Daily Returns' , title = 'VaR Exp Aggregation vs eGARCH-std')+
  theme(legend.position = 'right')

qplot(y = VaR.WTI$`eGARCH-std`$`alpha(1%)` , x = 1:80 , geom = 'line', color='black') +
  geom_point(aes(x = 1:80 , y = realized, color = as.factor(realized < VaR.WTI$`eGARCH-std`$`alpha(1%)`)) , size = 2)+ 
  geom_line(aes(y=  exp.weight.VaR1, x=1:80, color='blue'))+
  geom_point(aes(x = 1:80 , y = realized, color = as.factor(realized < exp.weight.VaR1)) , size = 2)+
  scale_color_manual(values = c('black','blue', 'grey','red'),
  labels=c("eGARCH-std (1%)","VaR Exp Aggregation","Realized" , "Violations")) + 
  labs(y = 'Daily Returns' , x = 'Year 2021', title = 'VaR 99%: exponential weights aggregation vs rank 1 MCS') + theme_light() + 
  theme(legend.position = "bottom")+
  labs(color = "Legend:")


qplot(y = VaR.WTI$`GM-YES-std`$`alpha(1%)` , x = 1:80 , geom = 'line', color='black') +
  geom_point(aes(x = 1:80 , y = realized, color = as.factor(realized < VaR.WTI$`GM-YES-std`$`alpha(1%)`)) , size = 2)+ 
  geom_line(aes(y=  exp.weight.VaR5, x=1:80, color='blue'))+
  geom_point(aes(x = 1:80 , y = realized, color = as.factor(realized < exp.weight.VaR5)) , size = 2)+
  scale_color_manual(values = c('black','blue', 'grey','red'),
                     labels=c("GM-skew-std","VaR Exp Aggregation","Realized" , "Violations")) + 
  labs(y = 'Daily Returns' , x = 'Year 2021', title = 'VaR 95%: exponential weights aggregation vs rank 1 MCS') + theme_light() + 
  theme(legend.position = "bottom")+
  labs(color = "Legend:")



qplot(y = exp.weight.VaR1 , x = 1:80 , geom = 'line', color='black') +
  geom_point(aes(x = 1:80 , y = realized, color = as.factor(realized < exp.weight.VaR1)) , size = 2)+ 
  geom_line(aes(y=  exp.weight.VaR5, x=1:80, color='blue'))+
  geom_point(aes(x = 1:80 , y = realized, color = as.factor(realized < exp.weight.VaR5)) , size = 2)+
  scale_color_manual(values = c('black','blue', 'grey','red'),
                     labels=c("VaR Exp Aggregation 99%","VaR Exp Aggregation 95%","Realized" , "Violations")) + 
  labs(y = 'Daily Returns' , x = 'Year 2021',
       title = 'VaR Aggregation Exponential Weights for 95% vs 99% confidence level') + theme_light() + 
  theme(legend.position = "bottom")+
  labs(color = "Legend:")







##################################### plot 
end_x<-endpoints(tail(r_t$WTI, n_oos),on="months")
end_x<-end_x+c(rep(1,length(end_x)-1),0)

par(mai=c(1,1,0.5,0.3))

plot(1:80,(tail(r_t$WTI, n_oos)),type="l",
     xaxt="n",ylim=range((tail(r_t$WTI, n_oos)),exp.weight.VaR1),
     xlab="",ylab='VaR Aggregation vs eGARCH-std',
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

points(realized)
lines(1:80,exp.weight.VaR1,col="red",type="l",lwd=2)
lines(1:80,VaR.WTI$`eGARCH-std`$`alpha(1%)`,col="blue",type="l",lwd=2)

legend("topright", c("VaR Exp Aggregation","eGARCH-std (1%)","Realized"),
       lty=1, col=c("red","blue","black"), lwd=2)

axis.timeDate(1, at = tail(r_t$WTI, n_oos)[1], format = "%Y")

axis(side=1, 
     end_x[1:(length(end_x))], 
     c("2021-02-10":"2021-06-04"),cex.axis = 1.1,xpd=T)


grid(NA,NULL)
abline(v=end_x,h=NA,col="gray",lty=3)
#####################################






