# Progetto Models

# Series: CAC 40, IBEX 35 Index, FTSE MIB
# https://realized.oxford-man.ox.ac.uk/data/assets
# assets time frame 01-06-2009 - 31-12-2021
# macro variables: CPI, BOND 10Y, M1, GDP                  ## download from FRED website ##
# macro variables were downloaded starting from 2005-01-01
# All the csv should be located in a subfolder of the Working Directory named "data"
# Working Directory has been named "Project"

setwd("/Users/gianlorenzo/Desktop/FINASS/Models for risk and forecasting/Project")

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

##
library(FinTS)
library(MCS)
###
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(purrr)
library(tidyverse)
library(data.table)
#install.packages("devtools")
library(devtools)
#install_github("kassambara/factoextra")
library("factoextra") #FARE INSTALLARE A GIAN
library(boot) #FARE INSTALLARE A GIAN
#install.packages("HSAUR")
library(HSAUR)
#install_github("sinhrks/ggfortify")
library(ggfortify)
library(corpcor)
library(xtable)

#### importing data
oxf_df <- list.files(path = "./data/", pattern = "*.csv", full.names = TRUE) %>% # Identify all CSV files
  .[ grepl("oxfordmanrealizedvolatilityindices.csv", .) ] %>%   # Exclude csv files
  read.csv()
  #data.table::fread() # più rapido ma bisogna risolvere problema con strptime (fare dopo)

head(oxf_df)
dim(oxf_df)

ftmib<-subset(oxf_df,oxf_df$Symbol==".FTMIB")
ftmib<-ftmib[complete.cases(ftmib),]
dim(ftmib)

ibex <-subset(oxf_df,oxf_df$Symbol==".IBEX")
ibex<-ibex[complete.cases(ibex),]
dim(ibex)

cac40<-subset(oxf_df,oxf_df$Symbol==".FCHI")
cac40<-cac40[complete.cases(cac40),]
dim(cac40)

#### transform data.frame into xts
Date_ftmib<-strptime(ftmib[,1], "%Y-%m-%d",tz="GMT")
ftmib.xts<-as.xts(ftmib[,3:ncol(ftmib)],Date_ftmib)

Date_ibex<-strptime(ibex[,1], "%Y-%m-%d",tz="GMT")
ibex.xts<-as.xts(ibex[,3:ncol(ibex)],Date_ibex)

Date_cac40<-strptime(cac40[,1], "%Y-%m-%d",tz="GMT")
cac40.xts<-as.xts(cac40[,3:ncol(cac40)],Date_cac40)

# adjusting the time frame
range(time(ftmib.xts)) # range "2009-06-01 GMT" "2022-03-18 GMT"
range(time(ibex.xts)) # "2000-01-03 GMT" "2022-03-18 GMT"
ibex.xts<-ibex.xts['2009-06-01/2021-12-31'] # cut the series so it starts from 2009
range(time(cac40.xts)) #r ange "2000-01-03 GMT" "2022-03-18 GMT"
cac40.xts<-cac40.xts['2009-06-01/2021-12-31'] # cut the series so it starts from 2009

# create xts with all prices
asset1 <- ftmib.xts$close_price
names(asset1) <- "ftmib_close_price"
asset2 <- ibex.xts$close_price
names(asset2) <- "ibex_close_price"
asset3 <- cac40.xts$close_price
names(asset3) <- "cac40_close_price"
assets <- merge.xts(asset1, asset2, asset3)
any(is.na(assets))
assets<-assets[complete.cases(assets),]

r_t <- makeReturns(assets)
N<-length(r_t[,1])

# import first group of macros (CPI, BOND and M1)
macro <- list.files(path = "./data/", pattern = "*.csv", full.names = TRUE) %>% # Identify all CSV files
  .[ !grepl("oxfordmanrealizedvolatilityindices.csv", .) ] %>%   # Exclude csv files
  lapply(read_csv) %>%                            # Store all files in list
  reduce(full_join, by = "DATE")                  # Full-join data sets into one data set
macro
newnames <- c("DATE", "BOND.ES", "BOND.FR", "BOND.IT", "M1.EU")
setnames(macro, colnames(macro), new = newnames)
macro = na.omit(macro)
## macro <- macro[, c(1,2,3,7,4,5,6,8)] # leave the row index blank to keep all rows
Date_macro<-strptime(macro$DATE, "%Y-%m-%d",tz="GMT")
macro <-as.xts(macro[,2:ncol(macro)],Date_macro)

a = c("assets", "oxf_df", "ftmib.xts", "ibex.xts", "cac40.xts", "macro", "r_t", "N")
rm(list=setdiff(ls(), a))
########################

########################
#   STYLIZED FACTS     #
########################

# PLOTTING ALL ASSETS
par(mfrow=c(3,1)) 
plot(assets[,1], main="FTMIB Index Close Price")
plot(assets[,2], main="IBEX 35 Index Close Price") #there is a given trend
plot(assets[,3], main="CAC 40 Index Close Price")
#so there is no stationarity

## ACF
par(mfrow=c(3,1))
acf(coredata(assets[,1]))
acf(coredata(assets[,2])) 
acf(coredata(assets[,3]))
#long memory. This means that the asset price can be identified as a Random Walk

## PACF
par(mfrow=c(3,1))
pacf(coredata(assets[,1]))
pacf(coredata(assets[,2]))
pacf(coredata(assets[,3]))

## ADF                                                                                            
adf.test(coredata(assets[,1])) #FTMIB
adf.test(coredata(assets[,2])) #IBEX
adf.test(coredata(assets[,3])) #CAC40
# H_0 = no stationarity.
# p-value > 0.05 -> we do not reject H_0. Close price series is non-stationary    


## PLOTTING RETURNS
par(mfrow=c(3,1))
plot(r_t[,1], main="Returns FTMIB Index")
plot(r_t[,2], main="Returns IBEX 35 Index")
plot(r_t[,3], main="Returns CAC 40 Index")

par(mfrow=c(1,1))
plot(r_t)

## ADF                                                                                            
adf.test(coredata(r_t[,1])) #FTMIB
adf.test(coredata(r_t[,2])) #IBEX
adf.test(coredata(r_t[,3])) #CAC40
# H_0 = no stationarity.

### ACF and PACF Returns:
par(mfrow=c(3,1))
acf(coredata(r_t[,1]))
acf(coredata(r_t[,2]))
acf(coredata(r_t[,3]))

pacf(coredata(r_t[,1]))
pacf(coredata(r_t[,2]))
pacf(coredata(r_t[,3]))
# no structure in the log-returns
# we can see that we have a sort of WN process

## squared returns:
r_t_sq <- r_t^2
plot(r_t_sq, main="All assets squared returns")

par(mfrow=c(3,1))
plot(r_t_sq[,1], main="Squared Returns FTMIB Index")
plot(r_t_sq[,2], main="Squared Returns IBEX 35 Index")
plot(r_t_sq[,3], main="Squared Returns CAC 40 Index")

### ACF and PACF on squared and absolute returns
par(mfrow=c(3,1))
acf(coredata(r_t_sq[,1]))
acf(coredata(r_t_sq[,2]))
acf(coredata(r_t_sq[,3]))

pacf(coredata(r_t_sq[,1]))
pacf(coredata(r_t_sq[,2]))
pacf(coredata(r_t_sq[,3]))
# there is a clear structure of squared returns
#this means we are able to predict future values of r_t_sq (variation of returns)

# absolute returns:
acf(coredata(abs(r_t[,1])))
acf(coredata(abs(r_t[,2])))
acf(coredata(abs(r_t[,3])))

# there is autocorrelation

# boxplot
par(mfrow=c(1,1))
boxplot(coredata(r_t),col="yellow",main="all assets")

########################################################################

#density distribution of the returns
ggplot(assets[,1])+
  geom_density(aes(x= r_t[,1]), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(r_t[,1]),
                            sd = sd(r_t[,1])))+
  labs( y = "Density", x = "Returns", title = "FTMIB Index returns and Normal distribution")

#density distribution of the returns
ggplot(assets[,2])+
  geom_density(aes(x= r_t[,2]), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(r_t[,2]),
                            sd = sd(r_t[,2])))+
  labs( y = "Density", x = "Returns", title = "IBEX 35 Index returns and Normal distribution")

#density distribution of the returns
ggplot(assets[,3])+
  geom_density(aes(x= r_t[,3]), fill = 'darkorange', color = 'black', alpha =.6)+
  theme_minimal()+
  stat_function(fun = dnorm, colour='blue', size=1,
                args = list(mean=mean(r_t[,3]),
                            sd = sd(r_t[,3])))+
  labs( y = "Density", x = "Returns", title = "CAC40 Index returns and Normal distribution")

########################################################################

# hist(r_t, freq = FALSE,ylim=c(0,18),breaks=40)
# curve(dnorm(x, mean=mean(r_t), sd=sd(r_t)), add=TRUE,col=2)

###### FTMIB
qqnorm(r_t[,1])
qqline(r_t[,1], col = 2) # huge deviation from the Normal distribution by both left and right tail

skewness(r_t[,1]) # S < 0, then negative returns are more common than positive ones
kurtosis(r_t[,1]) # K-3 > 0 then ret have heavier tails than a normal distribution
# EXCESS of Kurtosis (so kurtosis -3)

jarque.bera.test(r_t[,1]) # H_0 : Normality. p-value < 0.05 we reject H_0

#test correlation (White Noise test) H_0 : No correlation
Box.test(r_t[,1],type="Ljung-Box",lag=20)
Box.test(r_t[,1],type="Box-Pierce",lag=20)
# we do not reject the null hypothesis
# We do not have presence of autocorrelation

Box.test(r_t_sq[,1],type="Ljung-Box",lag=20) #p-value < 2.2e-16
Box.test(r_t_sq[,1],type="Box-Pierce",lag=20) #p-value < 2.2e-16
# we do reject the null hypothesis
# We do have presence of autocorrelation

#Heteroscedasticity test H_0 : No heteroscedasticity
ArchTest(r_t[,1], lag = 30) # p-value < 0.05 we reject H_0
# We have an  ARCH effect, meaning that there is presence of heteroskedasticity

#test stazionarity of the distribution.  H_0 = no stationarity
adf.test(r_t[,1]) # p-value < 0.05 we reject H_0. Returns series is stationary

#significance of mean different from zero test. H_0 : true mean is equal zero
t.test(r_t[,1]) #p-value > 0.05 we do not reject H_O

#Long Memory Test. 0 < H < 0.5 anti-persistent series. H = 0.5 RW series. 0.5 < H < 1 persistent series
Qu.test(r_t[,1], 3180, epsilon = 0.05)
hurstexp(r_t[,1], d = 50, display = TRUE) # Simple R/S Hurst estimation < 0.5. Series do not show memory persistence

Qu.test(r_t_sq[,1], 3180, epsilon = 0.05)
hurstexp(r_t_sq[,1], d = 50, display = TRUE) # Simple R/S Hurst estimation > 0.5. Series slightly show memory persistence


###### IBEX
qqnorm(r_t[,2])
qqline(r_t[,2], col = 2) # huge deviation from the Normal distribution by both left and right tail

skewness(r_t[,2]) # S < 0, then negative returns are more common than positive ones
kurtosis(r_t[,2]) # K-3 > 0 then ret have heavier tails than a normal distribution
# EXCESS of Kurtosis (so kurtosis -3)

jarque.bera.test(r_t[,2]) # H_0 : Normality. p-value < 0.05 we reject H_0

#test correlation (White Noise test) H_0 : No correlation
Box.test(r_t[,2],type="Ljung-Box",lag=20)
Box.test(r_t[,2],type="Box-Pierce",lag=20)
# we do not reject the null hypothesis
# We do not have presence of autocorrelation

Box.test(r_t_sq[,2],type="Ljung-Box",lag=20) #p-value < 2.2e-16
Box.test(r_t_sq[,2],type="Box-Pierce",lag=20) #p-value < 2.2e-16
# we do reject the null hypothesis
# We do have presence of autocorrelation

#Heteroscedasticity test H_0 : No heteroscedasticity
ArchTest(r_t[,2], lag = 30) # p-value < 0.05 we reject H_0
# We have an  ARCH effect, meaning that there is presence of heteroskedasticity

#test stazionarity of the distribution.  H_0 = no stationarity
adf.test(r_t[,2]) # p-value < 0.05 we reject H_0. Returns series is stationary

#significance of mean different from zero test. H_0 : true mean is equal zero
t.test(r_t[,2]) #p-value > 0.05 we do not reject H_O

#Long Memory Test. 0 < H < 0.5 anti-persistent series. H = 0.5 RW series. 0.5 < H < 1 persistent series
Qu.test(r_t[,2], 3180, epsilon = 0.05)
hurstexp(r_t[,2], d = 50, display = TRUE) # Simple R/S Hurst estimation < 0.5. Series do not show memory persistence

Qu.test(r_t_sq[,2], 3180, epsilon = 0.05)
hurstexp(r_t_sq[,2], d = 50, display = TRUE) # Simple R/S Hurst estimation > 0.5. Series slightly show memory persistence


###### CAC40
qqnorm(r_t[,3])
qqline(r_t[,3], col = 2) # huge deviation from the Normal distribution by both left and right tail

skewness(r_t[,3]) # S < 0, then negative returns are more common than positive ones
kurtosis(r_t[,3]) # K-3 > 0 then ret have heavier tails than a normal distribution
# EXCESS of Kurtosis (so kurtosis -3)

jarque.bera.test(r_t[,3]) # H_0 : Normality. p-value < 0.05 we reject H_0

#test correlation (White Noise test) H_0 : No correlation
Box.test(r_t[,3],type="Ljung-Box",lag=20)
Box.test(r_t[,3],type="Box-Pierce",lag=20)
# we do not reject the null hypothesis
# We do not have presence of autocorrelation

Box.test(r_t_sq[,3],type="Ljung-Box",lag=20) #p-value < 2.2e-16
Box.test(r_t_sq[,3],type="Box-Pierce",lag=20) #p-value < 2.2e-16
# we do reject the null hypothesis
# We do have presence of autocorrelation

#Heteroscedasticity test H_0 : No heteroscedasticity
ArchTest(r_t[,3], lag = 30) # p-value < 0.05 we reject H_0
# We have an  ARCH effect, meaning that there is presence of heteroskedasticity

#test stazionarity of the distribution.  H_0 = no stationarity
adf.test(r_t[,3]) # p-value < 0.05 we reject H_0. Returns series is stationary

#significance of mean different from zero test. H_0 : true mean is equal zero
t.test(r_t[,3]) #p-value > 0.05 we do not reject H_O

#Long Memory Test. 0 < H < 0.5 anti-persistent series. H = 0.5 RW series. 0.5 < H < 1 persistent series
Qu.test(r_t[,3], 3180, epsilon = 0.05)
hurstexp(r_t[,3], d = 50, display = TRUE) # Simple R/S Hurst estimation < 0.5. Series do not show memory persistence

Qu.test(r_t_sq[,3], 3180, epsilon = 0.05)
hurstexp(r_t_sq[,3], d = 50, display = TRUE) # Simple R/S Hurst estimation > 0.5. Series slightly show memory persistence





############################## 
#     MULTIVARIATE MODELS    #
############################## 
# Non parametric: MC, EWMA
# Parametric: BEKK, SBEKK, OGARCH, DCC-MIDAS
## VEC and DVEC --> NO SU R 
##########

library(dccmidas)

# create a list with all returns
ret <- list(makeReturns(assets$ftmib_close_price), # asset1 = FTMIB
            makeReturns(assets$ibex_close_price),  # asset2 = IBEX
            makeReturns(assets$cac40_close_price)  # asset3 = CAC
)
any(is.na(ret))

#### compute the MC matrix for these 3 assets
# using V=22, V=50, V=50, V = 252
# check main diagonal of MV_22$H_t if it is positive definite
MV_22<-moving_cov(ret)
MV_252<-moving_cov(ret,V=252)

#### compute the H_t matrix using the EWMA model
#### for lambda=0.94 and lambda=0.97
RM_0.94<-riskmetrics_mat(ret,lambda=0.94)
RM_0.97<-riskmetrics_mat(ret,lambda=0.97)

RM_0.94$H_t[,,100] #  positive diagonal


#################################
############ OGARCH #############
#################################

k<-ncol(r_t)
TT<-nrow(r_t)


SD<-matrix(apply(r_t,2,sd),ncol=k,nrow=TT,byrow=T)

X<-(r_t)/SD # matrix of standardized returns (to reduce variability in the data)
head(X)
round(cor(X), 2) # assets are highly correlated (there is redundancy in the data) 
round(cov(X), 2)

# Performing Principal Component Analysis
PCA<-prcomp(r_t, scale = TRUE) # PCA on standardized returns

summary(PCA) # proportion of data variability explained by each principal component
fviz_eig(PCA, addlabels = TRUE, ylim = c(0, 100))
biplot(PCA)
ggplot2::autoplot(PCA, label = FALSE, loadings.label = TRUE)

round(cor(PCA$x), 4) #PC are uncorrelated so orthogonal

## store the factor loadings (k x k matrix whose columns are the singular vectors)
W<-PCA$rotation # coefficients of the linear combinations of the variables to get principal components

PC<- matrix(NA,ncol=k,nrow=TT)

for(i in 1:TT){
  PC[i,]<-(as.matrix(X[i,]))%*%W
} # multiplying matrix X by matrix W of factor loadings to get Principal Components

PC # matrix of Principal Components

# standardization of principal components (because we performed standardization at
# the beginning and data were rescaled, but we want results on the real scale of our data)
SD_PC<-matrix(apply(PC,2,sd),ncol=k,nrow=TT,byrow=T)

PC_stand<-PC/SD_PC

# estimating univariate GARCH on each standardized PC

########## sGARCH-norm

o_spec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                   mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                   distribution.model="norm")

o_fit_g<-list()
o_sigma_sq<-matrix(NA,ncol=k,nrow=TT)

for(i in 1:k){
  o_fit_g[[i]]<-ugarchfit(data=PC_stand[,i],o_spec)
  o_sigma_sq[,i]<-o_fit_g[[i]]@fit$sigma^2
}
########################

## rebuild the conditional covariance matrix of returns

o_H_t<-array(NA,dim=c(k,k,TT))

for(i in 1:TT){
  o_H_t[,,i]<-diag(SD[1,])%*%W%*%diag(SD_PC[1,])%*%diag(c(o_sigma_sq[i,]))%*%diag(SD_PC[1,])%*%t(W)%*%diag(SD[1,])
}


############### gjrGARCH-norm 
o_spec2 <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                      mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                      distribution.model="norm")

o_fit_g2<-list()
o_sigma_sq2<-matrix(NA,ncol=k,nrow=TT)

for(i in 1:k){
  o_fit_g2[[i]]<-ugarchfit(data=PC_stand[,i],o_spec2)
  o_sigma_sq2[,i]<-o_fit_g2[[i]]@fit$sigma^2
}


o_H_t2<-array(NA,dim=c(k,k,TT))

for(i in 1:TT){
  o_H_t2[,,i]<-diag(SD[1,])%*%W%*%diag(SD_PC[1,])%*%diag(c(o_sigma_sq2[i,]))%*%diag(SD_PC[1,])%*%t(W)%*%diag(SD[1,])
}

############# sGARCH-std
o_spec3 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                      mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                      distribution.model="std")

o_fit_g3<-list()
o_sigma_sq3<-matrix(NA,ncol=k,nrow=TT)

for(i in 1:k){
  o_fit_g3[[i]]<-ugarchfit(data=PC_stand[,i],o_spec3)
  o_sigma_sq3[,i]<-o_fit_g3[[i]]@fit$sigma^2
}


o_H_t3<-array(NA,dim=c(k,k,TT))

for(i in 1:TT){
  o_H_t3[,,i]<-diag(SD[1,])%*%W%*%diag(SD_PC[1,])%*%diag(c(o_sigma_sq3[i,]))%*%diag(SD_PC[1,])%*%t(W)%*%diag(SD[1,])
}


############# gjrGARCH-std
o_spec4 <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                      mean.model=list(armaOrder=c(0,0), include.mean=FALSE),  
                      distribution.model="std")

o_fit_g4<-list()
o_sigma_sq4<-matrix(NA,ncol=k,nrow=TT)

for(i in 1:k){
  o_fit_g4[[i]]<-ugarchfit(data=PC_stand[,i],o_spec4)
  o_sigma_sq4[,i]<-o_fit_g4[[i]]@fit$sigma^2
}


o_H_t4<-array(NA,dim=c(k,k,TT))

for(i in 1:TT){
  o_H_t4[,,i]<-diag(SD[1,])%*%W%*%diag(SD_PC[1,])%*%diag(c(o_sigma_sq4[i,]))%*%diag(SD_PC[1,])%*%t(W)%*%diag(SD[1,])
}

########################################
############# END OGARCH ###############
########################################


############ SBEKK MODEL ############
#### compute the H_t matrix using the sBEKK model
sbekk_est<-bekk_fit(ret,model="sBEKK")
sbekk_est$mat_coef
sbekk_est$est_time
sbekk_est$llk

dim(sbekk_est$H_t)


############ DBEKK MODEL ############
#### compute the H_t matrix using the dBEKK model
dbekk_est<-bekk_fit(ret,model="dBEKK")
dbekk_est$mat_coef
dbekk_est$est_time
dbekk_est$llk

# faster alternative: load the enviroment
load("dbekk.RData") # enviroment dbekk


############ MODELLI DCC ############
# the models coming from rugarch are: 'sGARCH', 'eGARCH', 'gjrGARCH', 'iGARCH', and 'csGARCH'.
# The models implemented from rumidas are: 'GM_skew','GM_noskew', 'DAGM_skew', and 'DAGM_noskew'
# Distributions: "norm" (by default) and "std"
# corr_model: "cDCC" (the corrected DCC of Aielli (2013)), "aDCC" (the asymmetric DCC model of Cappiello et al. (2006)), "DECO" (Dynamic equicorrelation of Engle and Kelly (2012)), and "DCCMIDAS" (the DCC-MIDAS of Colacito et al. (2011)). By detault, it is "cDCC"


## FITTING for rugarch models as univariate specification
fit_cdcc <-list()
fit_dccmidas <- list()
models<- c("sGARCH","gjrGARCH")
distributions<-c("norm","std")
c_models <- c("cDCC", "DCCMIDAS") # "aDCC","DECO",

K_c <- 36
N_c <- 144

m<-c()
d<-c()
for(m in models) {
  for(d in distributions) {
    for(c in c_models) {
      if(c != "DCCMIDAS") {
      fit_cdcc[[paste(m, d, c ,sep="-")]] <- dcc_fit(ret,univ_model= m ,distribution= d,
                                                    corr_model= c) # no out of sample
      } else {
        fit_dccmidas[[paste(m, d, c ,sep="-")]] <- dcc_fit(ret,univ_model= m ,distribution= d,
                                                      corr_model= c,N_c=N_c, K_c=K_c) # no out of sample
      }
    }
  }
}
    
# Fit Diagnostic
names(fit_cdcc)
fit_cdcc
summary.dccmidas(fit_cdcc$`sGARCH-norm-cDCC`)
summary.dccmidas(fit_cdcc$`sGARCH-std-cDCC`)
summary.dccmidas(fit_cdcc$`gjrGARCH-norm-cDCC`)
summary.dccmidas(fit_cdcc$`gjrGARCH-std-cDCC`)

summary.dccmidas(fit_dccmidas$`sGARCH-norm-DCCMIDAS`)
summary.dccmidas(fit_dccmidas$`sGARCH-std-DCCMIDAS`)
summary.dccmidas(fit_dccmidas$`gjrGARCH-norm-DCCMIDAS`)
summary.dccmidas(fit_dccmidas$`gjrGARCH-std-DCCMIDAS`)


pdf("cDCC.pdf",height=9,width=16)
plot_dccmidas(fit_cdcc$`sGARCH-norm-cDCC`)
dev.off()

pdf("DCC-MIDAS.pdf",height=9,width=16)
plot_dccmidas(fit_dccmidas$`sGARCH-norm-DCCMIDAS`, K_c=K_c)
dev.off()

names(fit_dccmidas)
fit_dccmidas
summary.dccmidas(fit_dccmidas$`sGARCH-norm-DCCMIDAS`)


## FITTING for rumidas models as univariate specification
difftime(strptime("2009-06-01", format = "%Y-%m-%d"), strptime("2005-01-01", format = "%Y-%m-%d"), units="weeks")

###### MACRO - ADF TEST
adf.test(macro$BOND.IT)
adf.test(macro$BOND.ES)
adf.test(macro$BOND.FR)
adf.test(macro$M1.EU)

######
macro_diff = diff(macro)
macro_diff = na.omit(macro_diff)

adf.test(macro_diff$BOND.IT)
adf.test(macro_diff$BOND.ES)
adf.test(macro_diff$BOND.FR)

############# Seasonal Adjustment ################                # SI PUO' FARE ANCHE SENZA - DECIDERE DOPO SE TENERLO
# BOND ES
plot(macro$BOND.ES)
ts_bond.es = ts(macro$BOND.ES, frequency = 12, start = 2005)

decompose_bond.es = decompose(ts_bond.es, "additive")
adjust_bond.es = ts_bond.es - decompose_bond.es$seasonal
plot(adjust_bond.es)

adf.test(adjust_bond.es)
adf.test(diff(adjust_bond.es))

# BOND FR
plot(macro$BOND.FR)
ts_bond.fr = ts(macro$BOND.FR, frequency = 12, start = 2005)

decompose_bond.fr = decompose(ts_bond.fr, "additive")
adjust_bond.fr = ts_bond.fr - decompose_bond.fr$seasonal
plot(adjust_bond.fr)

adf.test(adjust_bond.fr)
adf.test(diff(adjust_bond.fr))

# BOND IT
plot(macro$BOND.IT)
ts_bond.it = ts(macro$BOND.IT, frequency = 12, start = 2005)

decompose_bond.it = decompose(ts_bond.it, "additive")
adjust_bond.it = ts_bond.it - decompose_bond.it$seasonal
plot(adjust_bond.it)

adf.test(adjust_bond.it)
adf.test(diff(adjust_bond.it))
##################################################

# asset1 = FTMIB
# asset2 = IBEX
# asset3 = CAC

# MV transformation (same MV for all the stocks)
require(rumidas)

K = 48   # max 52
mv_m1 <- mv_into_mat(ret[[1]], macro_diff$BOND.IT ,K=K,"monthly")
mv_m2 <- mv_into_mat(ret[[2]], macro_diff$BOND.ES ,K=K,"monthly")
mv_m3 <- mv_into_mat(ret[[3]], macro_diff$BOND.FR ,K=K,"monthly")

# list of MV
MV <-list(mv_m1,mv_m2,mv_m3)

# estimation
fit_cdcc.gm <-list()
fit_dccmidas.gm <- list()
models<- c('GM_skew','GM_noskew', 'DAGM_skew', 'DAGM_noskew')
distributions<-c("norm","std")
c_models <- c("cDCC", "DCCMIDAS") # "aDCC",

K_c<-36 # K_c lagged realizations of a particular matrix C_t
N_c<-144 # N_c crossproducts of the standardized residuals from the previous steps

m<-c()
d<-c()
c<-c()
for(m in models) {
  for(d in distributions) {
    for(c in c_models) {
      if(c != "DCCMIDAS") {
        fit_cdcc.gm[[paste(m, d, c ,sep="-")]] <- dcc_fit(ret,univ_model= m ,distribution= d,
                                                      MV=MV, K=K, corr_model= c) # no out of sample
      } else {
        fit_dccmidas.gm[[paste(m, d, c ,sep="-")]] <- dcc_fit(ret,univ_model= m ,distribution= d,
                                                           MV=MV,K=K, corr_model= c,
                                                           N_c=N_c, K_c=K_c) # no out of sample
      }
    }
  }
}


# Fit Diagnostic
names(fit_cdcc.gm)
fit_cdcc.gm

names(fit_dccmidas.gm)
fit_dccmidas.gm

plot_dccmidas(fit_dccmidas.gm$`GM_noskew-norm-DCCMIDAS`)

summary.dccmidas(fit_cdcc.gm$`GM_skew-norm-cDCC`)
summary.dccmidas(fit_cdcc.gm$`GM_skew-std-cDCC`)
summary.dccmidas(fit_cdcc.gm$`GM_noskew-norm-cDCC`)
summary.dccmidas(fit_cdcc.gm$`GM_noskew-std-cDCC`)
summary.dccmidas(fit_cdcc.gm$`DAGM_skew-norm-cDCC`)
summary.dccmidas(fit_cdcc.gm$`DAGM_skew-std-cDCC`)
summary.dccmidas(fit_cdcc.gm$`DAGM_noskew-norm-cDCC`)
summary.dccmidas(fit_cdcc.gm$`DAGM_noskew-std-cDCC`)

summary.dccmidas(fit_dccmidas.gm$`GM_skew-norm-DCCMIDAS`)
summary.dccmidas(fit_dccmidas.gm$`GM_skew-std-DCCMIDAS`)
summary.dccmidas(fit_dccmidas.gm$`GM_noskew-norm-DCCMIDAS`)
summary.dccmidas(fit_dccmidas.gm$`GM_noskew-std-DCCMIDAS`)
summary.dccmidas(fit_dccmidas.gm$`DAGM_skew-norm-DCCMIDAS`)
summary.dccmidas(fit_dccmidas.gm$`DAGM_skew-std-DCCMIDAS`)
summary.dccmidas(fit_dccmidas.gm$`DAGM_noskew-norm-DCCMIDAS`)
summary.dccmidas(fit_dccmidas.gm$`DAGM_noskew-std-DCCMIDAS`)

################
## create list of H_t for all models
M = list()

M$`MV_22`<- MV_22$H_t
M$`MV_252`<-MV_252$H_t
M$`EWMA_94`<-RM_0.94$H_t
M$`EWMA_97`<-RM_0.97$H_t
M$`OGARCH-sGARCH-norm`<-o_H_t
M$`OGARCH-gjrGARCH-norm`<-o_H_t2
M$`OGARCH-sGARCH-std`<-o_H_t3
M$`OGARCH-gjrGARCH-std`<-o_H_t4
M$`SBEKK`<-sbekk_est$H_t
M$`DBEKK`<-dbekk_est$H_t

for(kk in names(fit_dccmidas.gm)){
  M[[kk]] <- fit_dccmidas.gm[[kk]]$H_t
}

for(kk in names(fit_dccmidas)){
  M[[kk]] <- fit_dccmidas[[kk]]$H_t
}

for(kk in names(fit_cdcc)){
  M[[kk]] <- fit_cdcc[[kk]]$H_t
}

for(kk in names(fit_cdcc.gm)){
  M[[kk]] <- fit_cdcc.gm[[kk]]$H_t
}
################
res = list()
for(kk in names(M)){
  res[[kk]] = apply(M[[kk]]$H_t, 3, is.positive.definite)
}
rm(res)



#### LOSS AND MCS ####

# QLIKE LOSS FUNCTION
?cov_eval
loss_p1 = cov_eval(M$MV_22, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p2 = cov_eval(M$MV_252, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p3 = cov_eval(M$EWMA_94, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p4 = cov_eval(M$EWMA_97, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p5 = cov_eval(M$`OGARCH-sGARCH-norm`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p6 = cov_eval(M$`OGARCH-gjrGARCH-norm`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p7 = cov_eval(M$`OGARCH-sGARCH-std`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p8 = cov_eval(M$`OGARCH-gjrGARCH-std`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p9 = cov_eval(M$SBEKK, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p10 = cov_eval(M$DBEKK, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p11 = cov_eval(M$`GM_skew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p12 = cov_eval(M$`GM_skew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p13 = cov_eval(M$`GM_noskew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p14 = cov_eval(M$`GM_noskew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p15 = cov_eval(M$`DAGM_skew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p16 = cov_eval(M$`DAGM_skew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p17 = cov_eval(M$`DAGM_noskew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p18 = cov_eval(M$`DAGM_noskew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p19 = cov_eval(M$`sGARCH-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p20 = cov_eval(M$`sGARCH-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p21 = cov_eval(M$`gjrGARCH-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p22 = cov_eval(M$`gjrGARCH-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p23 = cov_eval(M$`sGARCH-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p24 = cov_eval(M$`sGARCH-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p25 = cov_eval(M$`gjrGARCH-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p26 = cov_eval(M$ `gjrGARCH-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p27 = cov_eval(M$`GM_skew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p28 = cov_eval(M$`GM_skew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p29 = cov_eval(M$`GM_noskew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p30 = cov_eval(M$`GM_noskew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p31 = cov_eval(M$`DAGM_skew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p32 = cov_eval(M$`DAGM_skew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p33 = cov_eval(M$`DAGM_noskew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")
loss_p34 = cov_eval(M$`DAGM_noskew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "QLIKE")


loss.mat = matrix(0,nrow=c(N),ncol=34) #ncol = number of models/portfolios
loss.mat[,1] = loss_p1
loss.mat[,2] = loss_p2
loss.mat[,3] = loss_p3
loss.mat[,4] = loss_p4
loss.mat[,5] = loss_p5
loss.mat[,6] = loss_p6
loss.mat[,7] = loss_p7
loss.mat[,8] = loss_p8
loss.mat[,9] = loss_p9
loss.mat[,10] = loss_p10
loss.mat[,11] = loss_p11
loss.mat[,12] = loss_p12
loss.mat[,13] = loss_p13
loss.mat[,14] = loss_p14
loss.mat[,15] = loss_p15
loss.mat[,16] = loss_p16
loss.mat[,17] = loss_p17
loss.mat[,18] = loss_p18
loss.mat[,19] = loss_p19
loss.mat[,20] = loss_p20
loss.mat[,21] = loss_p21
loss.mat[,22] = loss_p22
loss.mat[,23] = loss_p23
loss.mat[,24] = loss_p24
loss.mat[,25] = loss_p25
loss.mat[,26] = loss_p26
loss.mat[,27] = loss_p27
loss.mat[,28] = loss_p28
loss.mat[,29] = loss_p29
loss.mat[,30] = loss_p30
loss.mat[,31] = loss_p31
loss.mat[,32] = loss_p32
loss.mat[,33] = loss_p33
loss.mat[,34] = loss_p34


mcsTest(loss.mat,  alpha = 0.25, nboot = 100, nblock = 1, boot = "stationary")

SSM.qlike <- MCSprocedure(Loss = loss.mat, alpha=0.25, B=5000,
                           statistic= "Tmax")

xtable(SSM.qlike@show)
stargazer(SSM.qlike@show)

# RMSE LOSS FUNCTION
loss_p1 = cov_eval(M$MV_22, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p2 = cov_eval(M$MV_252, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p3 = cov_eval(M$EWMA_94, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p4 = cov_eval(M$EWMA_97, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p5 = cov_eval(M$`OGARCH-sGARCH-norm`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p6 = cov_eval(M$`OGARCH-gjrGARCH-norm`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p7 = cov_eval(M$`OGARCH-sGARCH-std`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p8 = cov_eval(M$`OGARCH-gjrGARCH-std`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p9 = cov_eval(M$SBEKK, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p10 = cov_eval(M$DBEKK, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p11 = cov_eval(M$`GM_skew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p12 = cov_eval(M$`GM_skew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p13 = cov_eval(M$`GM_noskew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p14 = cov_eval(M$`GM_noskew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p15 = cov_eval(M$`DAGM_skew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p16 = cov_eval(M$`DAGM_skew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p17 = cov_eval(M$`DAGM_noskew-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p18 = cov_eval(M$`DAGM_noskew-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p19 = cov_eval(M$`sGARCH-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p20 = cov_eval(M$`sGARCH-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p21 = cov_eval(M$`gjrGARCH-norm-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p22 = cov_eval(M$`gjrGARCH-std-DCCMIDAS`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p23 = cov_eval(M$`sGARCH-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p24 = cov_eval(M$`sGARCH-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p25 = cov_eval(M$`gjrGARCH-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p26 = cov_eval(M$`gjrGARCH-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p27 = cov_eval(M$`GM_skew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p28 = cov_eval(M$`GM_skew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p29 = cov_eval(M$`GM_noskew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p30 = cov_eval(M$`GM_noskew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p31 = cov_eval(M$`DAGM_skew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p32 = cov_eval(M$`DAGM_skew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p33 = cov_eval(M$`DAGM_noskew-norm-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")
loss_p34 = cov_eval(M$`DAGM_noskew-std-cDCC`, cov_proxy = NULL, r_t = ret, loss = "RMSE")


loss.mat = matrix(0,nrow=c(N),ncol=34) #ncol = number of models/portfolios
loss.mat[,1] = loss_p1
loss.mat[,2] = loss_p2
loss.mat[,3] = loss_p3
loss.mat[,4] = loss_p4
loss.mat[,5] = loss_p5
loss.mat[,6] = loss_p6
loss.mat[,7] = loss_p7
loss.mat[,8] = loss_p8
loss.mat[,9] = loss_p9
loss.mat[,10] = loss_p10
loss.mat[,11] = loss_p11
loss.mat[,12] = loss_p12
loss.mat[,13] = loss_p13
loss.mat[,14] = loss_p14
loss.mat[,15] = loss_p15
loss.mat[,16] = loss_p16
loss.mat[,17] = loss_p17
loss.mat[,18] = loss_p18
loss.mat[,19] = loss_p19
loss.mat[,20] = loss_p20
loss.mat[,21] = loss_p21
loss.mat[,22] = loss_p22
loss.mat[,23] = loss_p23
loss.mat[,24] = loss_p24
loss.mat[,25] = loss_p25
loss.mat[,26] = loss_p26
loss.mat[,27] = loss_p27
loss.mat[,28] = loss_p28
loss.mat[,29] = loss_p29
loss.mat[,30] = loss_p30
loss.mat[,31] = loss_p31
loss.mat[,32] = loss_p32
loss.mat[,33] = loss_p33
loss.mat[,34] = loss_p34

mcsTest(loss.mat,  alpha = 0.25, nboot = 100, nblock = 1, boot = "stationary")

SSM.rmse <- MCSprocedure(Loss = loss.mat, alpha=0.25, B=5000,
                          statistic= "Tmax")

xtable(SSM.rmse@show)
stargazer(SSM.rmse@show)

#### Portfolio optimization
########################## portfolio weight
library(RiskPortfolios)
library(RColorBrewer)

# portfolio weigths
M1 <- M$MV_22
TT<-dim(M1)[3]
k<-dim(M1)[1]
opt_weights<-matrix(0,nrow=c(TT),ncol=3) # we can also put ncol = k

for(tt in 1:TT){
  opt_weights[tt,]<-optimalPortfolio(M1[,,tt], 
                                     #mu = mu, 
                                     control = list(type='minvol',constraint='lo'))
}

# portfolio variance
sig_m <- matrix(0,nrow=c(TT),ncol=k)

for(tt in 1:TT){
  sig_m[tt,] <- opt_weights[tt,]%*%M1[,,tt]}

tran_opt_w <- t(opt_weights)
pt.var <- matrix(0,nrow=c(TT),ncol=1)

for(tt in 1:TT){
  pt.var[tt,] <- sig_m[tt,]%*%tran_opt_w[,tt]}

pt.var #it is the variance of the portfolio
round(pt.var, 8)

pt.ret = opt_weights[,1] * tail(r_t[,1],N) + opt_weights[,2] * tail(r_t[,2],N) + opt_weights[,3] * tail(r_t[,3],N)
colnames(pt.ret) = "portfolio returns"
round(pt.ret, 8)
#########

# just a quick check
opt_weights[1000,]
sum(opt_weights[1000,])
#

opt_weights<-as.xts(opt_weights,time(ret[[1]])) # if warning: run again row 43
# r_t<-list(aapl_ret,goog_ret,aig_ret,mmm_ret)

opt_weights_monthly<-apply(opt_weights,2, function(x) apply.monthly(x,sum))

date_monthly<-time(apply.monthly(opt_weights[,1],sum))

opt_weights_monthly<-as.xts(opt_weights_monthly,date_monthly)

opt_weights_monthly_barplot<-opt_weights_monthly
colnames(opt_weights_monthly_barplot)<-c("ftmib","ibex","cac40")
opt_weights_monthly_barplot<-(opt_weights_monthly_barplot/apply(opt_weights_monthly_barplot,1,sum))*100

#MonthlyReturns <- apply.monthly(db_f, Return.cumulative)


lab<-c(
  paste("0",6:9,"/","09",sep=""),paste(10:12,"/","09",sep=""),
  paste("0",1:9,"/","10",sep=""),paste(10:12,"/","10",sep=""),
  paste("0",1:9,"/","11",sep=""),paste(10:12,"/","11",sep=""),
  paste("0",1:9,"/","12",sep=""),paste(10:12,"/","12",sep=""),
  paste("0",1:9,"/","13",sep=""),paste(10:12,"/","13",sep=""),
  paste("0",1:9,"/","14",sep=""),paste(10:12,"/","14",sep=""),
  paste("0",1:9,"/","15",sep=""),paste(10:12,"/","15",sep=""),
  paste("0",1:9,"/","16",sep=""),paste(10:12,"/","16",sep=""),
  paste("0",1:9,"/","17",sep=""),paste(10:12,"/","17",sep=""),
  paste("0",1:9,"/","18",sep=""),paste(10:12,"/","18",sep=""),
  paste("0",1:9,"/","19",sep=""),paste(10:12,"/","19",sep=""),
  paste("0",1:9,"/","20",sep=""),paste(10:12,"/","20",sep=""),
  paste("0",1:9,"/","21",sep=""),paste(10:12,"/","21",sep=""))


par(mfrow=c(1, 1), mar=c(5, 5, 4, 11))
barplot(opt_weights_monthly_barplot,legend.text=T,col=brewer.pal(n = 4, name = "RdYlGn"),
        ylab="%",xlab="",
        args.legend = list(x = "right", bty = "n", inset=c(-0.25, 0)),names.arg=lab,las=2)
################################################################################################

N_oos = round(N * 0.2)

# we "remove" the first 100 observations: (this in order to evaluate HS)
pt.ret_est = pt.ret[index(pt.ret[(N-636):N,])]

### settings for plots
end_x<-endpoints(pt.ret_est,on="years")
end_x<-end_x+c(rep(1,length(end_x)-1),0)


###

L = length(pt.ret_est)
K2 = 12
mv_m4 <- mv_into_mat(pt.ret, macro_diff$M1.EU ,K=K2,"monthly")

#### VaR and ES settings
tau<-0.05    # we expect a number of violation = N * tau  

###### GARCH #####
# we define the variety of GARCH models we consider
spec.comp<-list()
models<- c("sGARCH","gjrGARCH")
distributions<-c("norm","std")
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


#Fit data into the models
fitmod.pt<- list()
for(s in specifications) {
  fitmod.pt[[s]] <- ugarchfit(spec= spec.comp[[s]], pt.ret_est)
}
fitmod.pt

# store sigma for GARCH models
sigma = list()
for(s in specifications){
  sigma[[s]] <- fitmod.pt[[s]]@fit$sigma
}

## VaR and ES GARCH(1,1) with a normal distribution
VaR_garch_n<-qnorm(tau)*sigma$`sGARCH-norm`
ES_garch_n<- -sigma$`sGARCH-norm`*dnorm(qnorm(tau))/tau


### short evaluation:
sum(ifelse(pt.ret_est<VaR_garch_n,1,0))/L


## VaR and ES GARCH(1,1)with a student-t  distribution
df<-as.numeric(fitmod.pt$`sGARCH-std`@fit$coef['shape'])

correc<-((df-2)/df)^0.5

VaR_garch_t<-qt(tau,df)*sigma$`sGARCH-std`*correc

## first term:
first_term<-(dt(qt(tau,df),df)/tau)

## second term:
second_term<- (df+(qt(tau,df))^2)/(df-1)

ES_garch_t<- -sigma$`sGARCH-std`*first_term*second_term*correc

### short evaluation:
sum(ifelse(pt.ret_est<VaR_garch_t,1,0))/L




##################### plot of returns vs VaR and ES using Garch Normal
end_x<-endpoints(pt.ret_est,on="years")
end_x<-end_x+c(rep(1,length(end_x)-1),0)

pdf("VaR_garch_n.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_garch_n),
     xlab="",ylab="Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_garch_n,col="red",type="l",lwd=2)
lines(1:L,ES_garch_n,col="blue",type="l",lwd=2)

legend(
  "topright",
  c("daily returns","GARCH-N VaR (5%)","GARCH-N ES (5%)"),
  lty=1,
  col=c("black","red","blue"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()

##################### plot of returns vs VaR and ES using Garch student-t

pdf("VaR_garch_t.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_garch_t),
     xlab="",ylab="Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)

lines(1:L,VaR_garch_t,col="red",type="l",lwd=2)
lines(1:L,ES_garch_t,col="gray",type="l",lwd=2)

legend(
  "topright",
  c("daily returns","GARCH-t VaR (5%)","GARCH-t ES (5%)"),
  lty=1,
  col=c("black","red","gray"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()



####################################

#### VaR and ES via non-parametric model
difftime(strptime("2020-06-17", format = "%Y-%m-%d"), strptime("2019-06-17", format = "%Y-%m-%d"), units="days")

source(file="functions.R")
w<-100 # length of rolling window
pt.ret[index(pt.ret[(N-636):N,])]                                                                         

HS_100<-hs_f(pt.ret,w,tau,"2019-06-14","2021")                       

VaR_HS_100<-HS_100$VaR_HS
ES_HS_100<-HS_100$ES_HS

head(VaR_HS_100)
tail(VaR_HS_100)

# Plot
pdf("VaR_HS_100.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_HS_100),
     xlab="",ylab="Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)

lines(1:L,VaR_HS_100,col="blue",type="l",lwd=2)
lines(1:L,ES_HS_100,col="red",type="l",lwd=2)

legend(
  "topright",
  c("portfolio daily ret.",
    "HS (w=100) VaR forecasts",
    "HS (w=100) ES forecasts"),
  lty=1,
  col=c("black","blue","red"),
  lwd=2)


grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()



w<-252 # length of rolling window
HS_252<-hs_f(pt.ret,w,tau,"2019-06-14","2021")

VaR_HS_252<-HS_252$VaR_HS
ES_HS_252<-HS_252$ES_HS

# Plot
pdf("VaR_HS_252.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_HS_252),
     xlab="",ylab="Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_HS_252,col="pink",type="l",lwd=2)
lines(1:L,ES_HS_252,col="green",type="l",lwd=2)

legend(
  "topright",
  c("portfolio daily ret.",
    "HS (w=252) VaR forecasts",
    "HS (w=252) ES forecasts"),
  lty=1,
  col=c("black","pink","green"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)

dev.off()


w<-500 # length of rolling window
HS_500<-hs_f(pt.ret,w,tau,"2019-06-14","2021")

VaR_HS_500<-HS_500$VaR_HS
ES_HS_500<-HS_500$ES_HS

# Plot
pdf("VaR_HS_500.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_HS_500),
     xlab="",ylab="Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2015:2020),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_HS_500,col="brown",type="l",lwd=2)
lines(1:L,ES_HS_500,col="pink",type="l",lwd=2)

legend(
  "topright",
  c("portfolio daily ret.",
    "HS (w=500) VaR forecasts",
    "HS (w=500) ES forecasts"),
  lty=1,
  col=c("black","brown","pink"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()

###### Semi-Parametric methods ######
library(rqmidas)

? uqfit

## Indirect GARCH (NO ES, with bootstrap standard errors)  # SCARTARE:
fit_ig<-ucaviarfit(model="IG", tau, daily_ret = pt.ret, R = 100, out_of_sample = L) # r_t_est
fit_ig
summary.rqmidas(fit_ig)
VaR_IG<-fit_ig$VaR_oos

sum(ifelse(pt.ret_est<VaR_IG,1,0))/L

## Indirect GARCH (With ES)
fit_ig_es<-ucaviarfit(model="IG", tau, daily_ret=pt.ret, R = 100, B=5000, ES="Yes", out_of_sample = L) # con out-of-sample con 1 in piu
fit_ig_es
summary.rqmidas(fit_ig_es)
VaR_IG<-fit_ig_es$VaR_oos
ES_IG<-fit_ig_es$ES_oos

sum(ifelse(pt.ret_est<VaR_IG,1,0))/L


## SAV
fit_sav<-ucaviarfit(model="SAV", tau, daily_ret=pt.ret, R = 100, B=5000, ES="Yes", out_of_sample = L)
fit_sav
summary.rqmidas(fit_sav)
VaR_SAV<-fit_sav$VaR_oos
ES_SAV<-fit_sav$ES_oos

sum(ifelse(pt.ret_est<VaR_SAV,1,0))/L


## AS
fit_as<-ucaviarfit(model="AS", tau, daily_ret=pt.ret, R = 100, B=1000, ES="Yes", out_of_sample = L)
fit_as
summary.rqmidas(fit_as)
VaR_AS<-fit_as$VaR_oos
ES_AS<-fit_as$ES_oos
sum(ifelse(pt.ret_est<VaR_AS,1,0))/L


##################################### linear ARCH
fit_larch<-uqfit(model="lARCH", tau, daily_ret=pt.ret,q=5,ES="Yes", out_of_sample = L)
fit_larch
summary.rqmidas(fit_larch)
VaR_larch<-fit_larch$VaR_oos
ES_larch<-fit_larch$ES_oos



##################################### linear ARCH-MIDAS
fit_larchmidas<-uqfit(model="lARCHMIDAS", tau, daily_ret=pt.ret,q=5,mv_m=mv_m4,K=K2, ES="Yes", out_of_sample = L) # non escono significativi i beta
fit_larchmidas
summary.rqmidas(fit_larchmidas)
VaR_larchmidas<-fit_larchmidas$VaR_oos
ES_larchmidas<-fit_larchmidas$ES_oos


# GRAFICO IG:
pdf("VaR_IG_portfolio.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_IG),
     xlab="",ylab="Portfolio Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_IG,col="green",type="l",lwd=2)
lines(1:L,ES_IG,col="red",type="l",lwd=2)

legend(
  "topright",
  c("Portfolio daily ret.",
    "Indirect GARCH VaR (5%)",
    "Indirect GARCH ES (5%)"),
  lty=1,
  col=c("black","green","red"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()


### PLOTS ###
# GRAFICO SAV:
pdf("VaR_SAV_portfolio.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_SAV),
     xlab="",ylab="Portfolio Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2015:2020),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_SAV,col="blue",type="l",lwd=2)
lines(1:L,ES_SAV,col="grey",type="l",lwd=2)

legend(
  "topright",
  c("SP500 daily ret.","SAV VaR (5%)","SAV ES (5%)"),
  lty=1,
  col=c("black","blue","grey"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()


# GRAFICO AS:
pdf("VaR_AS_portfolio.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_AS),
     xlab="",ylab="Portfolio Returns, VaR and ES (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_AS,col="grey",type="l",lwd=2)
lines(1:L,ES_AS,col="red",type="l",lwd=2)

legend(
  "topright",
  c("Portfolio daily ret.","AS VaR (5%)","ES VaR (5%)"),
  lty=1,
  col=c("black","grey","red"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()


# GRAFICO Linear ARCH:
pdf("VaR_lARCH_portfolio.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_larch),
     xlab="",ylab="Portfolio Returns and VaR (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_larch,col="blue",type="l",lwd=2)
lines(1:L,ES_larch,col="violet",type="l",lwd=2)

legend(
  "topright",
  c("Portfolio daily ret.","Linear ARCH(5) VaR (5%)","Linear ARCH(5) ES (5%)"),
  lty=1,
  col=c("black","blue","violet"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()


# GRAFICO Linear ARCH-MIDAS:
pdf("VaR_lARCHMIDAS_portfolio.pdf",height=9,width=16)
par(mai=c(1,1,0.5,0.3))

plot(1:L,coredata(pt.ret_est),type="l",
     xaxt="n",ylim=range(pt.ret_est,ES_larchmidas),
     xlab="",ylab="Portfolio Returns and VaR (5%)",
     cex.lab=2,font.lab=2,lty=1,col="black",lwd=2)

axis(side=1, 
     end_x[1:(length(end_x))], 
     c(2019:2021),cex.axis = 1.1,xpd=T)


lines(1:L,VaR_larchmidas,col="blue",type="l",lwd=2)
lines(1:L,ES_larchmidas,col="grey",type="l",lwd=2)

legend(
  "topright",
  c("Portfolio daily ret.","Linear ARCH-MIDAS VaR (5%)","Linear ARCH-MIDAS ES (5%)"),
  lty=1,
  col=c("black","blue","grey"),
  lwd=2)

grid(NA,NULL)

abline(v=end_x,h=NA,col="gray",lty=3)


dev.off()




################### save data
VaR_full<-cbind(
  VaR_garch_n,VaR_garch_t,
  # VaR_gm_n,VaR_gm_t,                        # modelli GM non funzionano
  VaR_HS_100,VaR_HS_252,VaR_HS_500,
  VaR_SAV,VaR_AS,VaR_IG,
  VaR_larch ,VaR_larchmidas                  #problemi con il midas
  )


ES_full<-cbind(
  ES_garch_n,ES_garch_t,
  # ES_gm_n,ES_gm_t,                          # modelli GM non funzionano
  ES_HS_100,ES_HS_252,ES_HS_500,
  ES_SAV,ES_AS,ES_IG,ES_larch ,ES_larchmidas #problemi con il midas
  )

###
save(VaR_full,ES_full,pt.ret_est,file="VaR_ES.RData") # modificare
###


###################################                                                              # TO DO
######## BACKTESTING VaR ##########
###################################
library(GAS)

M<-ncol(VaR_full) #number of models

col_res<-matrix(rep(NA,4*M),ncol=4)

for (i in 1:M){
  col_res[i,1]<-as.numeric(BacktestVaR(pt.ret_est, VaR_full[,i], tau)$AE)
  col_res[i,2]<-as.numeric(BacktestVaR(pt.ret_est, VaR_full[,i], tau)$LRuc[2])
  col_res[i,3]<-as.numeric(BacktestVaR(pt.ret_est, VaR_full[,i], tau)$LRcc[2])
  col_res[i,4]<-as.numeric(BacktestVaR(pt.ret_est, VaR_full[,i], tau, Lags = 4)$DQ[2])
}


#### considering the UC test ###########
# tau = 0.05



rownames(col_res)<-c(
  "GARCH-N",
  "GARCH-t",
  # "GARCH-MIDAS-N",
  # "GARCH-MIDAS-t",
  "HS (w=100)",
  "HS (w=250)",
  "HS (w=500)",
  "SAV",
  "AS",
  "IG",
  "Lin. ARCH" ,
  "Lin. ARCH MIDAS"
  )

colnames(col_res)<-c("AE","UC","CC","DQ")

round(col_res,3)

xtable(col_res,type="latex",digits=3)



###################################                                                              # TO DO
######## BACKTESTING ES ##########
###################################

col_res<-matrix(rep(NA,3*M),ncol=3)

for (i in 1:M){
  RES<-ESTest(actual=pt.ret_est, ES=ES_full[,i],VaR=VaR_full[,i],boot=TRUE, n.boot = 2000)
  col_res[i,1]<-as.numeric(RES$expected.exceed)
  col_res[i,2]<-as.numeric(RES$actual.exceed)
  col_res[i,3]<-as.numeric(RES$p.value)
}


rownames(col_res)<-c(
  "GARCH-N",
  "GARCH-t",
  # "GARCH-MIDAS-N",
  # "GARCH-MIDAS-t",
  "HS (w=100)",
  "HS (w=250)",
  "HS (w=500)",
  "SAV",
  "AS",
  "IG",
  "Lin. ARCH",
  "Lin. ARCH MIDAS"
  )

colnames(col_res)<-c("Expected Exc.","Actual Exc.","P-value")

round(col_res,3)

xtable(col_res,type="latex",digits=3)



###################################                                                              # TO DO
########       MCS       ##########
###################################
loss_M1<-QL(pt.ret_est,VaR_full[,1],tau)
loss_M2<-QL(pt.ret_est,VaR_full[,2],tau)
loss_M3<-QL(pt.ret_est,VaR_full[,3],tau)
loss_M4<-QL(pt.ret_est,VaR_full[,4],tau)
loss_M5<-QL(pt.ret_est,VaR_full[,5],tau)
loss_M6<-QL(pt.ret_est,VaR_full[,6],tau)
loss_M7<-QL(pt.ret_est,VaR_full[,7],tau)
loss_M8<-QL(pt.ret_est,VaR_full[,8],tau)
loss_M9<-QL(pt.ret_est,VaR_full[,9],tau)
loss_M10<-QL(pt.ret_est,VaR_full[,10],tau)
# loss_M11<-QL(pt.ret_est,VaR_full[,11],tau)
# loss_M12<-QL(pt.ret_est,VaR_full[,12],tau)

db_loss<-cbind(
  loss_M1,
  loss_M2,
  loss_M3,
  loss_M4,
  loss_M5,
  loss_M6,
  loss_M7,
  loss_M8,
  loss_M9,
  loss_M10 #,
  # loss_M11,
  # loss_M12
)

colnames(db_loss)<-rownames(col_res)

############################################# MCS 

alpha<-0.25
B<-5000

MCS_est<-mcsTest(coredata(db_loss), 
                 alpha = alpha, 
                 nboot = B, 
                 nblock = 10, #12
                 boot = c("block"))

col_means<-colMeans(coredata(db_loss))

dim(db_loss)

MCS_est$includedSQ

col_mcs<-cbind(
  round(col_means*100,3),
  ifelse(1:ncol(db_loss) %in% MCS_est$includedSQ,1,0)
)

row_mcs_f<-cbind(paste(
  ifelse(col_mcs[,2]==1,"cellcolor{gray!75}",""),
  col_mcs[,1],sep="")
)

rownames(row_mcs_f)<-colnames(db_loss)

xtable(row_mcs_f,type="latex",digits=3)

###################################                                                              # TO DO
###### BACKTESTING VaR + ES #######
###################################

loss_M1<-FZLoss(pt.ret_est,VaR_full[,1],ES_full[,1],tau)
loss_M2<-FZLoss(pt.ret_est,VaR_full[,2],ES_full[,2],tau)
loss_M3<-FZLoss(pt.ret_est,VaR_full[,3],ES_full[,3],tau)
loss_M4<-FZLoss(pt.ret_est,VaR_full[,4],ES_full[,4],tau)
loss_M5<-FZLoss(pt.ret_est,VaR_full[,5],ES_full[,5],tau)
loss_M6<-FZLoss(pt.ret_est,VaR_full[,6],ES_full[,6],tau)
loss_M7<-FZLoss(pt.ret_est,VaR_full[,7],ES_full[,7],tau)
loss_M8<-FZLoss(pt.ret_est,VaR_full[,8],ES_full[,8],tau)
loss_M9<-FZLoss(pt.ret_est,VaR_full[,9],ES_full[,9],tau)
loss_M10<-FZLoss(pt.ret_est,VaR_full[,10],ES_full[,10],tau)
# loss_M11<-FZLoss(pt.ret_est,VaR_full[,11],ES_full[,11],tau)
# loss_M12<-FZLoss(pt.ret_est,VaR_full[,12],ES_full[,12],tau)



db_loss<-cbind(
  loss_M1,
  loss_M2,
  loss_M3,
  loss_M4,
  loss_M5,
  loss_M6,
  loss_M7,
  loss_M8,
  loss_M9,
  loss_M10 #,
  #loss_M11,
  #loss_M12
)

colnames(db_loss)<-rownames(col_res)[1:10] # 1:12

############################################# MCS 

alpha<-0.25
B<-5000

MCS_est<-mcsTest(coredata(db_loss), 
                 alpha = alpha, 
                 nboot = B, 
                 nblock = 10, # 12
                 boot = c("block"))

col_means<-colMeans(coredata(db_loss))

dim(db_loss)

MCS_est$includedSQ

col_mcs<-cbind(
  round(col_means,3),
  ifelse(1:ncol(db_loss) %in% MCS_est$includedSQ,1,0)
)

row_mcs_f<-cbind(paste(
  ifelse(col_mcs[,2]==1,"cellcolor{gray!75}",""),
  col_mcs[,1],sep="")
)

rownames(row_mcs_f)<-colnames(db_loss)

xtable(row_mcs_f,type="latex",digits=3)








