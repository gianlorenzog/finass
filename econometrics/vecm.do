*** COMMANDS FOR DATASET'S CLEANING AND PREPARATION ***
* generate monthly data from string variable
generate date = monthly(periodunit, "YM")
format date %tm

* generate quarterly data from string variable
generate qdate = quarterly(periodunit, "YQ")
format qdate %tq

* generate quarterly data from string variable (monthly periods)
gen qdate = qofd(dofm(monthly(periodunit, "YM")))
format qdate %tq

* collapse monthly data into quarterly
collapse m1 hicp eurostoxx50, by(qdate)

tsset date
tsset qdate
*****************************
*** NOTES ***
* Euro introduction January 1999q1
* Entry into circulation of the Euro January 2002q1
* ECB APP 2015q1 - 2018q4
* Covid - shock output: 2020q1
* ECB Pandemic Emergency Purchase Programme: 2020q1
* hp: m1 e gdp more exogenous; inflation and eurostoxx50 least exogenous

**** PROJECT STARTS HERE ****
tsset qdate
keep if qdate >= tq(1997q1)

descr
summ m1 rgdp eurostoxx50 hicp, d

* check missing values
codebook m1 rgdp eurostoxx50 hicp

*outliers
iqr eurostoxx50
iqr rgdp
iqr m1
iqr hicp

tsline eurostoxx50
tsline m1
tsline rgdp
tsline hicp

* Logs
gen lrgdp = log(rgdp)
gen logm1 = log(m1)
gen loghicp = log(hicp)
gen logstoxx = log(eurostoxx50)

* tsline
tsline logstoxx
tsline lrgdp
tsline loghicp
tsline logm1

tsline logstoxx lrgdp loghicp logm1

* descriptive statistics with logs
summ logstoxx lrgdp hicp logm1, d

histogram logstoxx, normal normopts(lcolor(gs13)) kdensity kdenopts(lcolor(black))
histogram logstoxx, normal
qnorm logstoxx, grid
pnorm logstoxx, grid

* check again outliers on the log of eurostoxx50
iqr logstoxx

* Stationarity at Logs levels
tsline logstoxx
dfuller logstoxx, lags(5) notrend reg
* significant at 10%
dfgls logstoxx, maxlag(5)
* we cannot reject the null

tsline lrgdp
dfuller lrgdp, lags(5) trend reg
dfgls lrgdp, maxlag(5) trend
* non-stationary

tsline logm1
dfuller logm1, lags(5) trend reg
dfgls logm1, maxlag(5) trend
* non-stationary

tsline loghicp
dfuller loghicp, lags(5) notrend reg
dfgls loghicp, maxlag(5) trend
* non-stationary

* Structural breaks
* xtbreak: multiple tests for structural breaks (based on Bai and Perron 2003) 
h xtbreak
xtbreak logstoxx loghicp lrgdp logm1
* no breaks

xtbreak test logstoxx loghicp lrgdp logm1, breaks(1)
* 2008q3; test statistics = 21.18
xtbreak test logstoxx loghicp lrgdp logm1, breaks(2)
* 2002q2 2008q3; test statistics = 42.49
xtbreak test logstoxx loghicp lrgdp logm1, breaks(3)
* 2002q2 2008q3 2015q2; test statistics =  38.78

* H0: 1 break vs H1: break(s)
xtbreak test logstoxx loghicp lrgdp logm1, h(2) breaks(2)
xtbreak test logstoxx loghicp lrgdp logm1, h(2) breaks(3)

* breakpoint estimate
xtbreak estimate logstoxx loghicp lrgdp logm1, breaks(1)
xtbreak estimate logstoxx loghicp lrgdp logm1, breaks(2)
xtbreak estimate logstoxx loghicp lrgdp logm1, breaks(3)

* testing individual breakpoints
xtbreak test logstoxx loghicp lrgdp logm1, breakpoints(2002q1, fmt(tq))
xtbreak test logstoxx loghicp lrgdp logm1, breakpoints(2008q1, fmt(tq))
xtbreak test logstoxx loghicp lrgdp logm1, breakpoints(2015q1, fmt(tq))
xtbreak test logstoxx loghicp lrgdp logm1, breakpoints(2020q1, fmt(tq))

* Stationarity with structural breaks at Logs levels
*Zivot-Andrews (JBES 1992) unit root test for a timeseries allowing for one structural break in the series
* H0: the series has a unit root without a structural breaks
* H1: the series is stationary with a structural break
h zandrews
zandrews logstoxx, maxlags(5) lagmethod(BIC) graph
zandrews lrgdp, maxlags(5) lagmethod(BIC)
zandrews logm1, maxlags(5) lagmethod(BIC)
zandrews loghicp, maxlags(5) lagmethod(BIC)
* we cannot reject H0, so the series are not stationary.

* two-break unit root tests described by Clemente, Montañés, Reyes (1998)
* The test considers the null hypothesis that (rho - 1) is different from zero. A test statistic exceeding the critical value is significant.
h clemao
clemao2 logstoxx
clemao2 lrgdp
clemao2 logm1
clemao2 loghicp

* lag length
varsoc logm1 lrgdp loghicp logstoxx, maxlag(8)

* Cointegration test (without structural breaks)
h vecrank
vecrank logm1 lrgdp loghicp logstoxx, trend(rconstant) trace max ic levela
vecrank logm1 lrgdp loghicp logstoxx, lags(3) trend(rconstant) trace max ic levela
vecrank logm1 lrgdp loghicp logstoxx, lags(2) trend(rconstant) trace max ic levela
* 1 cointegrated relationship

* Gregory Hansen Cointegration with structural break
* H0 = No cointegration
* H1 = cointegration with a single shift
h ghansen
ghansen logm1 lrgdp loghicp logstoxx, break(trend) lagmethod(downt)
ghansen logm1 lrgdp loghicp logstoxx, break(regime) lagmethod(downt)
ghansen logm1 lrgdp loghicp logstoxx, break(level) lagmethod(downt)
* No Cointegration

* First differences (just for summary statistics)
gen returns = D.logstoxx
gen m1growth = D.logm1
gen growth = D.lrgdp
gen hicprate = D.loghicp

tsline returns hicprate growth m1growth
summ returns growth m1growth hicprate, d

xtbreak returns hicprate growth m1growth
xtbreak test returns hicprate growth m1growth, breaks(1)


* Cumsum: it tests for structural breaks in the residual.
* Under the null hypothesis, the cumulative sum of residuals will have mean zero.The command also graphs the cumulative sum with confidence bands, which allows you to see whether the series behaves as the null hypothesis would predict.

regress logstoxx loghicp logm1 lrgd
estat sbcusum, level(99)
h sbcusum


* VECM
* n-1 lag

vecrank logm1 lrgdp loghicp logstoxx, trend(rconstant) trace max ic levela

h vec

vec logstoxx loghicp logm1 lrgdp, rank(1) trend(rconstant) lags(4)
est store vecm4

vec logstoxx loghicp logm1 lrgdp, rank(1) trend(rconstant) lags(3)
est store vecm3

vec logstoxx loghicp logm1 lrgdp, rank(1) trend(rconstant) lags(2)
est store vecm2

estimate table vecm4 vecm3 vecm2, stats(ll aic bic)


qui vec logstoxx loghicp logm1 lrgdp, rank(1) trend(rconstant) lags(3)

vecstable, graph
veclmar, mlag(3)
vecnorm

* IRF & FEVD
irf create vecm, set(vecm, replace) step(20)
irf graph oirf
irf table oirf

irf table fevd
irf graph fevd

irf cgraph (vecm loghicp logstoxx oirf) (vecm logm1 logstoxx oirf) (vecm lrgdp logstoxx oirf) (vecm logm1 lrgdp oirf) (vecm logm1 loghicp oirf) (vecm loghicp logm1 oirf), title("oirf vecm")

irf cgraph (vecm loghicp logstoxx fevd) (vecm logm1 logstoxx fevd) (vecm lrgdp logstoxx fevd) (vecm logm1 lrgdp fevd) (vecm logm1 loghicp fevd) (vecm loghicp logm1 fevd), title("fevd vecm")

irf ctable (vecm loghicp logstoxx oirf) (vecm logm1 logstoxx oirf) (vecm lrgdp logstoxx oirf) (vecm logm1 loghicp oirf) (vecm loghicp logstoxx fevd) (vecm logm1 logstoxx fevd) (vecm lrgdp logstoxx fevd) (vecm logm1 loghicp fevd)

* Spectral Granger Causality
bcgcausality logstoxx loghicp, varlag(3)
bcgcausality loghicp logstoxx, varlag(3)
bcgcausality loghicp logm1, varlag(3)
bcgcausality logm1 loghicp, varlag(3)
bcgcausality logstoxx lrgdp, varlag(3)
bcgcausality lrgdp logstoxx, varlag(3)
bcgcausality lrgdp logm1, varlag(3)
bcgcausality lrgdp loghicp, varlag(3)
bcgcausality loghicp lrgdp, varlag(3)

bcgcausality logstoxx loghicp, varlag(3) freq(0.01)
bcgcausality logstoxx loghicp logm1, varlag(3) freq(0.01)
bcgcausality logstoxx loghicp lrgdp, varlag(3) freq(0.01)
bcgcausality logstoxx loghicp, varlag(3) freq(3.14)

bcgcausality loghicp logstoxx, varlag(3) freq(0.01)
bcgcausality loghicp logm1, varlag(3) freq(0.01)
bcgcausality loghicp logm1, varlag(3) freq(0.36)
bcgcausality loghicp logm1, varlag(3) freq(3.14)

bcgcausality logstoxx logm1, varlag(3) freq(0.01)
bcgcausality logstoxx logm1, varlag(3) freq(3.14)
bcgcausality logstoxx logm1 lrgdp, varlag(3) freq(0.01)
bcgcausality logstoxx logm1 lrgdp, varlag(3) freq(3.14)

bcgcausality logstoxx logm1 loghicp, varlag(3) freq(0.01)
bcgcausality logstoxx logm1 loghicp, varlag(3) freq(3.14)

bcgcausality logm1 loghicp, varlag(3) freq(0.01)
bcgcausality logm1 loghicp, varlag(3) freq(3.14)

bcgcausality lrgdp logstoxx, varlag(3) freq(0.50)
bcgcausality lrgdp logstoxx, varlag(3) freq(0.90)

* Cross correlation M1 - hicp
xcorr logm1 loghicp, name(left)
xcorr loghicp logm1, name(right)
graph combine left right, name(leftright)

pwcorr logm1 loghicp, sig star(.05) sidak
pwcorr loghicp logm1, sig star(.05) sidak


* dynamic forecast
tsset
h fcast
qui vec logstoxx loghicp logm1 lrgdp, rank(1) trend(rconstant) lags(3)

fcast compute f_, step(5)
fcast graph f_logm1 f_lrgdp f_loghicp f_logstoxx


