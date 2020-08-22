* Start with a clean slate.
log close _all
clear all

***************** Make sure the needed packages are installed ******************

* Install estout to get nice output from regressions
ssc install estout

* Install ftools (remove program if it existed previously)
cap ado uninstall ftools
net install ftools, from("https://raw.githubusercontent.com/sergiocorreia/ftools/master/src/")

* Install reghdfe
cap ado uninstall reghdfe
net install reghdfe, from("https://raw.githubusercontent.com/sergiocorreia/reghdfe/master/src/")

* Install boottest (Stata 11 and 12)
if (c(version)<13) cap ado uninstall boottest
if (c(version)<13) ssc install boottest

* Install moremata (sometimes used by ftools but not needed for reghdfe)
cap ssc install moremata

* Install ivreg2, the core package
cap ado uninstall ivreg2
ssc install ivreg2

* Finally, install this package
cap ado uninstall ivreghdfe
net install ivreghdfe, from(https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/)

************************** Done installing packages ****************************

********* Make changes below to switch between local and ACCRE *****************

* Set the working directory.
cd "E:/Research Projects/Worker Accidents and Pollution/Regression Models"

* Start logging
log using construction_linear_analysis_log.log, replace name(main)

* Import the clean data file, produced using R. I'm using Stata for the analysis.
* because Stata works with panel data a little easier.
import delimited "../Data/Data for Regression Models/construction_accidents_2003_to_2015.csv", varnames(1) numericcols(3/10)

* Take a smaller sample for local code testing
keep if strpos(date, "2005")

********* End of section to change when switching between local and ACCRE ******

********* Basic fixes to data file, applicable to every regression *************

* Replace the string date variable with one readable by Stata.
gen temporary_date = date(date, "YMD")
drop date
rename temporary_date date
format date %td

* Create a month variable so I can absorb month-of-year fixed effects
gen month = month(date)

* Create weekday dummy variables, since ivreg2 can't handle factor variables
tabulate weekday, generate(weekday_dummy_)
drop weekday_dummy_1

* Destring mean_pm25 because somehow it imported as a date.
destring mean_pm25, force replace

* Drop any observations from Alaska, Hawaii, or Puerto Rico.
drop if floor(fips / 1000) == 2 | floor(fips / 1000) == 15 | floor(fips / 1000) == 72

* Declare the data as panel data.
xtset fips date

* Make a binary for an accident occurring.
gen accident_occurred = 1 if num_accidents > 0
replace accident_occurred = 0 if accident_occurred == .

********* End basic fixes to data file, applicable to every regression *********

* Create variables for polynomial distributed lag model. See Burkhardt et al 2019
gen x0 = mean_pm25 + L.mean_pm25 + L2.mean_pm25 + L3.mean_pm25
gen x1 = L.mean_pm25 + 2 * L2.mean_pm25 + 3 * L3.mean_pm25
gen x2 = L.mean_pm25 + 4 * L2.mean_pm25 + 9 * L3.mean_pm25
gen x3 = L.mean_pm25 + 8 * L2.mean_pm25 + 27 * L3.mean_pm25

gen z0 = inversion_coverage + L.inversion_coverage + L2.inversion_coverage + L3.inversion_coverage
gen z1 = L.inversion_coverage + 2 * L2.inversion_coverage + 3 * L3.inversion_coverage
gen z2 = L.inversion_coverage + 4 * L2.inversion_coverage + 9 * L3.inversion_coverage
gen z3 = L.inversion_coverage + 8 * L2.inversion_coverage + 27 * L3.inversion_coverage

* Do a batch of simple linear iv regressions with different lag structures
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (x0-x2 = z0-z2), absorb(fips) cluster(fips) first
eststo: nlcom (beta_0:_b[x0]) (beta_1:_b[x0] + _b[x1] + _b[x2]) (beta_2:_b[x0] + 2 * _b[x1] + 4 * _b[x2]) (beta_3:_b[x0] + 3 * _b[x1] + 9 * _b[x2]), post
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (x0-x3 = z0-z3), absorb(fips) cluster(fips) first
eststo: nlcom (beta_0:_b[x0]) (beta_1:_b[x0] + _b[x1] + _b[x2] + _b[x3]) (beta_2:_b[x0] + 2 * _b[x1] + 4 * _b[x2] + 8 * _b[x3]) (beta_3:_b[x0] + 3 * _b[x1] + 9 * _b[x2] + 27 * _b[x3]), post

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using distributed_lag_analysis.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Lag Analysis\label{tab1})
esttab using distributed_lag_analysis.rtf, replace label mtitles nogap onecell
eststo clear

* Close the log
log close main
