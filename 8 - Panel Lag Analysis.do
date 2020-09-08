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
log using lag_analysis.log, replace name(main)

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

* Do a batch of simple linear iv regressions with different lag structures
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (L(0/1).mean_pm25 = L(0/1).inversion_coverage), absorb(fips) cluster(fips) first
eststo: testparm L(0/1).mean_pm25
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (L(0/3).mean_pm25 = L(0/3).inversion_coverage), absorb(fips) cluster(fips) first
eststo: testparm L(0/3).mean_pm25
eststo: testparm L(1/3).mean_pm25

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using unconstrained_distributed_lag.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Lag Analysis\label{tab1})
esttab using unconstrained_distributed_lag.rtf, replace label mtitles nogap onecell
eststo clear

* Check each of several lags, included with the contemporaneous effect but 
* no intervening lags, to  try and get a picture of how effects change over time
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 L.mean_pm25 = inversion_coverage L.inversion_coverage), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 L3.mean_pm25 = inversion_coverage L3.inversion_coverage), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 L5.mean_pm25 = inversion_coverage L5.inversion_coverage), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 L7.mean_pm25 = inversion_coverage L7.inversion_coverage), absorb(fips) cluster(fips) first

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using individual_lags.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Lag Analysis\label{tab1})
esttab using individual_lags.rtf, replace label mtitles nogap onecell
eststo clear

* Use Wooldridge p.326 to estimate long-run effect with standard error.
forvalues i = 1/4 {
	 gen pm25_diff_`i' = L`i'.mean_pm25 - mean_pm25
	 gen inversion_diff_`i' = L`i'.inversion_coverage - inversion_coverage
}

eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 pm25_diff_1 = inversion_coverage inversion_diff_1), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 pm25_diff_1 pm25_diff_2 = inversion_coverage inversion_diff_1 inversion_diff_2), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 pm25_diff_1 pm25_diff_2 pm25_diff_3 = inversion_coverage inversion_diff_1 inversion_diff_2 inversion_diff_3), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 pm25_diff_1 pm25_diff_2 pm25_diff_3 pm25_diff_4 = inversion_coverage inversion_diff_1 inversion_diff_2 inversion_diff_3 inversion_diff_4), absorb(fips) cluster(fips) first

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using long_run_effect.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Lag Analysis\label{tab1})
esttab using long_run_effect.rtf, replace label mtitles nogap onecell
eststo clear

* Close the log
log close main
