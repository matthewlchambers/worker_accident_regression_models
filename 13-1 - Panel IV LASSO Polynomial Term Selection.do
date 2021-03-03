* Start with a clean slate.
log close _all
clear all

***************** Make sure the needed packages are installed ******************

* Install estout to get nice output from regressions
ssc install estout

* Install ranktest for ivlasso
cap ado uninstall ranktest
ssc install ranktest

* Finally, install this package
cap ado uninstall lassopack
cap ado uninstall pdslasso

net install lassopack, from("https://raw.githubusercontent.com/statalasso/lassopack/master/") 
net install pdslasso, from("https://raw.githubusercontent.com/statalasso/pdslasso/master/") 

************************** Done installing packages ****************************

********* Make changes below to switch between local and ACCRE *****************

* Set the working directory.
cd "E:/Research Projects/Worker Accidents and Pollution/Regression Models"

* Start logging
log using iv_lasso.log, replace name(main)

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

* Generate powers of inversion coverage as separate variables so that I can prevent
* ivlasso from dropping them and then refusing to estimate the equation
forvalues i = 1/5 {
	gen inversion_coverage_`i' = inversion_coverage^`i'
	gen mean_pm25_`i' = mean_pm25^`i'
}

forvalues i = 1/5 {
	reghdfe mean_pm25_`i' inversion_coverage_* mean_temperature mean_precipitation employment weekday_dummy_*, absorb(fips) cluster(fips)
	predict pm25_hat_`i', xb
}

* Note that these standard errors will be wrong, but I just want to see which variables are selected.
eststo: rlasso accident_occurred pm25_hat_* mean_temperature mean_precipitation employment weekday_dummy_*, sqrt fe cluster(fips)

// eststo: ivlasso accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), cluster(fips) fe first
// eststo: ivlasso accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (c.mean_pm25##c.mean_pm25 = c.inversion_coverage##c.inversion_coverage), cluster(fips) fe first
// eststo: ivlasso accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (c.mean_pm25##c.mean_pm25##c.mean_pm25##c.mean_pm25##c.mean_pm25 = inversion_coverage_*), cluster(fips) fe first pnotpen(inversion_coverage_1 inversion_coverage_2 inversion_coverage_3 inversion_coverage_4 inversion_coverage_5)
// eststo: ivlasso accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (c.mean_pm25##c.mean_pm25##c.mean_pm25##c.mean_pm25##c.mean_pm25##c.mean_pm25 = inversion_coverage_*), cluster(fips) fe first pnotpen(inversion_coverage_1 inversion_coverage_2 inversion_coverage_3 inversion_coverage_4 inversion_coverage_5 inversion_coverage_6)

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using iv_lasso.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(FE and Clustering Experiments\label{tab1})
esttab using iv_lasso.rtf, replace label mtitles nogap onecell
eststo clear

* Close the log
log close main
