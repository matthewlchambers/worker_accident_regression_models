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
log using construction_monthly_analysis_log.log, replace name(main)

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

* Loop over months to explore heterogeneity in 
forvalues i = 1/12 {
	preserve
	keep if month == `i'
	eststo, title("Month `i' Cluster: County"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips) first
//	eststo, title("Month `i' Cluster: County Date"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips date) first
	restore
}

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using monthly_regressions.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Monthly Regressions\label{tab1})
esttab using monthly_regressions.rtf, replace label mtitles nogap onecell
eststo clear

eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (c.mean_pm25#i.month = c.inversion_coverage#i.month), absorb(fips) cluster(fips) first

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using separate_months_one_regression.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Month Interactions\label{tab1})
esttab using separate_months_one_regression.rtf, replace label mtitles nogap onecell
eststo clear

*********** Below this line also applies to all regressions ********************

* Close the log
log close main
