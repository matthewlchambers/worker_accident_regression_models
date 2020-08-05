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
log using construction_nonlinear_analysis_log.log, replace name(main)

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

* Make some variables for modeling simple non-linearity in the exposure.
gen mean_pm25_2 = mean_pm25^2
gen mean_pm25_3 = mean_pm25^3
gen mean_pm25_log = log(mean_pm25)
gen inversion_coverage_2 = inversion_coverage^2
gen inversion_coverage_3 = inversion_coverage^3

* Do a batch of polynomial and log regressions
eststo, title("Linear"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips) first
eststo, title("Quadratic"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 mean_pm25_2 = inversion_coverage inversion_coverage_2), absorb(fips) cluster(fips) first
eststo, title("Cubic"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 mean_pm25_2 mean_pm25_3 = inversion_coverage inversion_coverage_2 inversion_coverage_3), absorb(fips) cluster(fips) first
eststo, title("Linear-Log"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25_log = inversion_coverage), absorb(fips) cluster(fips) first

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using simple_nonlinear_regressions.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Simple Nonlinear Regressions\label{tab1})
esttab using simple_nonlinear_regressions.rtf, replace label mtitles nogap onecell
eststo clear

* Make some bins of exposure level, shock size, and inversions for looking at more complicated non-linearities.
egen pm25_exposure_bin_labels = cut(mean_pm25), group(10)
egen inversion_exposure_bin_labels = cut(inversion_coverage), group(20)
gen pm25_shock = mean_pm25 - L.mean_pm25
gen inversion_shock = inversion_coverage - L.inversion_coverage
egen pm25_shock_bin_labels = cut(pm25_shock), group(10)
egen inversion_shock_bin_labels = cut(inversion_shock), group(20)

* Have to actually create dummy variables, since ivreg2 does not support factor variable operators.
tabulate pm25_exposure_bin_labels, generate(pm25_exposure_bin_)
tabulate inversion_exposure_bin_labels, generate(inversion_exposure_bin_)
tabulate pm25_shock_bin_labels, generate(pm25_shock_bin_)
tabulate inversion_shock_bin_labels, generate(inversion_shock_bin_)
drop *_labels

* Do the regressions on binned variables.
eststo, title("Exposure Bins"): reghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (pm25_exposure_bin_* = inversion_exposure_bin_*), absorb(fips month) cluster(fips) old
eststo, title("Shock Bins"): reghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (pm25_shock_bin_* = inversion_shock_bin_*), absorb(fips month) cluster(fips) old
eststo, title("Exposure and Shock Bins"): reghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (pm25_exposure_bin_* pm25_shock_bin_* = inversion_exposure_bin_* inversion_shock_bin_*), absorb(fips month) cluster(fips) old

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using binned_nonlinear_regressions.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Binned Nonlinear Regressions\label{tab1})
esttab using binned_nonlinear_regressions.rtf, replace label mtitles nogap onecell
eststo clear

*********** Below this line also applies to all regressions ********************

* Close the log
log close main
