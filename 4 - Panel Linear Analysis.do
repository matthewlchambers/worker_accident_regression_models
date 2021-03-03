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
net install ivreghdfe, from("https://raw.githubusercontent.com/sergiocorreia/ivreghdfe/master/src/")

* Not sure why this is necessary, but it is
reghdfe, compile
ftools, compile

************************** Done installing packages ****************************

** Make changes below to switch between local and ACCRE, & between industries **

* Set the working directory.
cd "E:/Research Projects/Worker Accidents and Pollution/Regression Models"

* Set the industry
local industry "manufacturing"

* Start logging
log using `industry'_linear_analysis_log.log, replace name(main)

* Import the clean data file, produced using R. I'm using Stata for the analysis.
* because Stata works with panel data a little easier.
import delimited "../Data/Data for Regression Models/`industry'_accidents_2003_to_2015.csv", varnames(1) numericcols(3/10)

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

* Destring mean_pm25 and other variables because somehow they imported strangely
destring mean_pm25, force replace
destring qtrly_estabs, force replace     // Note that some of these are actually
destring employment, force replace		 // missing for some industries/counties
destring total_employment, force replace // in some years

* Drop any observations from Alaska, Hawaii, or Puerto Rico.
drop if floor(fips / 1000) == 2 | floor(fips / 1000) == 15 | floor(fips / 1000) == 72

* Declare the data as panel data.
xtset fips date

* Make a binary for an accident occurring.
gen accident_occurred = 1 if num_accidents > 0
replace accident_occurred = 0 if accident_occurred == .

********* End basic fixes to data file, applicable to every regression *********

* Do a batch of simple linear iv regressions with different fixed effects and different clustering
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips date) first
eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips month) cluster(fips) first
// This line and the one below it are the new ones as of 9-7-2020. Want to see what will happen if counties can have different first stage slopes.
// eststo: ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = i.fips#c.inversion_coverage), absorb(fips month) cluster(fips) first

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using `industry'_fe_clustering_experiments.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(FE and Clustering Experiments\label{tab1})
esttab using `industry'_fe_clustering_experiments.rtf, replace label mtitles nogap onecell
eststo clear

// * Construct some alternate measures of PM 2.5, such as moving averages 
// * and shocks, along with similar measures of inversions for instruments
// gen ma_pm25 = (L.mean_pm25 + mean_pm25) / 2
// gen pm25_shock = mean_pm25 - L.mean_pm25
// gen ma_inversion_coverage = (L.inversion_coverage + inversion_coverage) / 2
// gen inversion_coverage_shock = inversion_coverage - L.inversion_coverage
//
// gen year = year(date)
// bysort fips year: egen annual_mean_pm25 = mean(mean_pm25)
// gen diff_from_annual_mean_pm25 = mean_pm25 - annual_mean_pm25
//
// * Do regressions using other PM 2.5 measures
// eststo, title("Exposure"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (mean_pm25 = inversion_coverage), absorb(fips) cluster(fips) first
// eststo, title("Shock"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (pm25_shock = inversion_coverage_shock), absorb(fips) cluster(fips) first
// eststo, title("2-Day Moving Average"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (ma_pm25 = ma_inversion_coverage), absorb(fips) cluster(fips) first
// eststo, title("Difference from Annual Mean"): ivreghdfe accident_occurred mean_temperature mean_precipitation employment weekday_dummy_* (diff_from_annual_mean_pm25 = inversion_coverage), absorb(fips) cluster(fips) first
//
// * Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
// esttab using exp_shock_ma_experiments.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(Exposure, Shock, and Moving Average Experiments\label{tab1})
// esttab using exp_shock_ma_experiments.rtf, replace label mtitles nogap onecell
// eststo clear

* Close the log
log close main
