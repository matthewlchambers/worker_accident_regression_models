* Start with a clean slate.
log close _all
clear all

* Make sure the needed packages are installed
ssc install ivreg2

* Set the working directory.
cd "E:/Research Projects/Worker Accidents and Pollution/Regression Models"

* Import the clean data file, produced using R. I'm using Stata for the analysis.
* because Stata works with panel data a little easier.
import delimited "../Data/Data for Regression Models/construction_accidents_2003_to_2015.csv", varnames(1) numericcols(3/10)

* Replace the string date variable with one readable by Stata.
gen temporary_date = date(date, "YMD")
drop date
rename temporary_date date
format date %td

* Destring mean_pm25 because somehow it imported as a date.
destring mean_pm25, force replace

* Drop any observations from Alaska, Hawaii, or Puerto Rico.
drop if floor(fips / 1000) == 2 | floor(fips / 1000) == 15 | floor(fips / 1000) == 72

* Declare the data as panel data.
xtset fips date

* Make a binary for an accident occurring.
gen accident_occurred = 1 if num_accidents > 0
replace accident_occurred = 0 if accident_occurred == .

* Make some variables for modeling simple non-linearity in the exposure.
gen mean_pm25_2 = mean_pm25^2
gen mean_pm25_3 = mean_pm25^3
gen mean_pm25_log = log(mean_pm25)
gen inversion_coverage_2 = inversion_coverage^2
gen inversion_coverage_3 = inversion_coverage^3

* Start esimating some models.
quietly eststo: xtivreg accident_occurred mean_temperature mean_precipitation employment i.weekday (mean_pm25 = inversion_coverage), fe
quietly eststo: xtivreg accident_occurred mean_temperature mean_precipitation employment i.weekday (mean_pm25 mean_pm25_2 = inversion_coverage inversion_coverage_2), fe
quietly eststo: xtivreg accident_occurred mean_temperature mean_precipitation employment i.weekday (mean_pm25 mean_pm25_2 mean_pm25_3 = inversion_coverage inversion_coverage_2 inversion_coverage_3), fe
quietly eststo: xtivreg accident_occurred mean_temperature mean_precipitation employment i.weekday (mean_pm25_log = inversion_coverage), fe

//esttab using parametric_table.tex, label replace booktabs alignment(D{.}{.}{-1}) title(Initial Regressions\label{tab1})
esttab using parametric_table.rtf, replace label nogap onecell

* Make some bins of exposure level, shock size, and inversions for looking at more complicated non-linearities.
// egen pm25_exposure_bin_labels = cut(mean_pm25), group(20)
// egen inversion_exposure_bin_labels = cut(inversion_coverage), group(20)
// gen pm25_shock = mean_pm25 - L.mean_pm25
// gen inversion_shock = inversion_coverage - L.inversion_coverage
// egen pm25_shock_bin_labels = cut(pm25_shock), group(20)
// egen inversion_shock_bin_labels = cut(inversion_shock), group(20)
