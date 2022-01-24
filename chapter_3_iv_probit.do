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

* Set the industry.
local industry "traffic"

* Start logging
log using `industry'_linear_analysis_log.log, replace name(main)

* Import the clean data file, produced using R. I'm using Stata for the analysis.
* because Stata works with panel data a little easier.
import delimited "../Data/Data for Regression Models/`industry'_accidents_2003_to_2015.csv", varnames(1) numericcols(3/25)

* Take a smaller sample for local code testing
keep if strpos(date, "2005") | strpos(date, "2006")

********* End of section to change when switching between local and ACCRE ******

********* Basic fixes to data file, applicable to every regression *************

* Replace the string date variable with one readable by Stata.
gen temporary_date = date(date, "YMD")
drop date
rename temporary_date date
format date %td

* Create a month variable so I can absorb month-of-year fixed effects
gen month = month(date)

// * Create weekday dummy variables, since ivreg2 can't handle factor variables
// tabulate weekday, generate(weekday_dummy_)
// drop weekday_dummy_1

* Drop any observations from Alaska, Hawaii, or Puerto Rico.
drop if floor(fips / 1000) == 2 | floor(fips / 1000) == 15 | floor(fips / 1000) == 72

* Drop any duplicates on fips and date, since multiple accidents may occur in the same county on the same day
* but I just want a dummy for whether an accident occurred. We can worry about numbers of accidents later
duplicates drop fips date, force

* Declare the data as panel data.
xtset fips date

* Generate heating- and cooling-degree-day measurements to capture the fact that
* temperature changes could plausibly have very different effects depending
* on the base temperature; but first, transform from kelvin to celsius
replace mean_temperature = mean_temperature - 273.15
gen heat_degree_days = mean_temperature - 18 if mean_temperature > 18
replace heat_degree_days = 0 if heat_degree_days == .
gen cool_degree_days = 18 - mean_temperature if mean_temperature < 18
replace cool_degree_days = 0 if cool_degree_days == .

********* End basic fixes to data file, applicable to every regression *********

* Test out ivprobit. See https://www.statalist.org/forums/forum/general-stata-discussion/general/1305265-instrumental-variable-probit-using-panel-data
* for Jeff Wooldridge saying that clustering on the panel identifier allows for
* valid inference with panel data using ivprobit. It is a correlated random
* effects approach rather than fixed effects, because unconditional fixed 
* effects estimates are biased.
eststo: ivprobit accident_occurred heat_degree_days cool_degree_days mean_precipitation population i.weekday (mean_pm25 = *_lapse pbl_height), vce(cluster fips) first
version 14.0: eststo: margins, dydx(_all) predict(pr)

* Don't remember why I did this. Maybe I'll be smart if I remember and put an explanatory note.
preserve
drop if weekday == 1 | weekday == 7
eststo: ivprobit accident_occurred heat_degree_days cool_degree_days mean_precipitation population i.weekday (mean_pm25 = *_lapse pbl_height), vce(cluster fips) first
version 14.0: eststo: margins, dydx(_all) predict(pr)
restore

* Save the stored regressions as latex and rtf tables, then clear them so I can save the next batch.
esttab using `industry'_iv_probit_models.tex, replace label mtitles booktabs alignment(D{.}{.}{-1}) title(IV Probit Models\label{tab1})
esttab using `industry'_iv_probit_models.rtf, replace label mtitles nogap onecell
eststo clear

* Close the log
log close main
