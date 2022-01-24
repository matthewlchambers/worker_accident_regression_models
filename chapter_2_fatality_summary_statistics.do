* Start with a clean slate.
log close _all
clear all

***************** Make sure the needed packages are installed ******************

* Install estout to get nice output from regressions
ssc install estout

************************** Done installing packages ****************************

** Make changes below to switch between local and ACCRE, & between industries **

* Set the working directory.
cd "E:/Research Projects/Worker Accidents and Pollution/Regression Models"

* Set the industry.
local industry "manufacturing"

* Start logging
log using `industry'_summary_statistics.log, replace name(main)

* Import the clean data file, produced using R. I'm using Stata for the analysis.
* because Stata works with panel data a little easier.
import delimited "../Data/Data for Regression Models/Fatality Only/`industry'_accidents_2003_to_2015.csv", varnames(1) numericcols(3/25)

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

* Declare the data as panel data.
xtset fips date

* Make a binary for an accident occurring.
gen accident_occurred = 1 if num_accidents > 0
replace accident_occurred = 0 if accident_occurred == .

* Generate heating- and cooling-degree-day measurements to capture the fact that
* temperature changes could plausibly have very different effects depending
* on the base temperature; but first, transform from kelvin to celsius
replace mean_temperature = mean_temperature - 273.15
gen heat_degree_days = mean_temperature - 18 if mean_temperature > 18
replace heat_degree_days = 0 if heat_degree_days == .
gen cool_degree_days = 18 - mean_temperature if mean_temperature < 18
replace cool_degree_days = 0 if cool_degree_days == .

********* End basic fixes to data file, applicable to every regression *********

gen season = .
replace season = 4 if month == 12 | month == 1  | month == 2
replace season = 1 if month == 3  | month == 4  | month == 5
replace season = 2 if month == 6  | month == 7  | month == 8
replace season = 3 if month == 9  | month == 10 | month == 11

* Get summary statistics by season
estpost tabstat accident_occurred mean_pm25 pbl_height *_lapse heat_degree_days cool_degree_days mean_precipitation wind_speed relative_humidity employment, by(season) statistics(mean sd) columns(statistics)
esttab using chapter_2_fatality_only_summary_stats_season.tex, main(mean) aux(sd) nostar noobs unstack nonote replace label nomtitles booktabs alignment(D{.}{.}{-1}) title(Summary Statistics by Season\label{sumstats_season})
esttab using chapter_2_fatality_only_summary_stats_season.rtf, main(mean) aux(sd) nostar noobs unstack nonote replace label nomtitles nogap onecell

// * Get summary statistics by season, weighted by employment
// estpost tabstat accident_occurred mean_pm25 inversion_coverage mean_temperature mean_precipitation employment [fweight=employment], by(season) statistics(mean sd) columns(statistics)
// esttab using summary_stats_season_emp_weighted.tex, main(mean) aux(sd) nostar noobs unstack nonote replace label nomtitles booktabs alignment(D{.}{.}{-1}) title(Summary Statistics by Season, Weighted by Employment\label{sumstats_season_emp_weighted})
// esttab using summary_stats_season_emp_weighted.rtf, main(mean) aux(sd) nostar noobs unstack nonote replace label nomtitles nogap onecell

* Close log
log close main
