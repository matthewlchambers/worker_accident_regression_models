* Start with a clean slate.
log close _all
clear all

/* 1. Get shapefile */
cd "E:/Research Projects/Worker Accidents and Pollution/Data/Maps"
/* 2. Convert shapefile to Stata attribute and coordinate datasets */
shp2dta using "../County Shapefile/cb_2016_us_county_20m.shp", data("attributes.dta") coord("coordinates.dta") ///
genid(stid) gencentroids(cc) replace
use attributes, clear

rename STATEFP statefip
destring statefip, force replace
drop if statefip == 2 | statefip == 15 | statefip > 56

save, replace
clear all

* Set the industry.
local industry "manufacturing"

* Start logging
log using `industry'_summary_statistics.log, replace name(main)

* Import the clean data file, produced using R. I'm using Stata for the analysis.
* because Stata works with panel data a little easier.
import delimited "../Data for Regression Models/`industry'_accidents_2003_to_2015.csv", varnames(1) numericcols(3/25)

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

destring total_employment, force replace

collapse (mean) inversion_coverage mean_temperature mean_precipitation mean_pm25 ///
	qtrly_estabs employment total_employment ///
	(sd) deviation_pm25=mean_pm25 (sum) num_injured num_accidents, by(fips)

merge 1:1 fips using attributes, keep(match using)

spmap employment using coordinates, id(stid) clmethod(custom) clbreaks(0 2000 4000 6000 8000 10000 12000 20000) fcolor(Blues) ///
	ocolor(white ...) osize(vthin) legend(position(2)) ///
	legtitle("Manufacturing Employment") legend(pos(4))
graph export "manufacturing_employment_map.pdf", replace

spmap num_accidents using coordinates, id(stid) clmethod(custom) clbreaks(0 10 20 30 40 50 2000) fcolor(Reds) ///
	ocolor(white ...) osize(vthin) legend(position(2)) ///
	legtitle("Manufacturing Accidents") legend(pos(4))
graph export "manufacturing_accidents_map.pdf", replace

* Close log
log close main
