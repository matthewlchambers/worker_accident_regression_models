clear all

/* 1. Get shapefile */
cd "E:/Research Projects/Worker Accidents and Pollution/Data/Maps"
/* 2. Convert shapefile to Stata attribute and coordinate datasets */
shp2dta using "../County Shapefile/cb_2016_us_county_20m.shp", data("attributes.dta") coord("coordinates.dta") ///
genid(stid) gencentroids(cc) replace
/* 3. Draw map */
use attributes, clear

rename STATEFP statefip
destring statefip, force replace
drop if statefip == 2 | statefip == 15 | statefip > 56

save, replace
clear all
import delimited "../Data for Regression Models/construction_accidents_2003_to_2015.csv", varnames(1) numericcols(3/10)

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

destring total_employment, force replace

gen high_inversion_pm25 = mean_pm25 if inversion_coverage > 0.75
gen low_inversion_pm25 = mean_pm25 if inversion_coverage < 0.25
gen construction_share = employment / total_employment
gen emp_per_est = employment / qtrly_estabs
gen inj_per_thousand = num_injured * 1000 / employment
gen accidents_per_thousand = num_accidents * 1000 / employment

gen year = year(date)
merge m:1 fips year using "../Population Density/annual_county_population_estimates.dta"

gen construction_per_pop = employment / population
gen accidents_per_pop = num_accidents / population

collapse (mean) inversion_coverage mean_temperature mean_precipitation mean_pm25 ///
	qtrly_estabs employment total_employment construction_share emp_per_est construction_per_pop accidents_per_pop ///
	high_inversion_pm25 low_inversion_pm25 (sd) deviation_pm25=mean_pm25 (sum) num_injured num_accidents ///
	inj_per_thousand accidents_per_thousand, by(fips)

gen ann_mean_inj_per_thousand = inj_per_thousand / 13
gen ann_mean_accidents_per_thousand = accidents_per_thousand / 13

preserve

gen more_than_five_thousand = (employment > 5000)
gen more_than_ten_percent = (construction_share > 0.1)

keep fips more_than_five_thousand more_than_ten_percent
merge 1:1 fips using attributes, keep(match)
keep stid more_than_five_thousand more_than_ten_percent
rename stid _ID
save temp_flags, replace
clear
use coordinates
gen row_num = _n
merge m:1 _ID using temp_flags, keep(match)
sort row_num
drop row_num
save flagged_coordinates, replace

restore
//
// preserve
//
// gen deviation_pm25_cat = ""
//
// replace deviation_pm25_cat = "[6, infinity)" if deviation_pm25 >= 6 & deviation_pm25_cat == ""
// replace deviation_pm25_cat = "[3, 6)" if deviation_pm25 >=3 & deviation_pm25 < 6 & deviation_pm25_cat == ""
// replace deviation_pm25_cat = "[0, 3)" if deviation_pm25 < 3 & deviation_pm25_cat == ""
//
// keep fips deviation_pm25_cat
// merge 1:1 fips using attributes, keep(match)
// keep stid deviation_pm25_cat
// rename stid _ID
// save temp_flags, replace
// clear
// use coordinates
// gen row_num = _n
// merge m:1 _ID using temp_flags, keep(match)
// sort row_num
// drop row_num
// save flagged_coordinates, replace
//
// restore

merge 1:1 fips using attributes, keep(match using)

// spmap mean_pm25 using coordinates, id(stid) clmethod(custom) clbreaks(2 4 6 8 10 12 14) fcolor(Reds) ///
// 	ocolor(black) osize(vthin) legend(position(2)) ///
// 	legtitle("Mean PM 2.5") legend(pos(4))
// graph export "mean_pm25_all.pdf", replace

// spmap mean_pm25 using coordinates, id(stid) clmethod(custom) clbreaks(2 4 6 8 10 12 14) fcolor(Blues) ///
// 	ocolor(white ...) osize(vvthin ...) legend(position(2)) ///
// 	legtitle("Mean PM 2.5") legend(pos(4)) ///
// 	polygon(data(flagged_coordinates) by(deviation_pm25_cat) ocolor(white gray black) osize(vthin thin medthin) ///
// 		legenda(on) legtitle("PM 2.5 St. Dev."))
// graph export "mean_and_deviation_pm25_all.pdf", replace

// spmap mean_pm25 using coordinates, id(stid) clmethod(custom) clbreaks(2 4 6 8 10 12 14) fcolor(Reds) ///
// 	ocolor(black) osize(vthin) legend(position(2)) ///
// 	legtitle("Mean PM 2.5") legend(pos(4))
// graph export "mean_pm25_all.pdf", replace
//
// spmap inversion_coverage using coordinates, id(stid) clmethod(stdev) clnumber(8) fcolor(BuRd) ///
// 	ocolor(black) osize(vthin) legend(position(2)) ///
// 	legtitle("Mean PM 2.5") legend(pos(4))
// graph export "inversion_coverage_all.pdf", replace
//
// spmap employment using coordinates, id(stid) clnumber(9) fcolor(Greens) ///
// 	ocolor(black) osize(vthin) legend(position(2)) ///
// 	legtitle("Construction Employment") legend(pos(4))
// graph export "construction_employment_all.pdf", replace
//
// spmap construction_share using coordinates, id(stid) clnumber(9) fcolor(Greens) ///
// 	ocolor(black) osize(vthin) legend(position(2)) ///
// 	legtitle("Construction Employment") legend(pos(4))
// graph export "construction_employment_all.pdf", replace
//
// spmap ann_mean_inj_per_thousand using coordinates, id(stid) clmethod(custom) clbreaks(0 0.2 0.4 0.6 0.8 1 12) fcolor(Reds) ///
// 	ocolor(black) osize(vthin) legend(position(2)) ///
// 	legtitle("Mean Annual Injuries Per Thousand Employees") legend(pos(4))
// graph export "ann_mean_inj_per_thousand.pdf", replace
//
// spmap employment using coordinates, id(stid) clmethod(custom) clbreaks(0 2000 4000 6000 8000 10000 12000 200000) fcolor(Blues) ///
// 	ocolor(white ...) osize(vthin) legend(position(2)) ///
// 	legtitle("Mean # of Construction Employees") legend(pos(4)) ///
// // 	polygon(data(flagged_coordinates) select(keep if more_than_ten_percent) ocolor(black) osize(thin))
// graph export "Employment_map.pdf", replace
//
spmap num_accidents using coordinates, id(stid) clmethod(custom) clbreaks(0 10 20 30 40 50 2000) fcolor(Reds) ///
	ocolor(white ...) osize(vthin) legend(position(2)) ///
	legtitle("Mean # of Construction Employees") legend(pos(4)) ///
// 	polygon(data(flagged_coordinates) select(keep if more_than_ten_percent) ocolor(black) osize(thin))
graph export "accidents_map.pdf", replace
//
// spmap construction_per_pop using coordinates, id(stid) clmethod(custom) clbreaks(0 0.01 0.02 0.03 0.04 0.05 0.06 1) fcolor(Greens) ///
// 	ocolor(white ...) osize(vthin) legend(position(2)) ///
// 	legtitle("Construction Employees per Population") legend(pos(4)) ///
// // 	polygon(data(flagged_coordinates) select(keep if more_than_ten_percent) ocolor(black) osize(thin))
// graph export "construction_per_pop_map.pdf", replace

// replace accidents_per_pop = accidents_per_pop * 1000000
//
// spmap accidents_per_pop using coordinates, id(stid) clmethod(custom) clbreaks(0 0.01 0.02 0.03 0.04 0.05 0.06 1) fcolor(Oranges) ///
// 	ocolor(white ...) osize(vthin) legend(position(2)) ///
// 	legtitle("Construction Accidents per 100,000 Population") legend(pos(4)) ///
// // 	polygon(data(flagged_coordinates) select(keep if more_than_ten_percent) ocolor(black) osize(thin))
// graph export "accidents_per_pop_map.pdf", replace
