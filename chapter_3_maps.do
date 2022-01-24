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
local industry "traffic"

* Start logging
log using `industry'_summary_statistics.log, replace name(main)

* Import the clean data file, produced using R. I'm using Stata for the analysis.
* because Stata works with panel data a little easier.
import delimited "../Data for Regression Models/`industry'_accidents_2003_to_2015.csv", varnames(1) numericcols(3/25)

********* End of section to change when switching between local and ACCRE ******

* Drop any observations from Alaska, Hawaii, or Puerto Rico.
drop if floor(fips / 1000) == 2 | floor(fips / 1000) == 15 | floor(fips / 1000) == 72

collapse (mean) population (sum) accident_occurred, by(fips)

merge 1:1 fips using attributes, keep(match using)

spmap population using coordinates, id(stid) clmethod(custom) clbreaks(0 20000 40000 60000 80000 100000 10000000) fcolor(Blues) ///
	ocolor(white ...) osize(vthin) legend(position(2)) ///
	legtitle("Population") legend(pos(4))
graph export "population_map.pdf", replace

spmap accident_occurred using coordinates, id(stid) clmethod(custom) clbreaks(0 200 400 600 800 1000 10000) fcolor(Reds) ///
	ocolor(white ...) osize(vthin) legend(position(2)) ///
	legtitle("Traffic Accidents") legend(pos(4))
graph export "traffic_accidents_map.pdf", replace

* Close log
log close main
