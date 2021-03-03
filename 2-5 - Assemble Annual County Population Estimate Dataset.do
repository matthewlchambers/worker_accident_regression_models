clear all

/* Set working directory */
cd "E:/Research Projects/Worker Accidents and Pollution/Data/Population Density"

/* Get county population estimates csv for 2010-2019 */
import delimited co-est2019-alldata.csv

/* Drop states and state-level entities that aren't 
in my analysis */
drop if state == 2 | state == 15 | state > 56

/* Create a single fips code variable from the 
existing state and county ones*/
gen fips = state * 1000 + county

/* Drop variables I don't need */
keep fips popestimate*

/* Reshape, since the population estimates for each 
year are given as separate variables */
reshape long popestimate, i(fips) j(year)

/* Rename popestimate to population */
rename popestimate population

/* Save */
save annual_county_population_2010_2019, replace

clear

/* Get county population estimates csv for 2000-2010 */
import delimited co-est00int-tot.csv

/* Drop states and state-level entities that aren't 
in my analysis */
drop if state == 2 | state == 15 | state > 56

/* Create a single fips code variable from the 
existing state and county ones*/
gen fips = state * 1000 + county

/* Drop variables I don't need */
keep fips popestimate*

/* Reshape, since the population estimates for each 
year are given as separate variables */
reshape long popestimate, i(fips) j(year)

/* Rename popestimate to population */
rename popestimate population

/* Drop 2010 data, since the other file has that as well */
drop if year == 2010

/* Might as well save this one too */
save annual_county_population_2000_2010, replace

/* Append the previously created dataset */
append using annual_county_population_2010_2019

/* Save the full set of annual county population estimates */
save annual_county_population_estimates, replace

clear all
