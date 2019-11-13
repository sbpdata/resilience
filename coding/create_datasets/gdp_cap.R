# This file reads gdp / cap data from three sources (WDI, maddison, PWT), formats and saves it.

library(here)
library(tidyverse)
library(countrycode)
library(haven)

## DATA SOURCES ##
# pwt9 contains the penn world tables v9
library(pwt9)
# wdi contains api for wdi
library(WDI)
# Maddison data is downloaded from:
# https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2018 

maddison_path = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2018.dta"

## OUTPUT PATH
output_dir <- here("/data/joined_gdp_cap.csv")

################
### WDI data ###
################

# WDI data is from: https://databank.worldbank.org/source/world-development-indicators#
# Measured in constant 2011 intl dollars (PPP)

wdi <- as_tibble(WDI(country = "all", 
		      indicator = "NY.GDP.PCAP.PP.CD",
		      extra = TRUE,
		      start = NULL))

wdi_tidy <- wdi %>%
	select(isocode = iso3c, year, wdi_gdp_cap = NY.GDP.PCAP.PP.CD) %>%
	mutate_if(is.factor, as.character) %>%
	mutate(year = as.double(year))

#####################
### Maddison data ###
#####################

# Maddison data is from: https://www.rug.nl/ggdc/historicaldevelopment/maddison/
mad <- read_stata(maddison_path)

# for computing relative growth rates, authors recommend the RGDPNApc
mad_tidy <- mad %>%
	select(isocode = countrycode, year, mad_gdp_cap = rgdpnapc)

################
### PWT data ###
################

# pwt data is from https://www.rug.nl/ggdc/productivity/pwt
pwt <- as_tibble(pwt9.1)

pwt_tidy <- pwt %>% 
	select(isocode, year, rgdpe, pop) %>% 
	mutate(pwt_gdp_cap = rgdpe / pop) %>% 
	mutate_if(is.factor, as.character) %>%
	select(isocode, year, pwt_gdp_cap)

#######################
### join the frames ###
#######################
joined <- wdi_tidy %>%
	full_join(mad_tidy) %>%
	full_join(pwt_tidy) 

# standardise country-names by running am external matching 
# function on the country variable and dropping all the non-matched 
# observations. This removes entries like "Arab World".
joined$iso_new <- countrycode(sourcevar = joined$isocode,
	     origin = 'iso3c',
	     destination = 'iso3c')

joined_clean <- joined %>%
	drop_na(iso_new)

dropped_obs <- joined %>% 
	filter(isocode %in% setdiff(unique(joined$isocode), unique(joined$iso_new))) %>% pull(isocode) %>%
	unique()

write_csv(joined_clean, output_dir)
