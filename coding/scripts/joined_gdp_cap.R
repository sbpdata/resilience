library(here)
library(tidyverse)
library(countrycode)
library(haven)
library(pwt9)
library(WDI)

pwt <- as_tibble(pwt9.1)

pwt_tidy <- pwt %>% 
  select(isocode, year, rgdpna, pop) %>% 
  mutate(pwt_gdp_cap = rgdpna / pop) %>% 
  mutate_if(is.factor, as.character) %>%
  select(isocode, year, pwt_gdp_cap)

maddison_path = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2018.dta"

mad <- read_stata(maddison_path)

mad_tidy <- mad %>%
	select(isocode = countrycode, year, mad_gdp_cap = rgdpnapc)

wdi <- as_tibble(
  WDI(
    country = "all",
    indicator = "NY.GDP.PCAP.PP.CD",
    extra = TRUE,
    start = 1960)
)

wdi_tidy <- wdi %>%
  select(isocode = iso3c, year, wdi_gdp_cap = NY.GDP.PCAP.PP.CD) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(year = as.double(year))

wdi_tidy %>% filter(year == 1989) %>% filter(!is.na(wdi_gdp_cap))

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

# NOT RUN:
# dropped_obs <- joined %>% 
 # filter(isocode %in% setdiff(unique(joined$isocode), unique(joined$iso_new))) %>% pull(isocode) %>%
 # unique()

joined_clean <- joined_clean %>%
  select(-iso_new, wdi = wdi_gdp_cap, mad = mad_gdp_cap, pwt = pwt_gdp_cap) %>%
  gather(c(wdi, mad, pwt), key = series, value = gdp_cap) %>%
  group_by(series, isocode) %>%
  arrange(year) %>%
  mutate(gdp_change = lead(gdp_cap, n = 1L) / gdp_cap) %>%
  ungroup()

output_dir <- here("data/joined_gdp_cap.csv")

write_csv(joined_clean, output_dir)
