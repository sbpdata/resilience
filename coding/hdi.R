library(WDI)
library(tidyverse)

# output path
output_path <- "../data/hdi.csv"

# read data from package
hdi <- as_tibble(WDI(country = "all", indicator = "UNDP.HDI.XD", extra = TRUE)) %>%
	filter(!is.na(iso3c))

# rename + drop vars
hdi_tidy <- hdi %>% 
	select(year, country = iso3c, hdi = UNDP.HDI.XD)

# write file
write_csv(hdi_tidy, output_path)
