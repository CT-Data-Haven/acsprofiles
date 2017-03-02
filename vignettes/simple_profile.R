## ----setup, warning=FALSE, message=FALSE, echo=FALSE---------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)

## ----libraries-----------------------------------------------------------
library(acsprofiles)
library(dplyr)
library(purrr)

## ----variables-----------------------------------------------------------
data("nhv_neighborhoods")
data("nhv_tracts")
data("regions")

# get neighborhoods that include block groups for Dwight & Hill
bg <- nhv_neighborhoods[c("dwight", "hill")]

# get neighborhoods defined only by tract for Dwight & Hill
tracts <- nhv_tracts[c("dwight", "hill")]

# copy Greater New Haven region from regions
gnh <- regions$`Greater New Haven`

# run get_town_names to save lookup list of all CT towns
ct_towns <- get_town_names(state = 9)

# save table numbers needed for convenience
# B01003: total population, available by block group
# C17002: ratio of income to poverty, available by block group
# B17024: ratio of income to poverty by sex & age, available only by tract
pop_table_num <- "B01003"
pov_table_num <- "C17002"
pov_age_table_num <- "B17024"

## ----city_geo------------------------------------------------------------
nhv_geo <- geo.make(state = 9, county = 9, county.subdivision = "New Haven")

## ----get_pops------------------------------------------------------------
# keeping defaults of state = 9, county = 9 for New Haven County, CT
nhood_pop <- neighborhood_table(neighborhood_list = bg, table.number = pop_table_num, year = 2015, blocks = T)

city_pop <- acs.fetch(geography = nhv_geo, endyear = 2015, table.number = pop_table_num, col.names = "pretty")
geography(city_pop)$NAME <- "New Haven"

gnh_pop <- regional_table(gnh, name = "Greater New Haven", town_lookup = ct_towns, table.number = pop_table_num, year = 2015)

all_pops <- list(nhood_pop, city_pop, gnh_pop) %>% reduce(rbind.acs)
acs.colnames(all_pops) <- "num_total_pop"

# clean up
rm(nhood_pop, city_pop, gnh_pop)

## ----get_poverty_rates---------------------------------------------------
nhood_pov <- neighborhood_table(neighborhood_list = bg, table.number = pov_table_num, year = 2015, blocks = T)

city_pov <- acs.fetch(geography = nhv_geo, endyear = 2015, table.number = pov_table_num, col.names = "pretty")
geography(city_pov)$NAME <- "New Haven"

gnh_pov <- regional_table(gnh, name = "Greater New Haven", town_lookup = ct_towns, table.number = pov_table_num, year = 2015)

all_povs <- list(nhood_pov, city_pov, gnh_pov) %>% reduce(rbind.acs)
rm(nhood_pov, city_pov, gnh_pov)

