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

## ----get_pov_by_age------------------------------------------------------
nhood_pov2 <- neighborhood_table(neighborhood_list = tracts, table.number = pov_age_table_num, year = 2015, blocks = F)

city_pov2 <- acs.fetch(geography = nhv_geo, endyear = 2015, table.number = pov_age_table_num, col.names = "pretty")
geography(city_pov2)$NAME <- "New Haven"

gnh_pov2 <- regional_table(gnh, name = "Greater New Haven", town_lookup = ct_towns, table.number = pov_age_table_num, year = 2015)

all_pov_age <- list(nhood_pov2, city_pov2, gnh_pov2) %>% reduce(rbind.acs)
rm(nhood_pov2, city_pov2, gnh_pov2)

## ----calc_pov_rates------------------------------------------------------
# all_povs@acs.colnames
# need to add columns 2 and 3 to get population below poverty, and columns 2 to 7 to get low-income
# Column 1 will be the denominator, as this has the total population with a poverty status
pov_groups <- list(
  poverty = 2:3,
  low_inc = 2:7
)
pov_calculated <- calc_acs_table(groups = pov_groups, denom = all_povs[, 1], table = all_povs)
pov_rates <- cbind.acs(all_povs[, 1], pov_calculated)

# manually set nicer column names
acs.colnames(pov_rates) <- c("num_pov_status", "num_poverty", "per_poverty", "num_low_inc", "per_low_inc")
rm(pov_groups, pov_calculated)
pov_rates

## ----calc_pov_by_age-----------------------------------------------------
# all_pov_age@acs.colnames
# This table has 131 columns! They're broken into small age groups. The relevant ones:
# * cols 2, 15, and 28 add up to the poverty status for ages 0-17--no percentage is needed, so I'll just use apply to add them
# * cols 3-5, 16-18, and 29-31 add up to ages 0-17 below 1.00x poverty line
# * cols 3-10, 16-23, and 29-36 add up to ages 0-17 below 2.00x poverty line
pov_determined <- apply(X = all_pov_age[, c(2, 15, 28)], FUN = sum, MARGIN = 2, agg.term = "num_pov_status")
pov_age_groups <- list(
  poverty = c(3:5, 16:18, 29:31),
  low_inc = c(3:10, 16:23, 29:36)
)
child_pov_calculated <- calc_acs_table(groups = pov_age_groups, denom = pov_determined, table = all_pov_age)
child_pov_rates <- cbind.acs(pov_determined, child_pov_calculated)
acs.colnames(child_pov_rates) <- c("num_child_pov_status", "num_child_poverty", "per_child_poverty", "num_child_low_inc", "per_child_low_inc")
# clean up
rm(pov_determined, pov_age_groups, child_pov_calculated)
child_pov_rates

## ----bind_tables---------------------------------------------------------
all_tables <- list(all_pops, pov_rates, child_pov_rates) %>% reduce(cbind.acs)

profile_df <- data.frame(name = all_tables@geography$NAME, all_tables@estimate, all_tables@standard.error * qnorm(0.95), row.names = NULL) %>% 
  tbl_df() %>%
  mutate_at(vars(starts_with("per")), funs(round(., digits = 3))) %>%
  mutate_at(vars(starts_with("num")), funs(round(., digits = 0))) %>%
  arrange_est_moe(omit = 1) %>%
  mutate(name = stringr::str_to_title(name))
profile_df

## ----display_names-------------------------------------------------------
names(profile_df) <- c("Geography", "Total population", "MOE total population", "Population, poverty status determined", "MOE population poverty status determined", "Population in poverty", "MOE population in poverty", "Poverty rate", "MOE poverty rate", "Population low-income", "MOE population low-income", "Low-income rate", "MOE low-income rate", "Population ages 0-17, poverty status determined", "MOE population ages 0-17, poverty status determined", "Population ages 0-17 in poverty", "MOE population ages 0-17 in poverty", "Child poverty rate", "MOE child poverty rate", "Population ages 0-17 low-income", "MOE population ages 0-17 low income", "Child low-income rate", "MOE child low-income rate")

## ----graph, fig.width=4--------------------------------------------------
library(ggplot2)
library(tidyr)

# select rate columns and their margins of error
# make long format for ggplot
pov_df <- profile_df %>% 
  select(1, 8, 9, 18, 19) %>%
  mutate(`Poverty rate_upper` = `Poverty rate` + `MOE poverty rate`,
         `Poverty rate_lower` = `Poverty rate` - `MOE poverty rate`,
         `Child poverty rate_upper` = `Child poverty rate` + `MOE child poverty rate`,
         `Child poverty rate_lower` = `Child poverty rate` - `MOE child poverty rate`) %>%
  select(-`MOE poverty rate`, -`MOE child poverty rate`)
# splitting into rates and bounds in order to gather, then will stick back together
# use gather to make long format
rates <- pov_df %>% 
  select(Geography, `Poverty rate`, `Child poverty rate`) %>%
  gather(key = Measure, value = Rate, -Geography)
# use gather to make long format, separate to get type of measure & type of bound, then spread to make individual columns for upper & lower bounds
bounds <- pov_df %>% 
  select(Geography, 4:7) %>%
  gather(key = Measure, value = Bound, -Geography) %>%
  separate(Measure, into = c("Measure", "Bound_type"), sep = "_") %>%
  spread(key = Bound_type, value = Bound)

# join rates & bounds
pov_long <- inner_join(rates, bounds, by = c("Geography", "Measure"))
pov_long
rm(rates, bounds)

ggplot(pov_long, aes(x = Geography, y = Rate, fill = Measure)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
  facet_wrap(~ Measure, ncol = 1) +
  guides(fill = F) +
  coord_flip()

