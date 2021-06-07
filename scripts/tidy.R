# explore data of the US Census for AZ legislative district 7
# Rene Dario Herrera 
# June 2021

# setup ----
# load packages 
library(here)
library(tidycensus)
library(tidyverse)

# inspect the acs
acs5_2019_var <- load_variables(2019, "acs5", cache = TRUE)
acs5_2019_var_subject <- load_variables(2019, "acs5/subject", cache = TRUE)
acs5_2019_var_profile <- load_variables(2019, "acs5/profile", cache = TRUE)

# read data: total population by sex ----
population_by_sex <- get_acs(
  geography = "congressional district",
  variables = c("total_pop" = "B01001_001",
                "total_pop_male" = "B01001_002",
                "total__pop_female" = "B01001_026"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5",
  state = "az"
)

# read data: total population by sex spatial ----
population_spatial <- get_acs(
  geography = "congressional district",
  variables = c("total_population" = "B01001_001"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5",
  state = "az",
  geometry = TRUE
)

# inspect
glimpse(population_by_sex)

# average total population for each district
population_by_sex %>%
  group_by(GEOID) %>%
  filter(variable == "total_pop") %>%
  summarize(total_population = mean(estimate))

# average total male population for each district
population_by_sex %>%
  group_by(GEOID) %>%
  filter(variable == "total_pop_male") %>%
  summarize(total_male_population = mean(estimate))

# average total female population for each district
population_by_sex %>%
  group_by(GEOID) %>%
  filter(variable == "total_pop_female") %>%
  summarize(total_female_population = mean(estimate))

# plot total population by sex for each district
population_by_sex %>%
  filter(variable != "total_pop") %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate, fill = variable)) +
  coord_flip() +
  labs(title = "Total Population for Each AZ Legislative District",
       subtitle = "Total Population")

# plot male population for each district
population_by_sex %>%
  filter(variable == "total_pop_male") %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate)) +
  coord_flip() +
  labs(title = "Total Population for Each AZ Legislative District",
       subtitle = "Male Sex Only")

# plot female population for each district
population_by_sex %>%
  filter(variable == "total_pop_female") %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate)) +
  coord_flip() +
  labs(title = "Total Population for Each AZ Legislative District",
       subtitle = "Female Sex Only")

# read data: median age by sex ----
median_age_by_sex <- get_acs(
  geography = "congressional district",
  variables = c("median_age_pop" = "B01002_001",
                "median_age_pop_male" = "B01002_002",
                "median_age_pop_female" = "B01002_003"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5",
  state = "az"
)

# inspect
glimpse(median_age_by_sex)

# median age of each district
median_age_by_sex %>%
  group_by(GEOID) %>%
  filter(variable == "median_age_pop") 

# plot 
median_age_by_sex %>%
  filter(variable == "median_age_pop") %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate)) +
  coord_flip() +
  labs(title = "Median Age for Each AZ Legislative District",
       subtitle = "Total Population Only")

# plot 
median_age_by_sex %>%
  filter(variable == "median_age_pop_male") %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate)) +
  coord_flip() +
  labs(title = "Median Age for Each AZ Legislative District",
       subtitle = "Male Population Only")

# plot 
median_age_by_sex %>%
  filter(variable == "median_age_pop_female") %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate)) +
  coord_flip() +
  labs(title = "Median Age for Each AZ Legislative District",
       subtitle = "Female Population Only")

# read data: internet usage ----
internet_usage <- get_acs(
  geography = "congressional district",
  variables = c("total_household" = "B28011_001",
                "broadband" = "B28011_004"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5",
  state = "az"
)

# inspect
glimpse(internet_usage)

# tidy data 
# internet_usage <- internet_usage %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = c("estimate", "moe")
#   ) 

# summarize 
internet_usage %>%
  pivot_wider(
    names_from = variable,
    values_from = c("estimate", "moe")
  ) %>%
  group_by(NAME) %>%
  transmute(percentage_broadband = estimate_broadband / estimate_total_household)

# plot 
internet_usage %>%
  pivot_wider(
    names_from = variable,
    values_from = c("estimate", "moe")
  ) %>%
  group_by(NAME) %>%
  transmute(percentage_broadband = estimate_broadband / estimate_total_household) %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, percentage_broadband), y = percentage_broadband)) +
  ylim(0,1) +
  coord_flip() +
  labs(title = "Broadband Internet for Each AZ Legislative District",
       subtitle = "Percentage of Total Households")

# read data: median household income ----
income <- get_acs(
  geography = "congressional district",
  variables = c("median_household_income" = "S1903_C03_001"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5",
  state = "az"
)

# inspect
glimpse(income)

# view 
income

# plot 
income %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate)) +
  coord_flip() +
  labs(title = "Median Household Income for Each AZ Legislative District",
       subtitle = "in 2015 inflation adjusted dollars")

# read data: employment 
employment <- get_acs(
  geography = "congressional district",
  variables = c("employed_percent" = "DP03_0004P"),
  cache_table = TRUE,
  year = 2019,
  survey = "acs5",
  state = "az"
)

# inspect 
glimpse(employment)

# plot 
employment %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(NAME, estimate), y = estimate)) +
  coord_flip() +
  labs(title = "Percent Employed for Each AZ Legislative District",
       subtitle = "Population 16 years and over; civilian labor force")

# population per square mile ---- 
glimpse(population_spatial)

# join all data frames in to one data frame ----
az_congressional_demographics <- bind_rows(employment, 
          income,
          internet_usage,
          median_age_by_sex,
          population_by_sex)
