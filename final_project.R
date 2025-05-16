library(lubridate)
library(janitor) 
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
# Import datasets to variable names

crimeData <- read.csv("US_Crime_DataSet.csv")
minWage <- read.csv("Minimum Wage Data.csv")
povertyRate <- read.csv("Poverty_rate_CPS.csv")

# Evaluate the structure of the datasets

str(crimeData)
str(minWage)
str(povertyRate)

# Clean attribute names

crimeData <- crimeData %>% janitor::clean_names()
minWage <- minWage %>% janitor::clean_names()
povertyRate <- povertyRate %>% janitor::clean_names()

# Add a year attribute to povertyRate
povertyRate <- povertyRate %>%
  rename(temp_date = date)
povertyRate$year <- as.numeric(format(as.Date(povertyRate$temp_date), "%Y"))
povertyRate$temp_date <- NULL

# Pivot povertyRate to match state attributes 
povertyRate_long <- povertyRate %>%
  pivot_longer(
    cols = -year,
    names_to = "state",
    values_to = "poverty rate"
  )


# Group by year and state, then filter timeframe
povertyRate <- povertyRate_long %>%
  filter(
    year %in% c(1980:2015),
    !is.na(state),
    !is.na(year),
    !state %in% c("district_of_columbia", "united_states")
  ) %>%
  select(year, state, `poverty rate`) %>%
  group_by(year, state)

povertyRate$state <- str_to_title(povertyRate$state)

# Group homicide by state and then by year for 20 years
crime_by_year <- crimeData %>%
  select(state, year, incident) %>%
  filter(year %in% c(1980:2015), !is.na(state), !is.na(year)) %>%
  group_by(year, state) %>%
  summarize(total = sum(incident))

# Group wages by year and then by state to create the same order as the crime database
min_wage_by_year <- minWage %>%
  select(state, year, state_minimum_wage, state_minimum_wage_2020_dollars, federal_minimum_wage, federal_minimum_wage_2020_dollars, cpi_average) %>%
  filter(year %in% c(1980:2015)) %>%
  group_by(state, year)

# Create homicide dataframes for the five most populous states (California, Texas, New York, Florida and Pennsylvania)
new_york_crime <- crime_by_year %>% filter(state == "New York")
texas_crime <- crime_by_year %>% filter(state == "Texas")
california_crime <- crime_by_year %>% filter(state == "California")
florida_crime <- crime_by_year %>% filter(state == "Florida")
pennsylvania_crime <- crime_by_year %>% filter(state == "Pennsylvania")

# Create minimum wage dataframes for the five most populous states
new_york_min_wage <- min_wage_by_year %>% filter(state == "New York")
texas_min_wage <- min_wage_by_year %>% filter(state =="Texas")
california_min_wage <- min_wage_by_year %>% filter(state == "California")
florida_min_wage <- min_wage_by_year %>% filter(state == "Florida")
pennsylvania_min_wage <- min_wage_by_year %>% filter(state == "Pennsylvania")



# Join dataframes 
merged_df <- povertyRate %>%
  left_join(crimeData, by = c("state", "year")) %>%
  left_join(minWage, by = c("state", "year"))

head(merged_df)
