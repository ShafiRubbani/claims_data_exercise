# Install relevant libraries

library(readr)
library(readxl)
library(janitor)
library(lubridate)
library(moderndive)
library(tidyverse)

# Read claims data from csv file, renaming some columns as appropriate and
# reformatting dates from STATA format

claims <- read_csv("claims_ex1.csv",
                   skip = 1,
                   col_names = c(
                     "principal_diagnosis_cleaned",
                     "allowed_amount_cleaned",
                     "visit_type",
                     "line",
                     "claim_id",
                     "claim_date",
                     "id",
                     "cap"),
                   col_types = cols(
                     principal_diagnosis_cleaned = col_character(),
                     allowed_amount_cleaned = col_double(),
                     visit_type = col_character(),
                     line = col_double(),
                     claim_id = col_double(),
                     claim_date = col_double(),
                     id = col_double(),
                     cap = col_double()
                   )) %>%
  mutate(claim_date = format(as.Date(claim_date, origin="1960-01-01"),"%Y-%m-%d"))

# Read enrollment data from csv file and rename enrollment columns by month number

enrollment <- read_csv("enrollment_ex1.csv",
                       skip = 1,
                       col_names = c("id",
                         "age",
                         "female",
                         "cap",
                         "01",
                         "02",
                         "03",
                         "04",
                         "05",
                         "06",
                         "07",
                         "08",
                         "09",
                         "10",
                         "11",
                         "12",
                         "race",
                         "zip"),
                       col_types = cols(
                         id = col_double(),
                         age = col_double(),
                         female = col_double(),
                         cap = col_double(),
                         `01` = col_double(),
                         `02` = col_double(),
                         `03` = col_double(),
                         `04` = col_double(),
                         `05` = col_double(),
                         `06` = col_double(),
                         `07` = col_double(),
                         `08` = col_double(),
                         `09` = col_double(),
                         `10` = col_double(),
                         `11` = col_double(),
                         `12` = col_double(),
                         race = col_character(),
                         zip = col_double()
                       ))

# Read income data from csv file

income_by_zip <- read_csv("income_by_zip_ex1.csv",
                          col_types = cols(
                            med_income = col_double(),
                            zip_code = col_double()
                          ))

# Reformat enrollment data to identify months enrolled

enrollment_modified <- enrollment %>% 
  gather(key = "month", value = "enrolled",
         `01`,
         `02`,
         `03`,
         `04`,
         `05`,
         `06`,
         `07`,
         `08`,
         `09`,
         `10`,
         `11`,
         `12`) %>% 
  filter(enrolled != 0) %>% 
  select(-enrolled) %>% 
  mutate(month = as.numeric(month))

# Identify relevant claim ID's based on criteria

diabetes_claim_ids <- claims %>% 
  filter(visit_type == "OFF") %>% 
  filter(str_detect(principal_diagnosis_cleaned, "250")) %>% 
  mutate(claim_month = as.numeric(month(claim_date))) %>% 
  semi_join(enrollment_modified, by = c("id" = "id", "claim_month" = "month")) %>% 
  select(claim_id) %>% 
  distinct() %>% 
  unlist()

# Calculate median income across zip codes

median_income <- income_by_zip %>% 
  summarize(median(med_income)) %>% 
  unlist()

# Calculate the individual total annual cost of claims meeting above criteria

individual_annual <- claims %>% 
  filter(claim_id %in% diabetes_claim_ids) %>% 
  # distinct() %>% 
  group_by(id) %>% 
  summarize(total = sum(allowed_amount_cleaned)) %>% 
  ungroup()

# Gather relevant variables for regression models

regression_variables <- individual_annual %>% 
  left_join(enrollment, by = c("id")) %>% 
  mutate(months_enrolled = 
           `01` +
           `02` +
           `03` +
           `04` +
           `05` +
           `06` +
           `07` +
           `08` +
           `09` +
           `10` +
           `11` +
           `12`) %>% 
  select(id, total, age, female, race, zip, months_enrolled) %>% 
  left_join(income_by_zip, by = c("zip" = "zip_code")) %>% 
  mutate(lower_SES = if_else(med_income < median_income, 1, 0)) %>% 
  mutate(race = if_else(is.na(race), "unknown", race)) %>% 
  mutate(race = fct_recode(race,"white" = "w", 
                           "black" = "b", 
                           "asian" = "a")) %>% 
  mutate(black = if_else(race == "black", 1, 0),
         asian = if_else(race == "asian", 1, 0),
         unknown = if_else(race == "unknown", 1, 0)) %>% 
  select(id, total, lower_SES, age, female, black, asian, unknown, months_enrolled)

# Create regression model

SES_vs_annual_costs <- lm(total ~ lower_SES + age + female + race + months_enrolled,
                          data = regression_variables)

# Calculate regression values

regression_table <- get_regression_table(SES_vs_annual_costs)
