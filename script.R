library(readr)
library(readxl)
library(janitor)
library(tidyverse)

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
                   ))

enrollment <- read_csv("enrollment_ex1.csv",
                       col_types = cols(
                         id = col_double(),
                         age = col_double(),
                         female = col_double(),
                         cap = col_double(),
                         diab_program_ind1 = col_double(),
                         diab_program_ind2 = col_double(),
                         diab_program_ind3 = col_double(),
                         diab_program_ind4 = col_double(),
                         diab_program_ind5 = col_double(),
                         diab_program_ind6 = col_double(),
                         diab_program_ind7 = col_double(),
                         diab_program_ind8 = col_double(),
                         diab_program_ind9 = col_double(),
                         diab_program_ind10 = col_double(),
                         diab_program_ind11 = col_double(),
                         diab_program_ind12 = col_double(),
                         race = col_character(),
                         zip = col_double()
                       ))

income_by_zip <- read_csv("income_by_zip_ex1.csv",
                          col_types = cols(
                            med_income = col_double(),
                            zip_code = col_double()
                          ))