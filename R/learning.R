# loading packages --------------------------------------------------------
library(tidyverse)
library(NHANES)

glimpse(NHANES) # check tibble: 10.000 rows and 76 columns
# str(NHANES)


# select specific columns -------------------------------------------------
select(NHANES, Age, Weight, BMI)

select(NHANES, -Depressed) # 75 columns

select(NHANES, starts_with("BP")) # all column names starting with BP characters
select(NHANES, ends_with("Day"))
select(NHANES, contains("Age"))


# rename for all columns by snakecase------------------------------------------------------------------
NHANES_small <- rename_with(NHANES, snakecase::to_snake_case) # colnames are changed to snakecase format: id survey_yr,
# gender, age, age_decade, age_months, race_1, race_3, education,marital_status

# rename specific column--------------------------------------------------
rename(NHANES_small, sex = gender) # without overwriting it, gender will not be replaced by sex.
colnames(NHANES_small)

NHANES_small <- rename(NHANES_small, sex = gender)


# chaining functions with pipe --------------------------------------------
colnames(NHANES_small)

NHANES_small %>% colnames()

NHANES_phys <- NHANES_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)


# exercise 7.8 ------------------------------------------------------------
#1
nhanes_small <- NHANES %>%
  rename_with(snakecase::to_snake_case) %>%
  select(bp_sys_ave, education)

#2
nhanes_small_bp <- NHANES %>%
  rename_with(snakecase::to_snake_case) %>%
  select(contains("ave")) %>%
  rename(
    bp_sys = bp_sys_ave,
    bp_dia = bp_dia_ave
  )

#3
nhanes_small_bmi_age <- NHANES %>%
  rename_with(snakecase::to_snake_case) %>%
  select(bmi, contains("age"))

#4
nhanes_small_bloodpressure <- NHANES %>%
  rename_with(snakecase::to_snake_case) %>%
  select(contains("bp_")) %>%
  rename(bp_systolic = bp_sys_ave)
