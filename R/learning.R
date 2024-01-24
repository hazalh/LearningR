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


# filtering data by row ---------------------------------------------------

filter(NHANES_small, phys_active == "No")

NHANES_small %>% filter(phys_active == "No")

NHANES_small %>%
    select(phys_active) %>%
    filter(phys_active != "No") #phys_active == "Yes" and == "NA"

NHANES_small %>% select(bmi) %>%
    filter(bmi >=25)

NHANES_small %>% select(bmi, phys_active) %>%
    filter(bmi == 25 & phys_active == "No") #11 rows

NHANES_small %>% select(bmi, phys_active) %>%
    filter(bmi == 25 | phys_active == "No") #3701 rows


# arranging rows ----------------------------------------------------------

NHANES_small %>% arrange(age) #ascending order
NHANES_small %>% arrange(desc(age)) #descending order

NHANES_small %>% select(education, age, sex) %>%
    arrange(education)

NHANES_small %>% select(education, age) %>%
    arrange(education, age) #arrange by education first, and then by age


# transform or add columns ------------------------------------------------

NHANES_small %>% mutate(age = age *12) #age column is overwritten by age*12

NHANES_small %>% mutate(bmi_log = log(bmi),
                        age = age *12) %>%
    select(bmi, bmi_log, age)

NHANES_small %>% mutate(old = if_else(age >= 30, "Yes", "No")) %>%
    select(age, old)


# exercise 7.12  ----------------------------------------------------------

#1
NHANES_small %>% select(bmi, diabetes) %>%
    filter(bmi >= 20 & bmi <= 40) #7408 rows

#1 correct answer: to select cases with bmi >= 20 and bmi <=40 who have diabaetes
NHANES_small %>% select(bmi, diabetes) %>%
    filter(bmi >= 20 & bmi <= 40, diabetes == "Yes") #616 rows

#2
NHANES_mofidied <- NHANES_small %>%
    mutate(mean_arterial_pressure = ((2 * bp_dia_ave) + bp_sys_ave)/3, #new column with mean arterial pressure
           young_child = if_else(age < 6, "Yes", "No") #new column with young child info
                                 ) #%>%
    select(young_child, mean_arterial_pressure)

NHANES_mofidied
