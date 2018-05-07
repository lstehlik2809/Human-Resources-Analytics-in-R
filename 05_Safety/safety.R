# Improving employee safety with data
# In many industries, workplace safety is a critical consideration. 
# Maintaining a safe workplace provides employees with confidence and 
# reduces costs for workers' compensation and legal liabilities. Let's
# look for explanations for an increase in workplace accidents.

# Loading the packages
library(tidyverse)
library(broom)
library(skimr)
library(lme4)

# Importing the data 
path_hr_data <- "./00_Data/hr_data_2.csv"
path_accident_data <- "./00_Data/accident_data.csv"

hr_data <- read_csv(path_hr_data)
accident_data <- read_csv(path_accident_data)

# Creating hr_joined with left_join() and mutate()
hr_joined <- hr_data %>% 
  left_join(accident_data, by = c("employee_id", "year")) %>%
  mutate(had_accident = ifelse(is.na(accident_type), 0, 1)) 

# Inspecting the data
glimpse(hr_joined)
summary(hr_joined)
skim(hr_joined)

# Finding accident rate for each year
hr_joined %>% 
  group_by(year) %>% 
  summarize(accident_rate = mean(had_accident))

# Testing difference in accident rate between years
chisq.test(hr_joined$year, hr_joined$had_accident)

# Which location had the highest acccident rate?
hr_joined %>%
  group_by(location) %>%
  summarize(accident_rate = mean(had_accident)) %>%
  arrange(desc(accident_rate))

# Where did the accident rate increase most?
# Comparing annual accident rates by location
accident_rates <- hr_joined %>% 
  group_by(location, year) %>% 
  summarize(accident_rate = mean(had_accident))
accident_rates

accident_rates %>% 
  ggplot(aes(x = factor(year), y = accident_rate)) +
  geom_col() +
  facet_wrap(~ location)

# Let's focus on location with the highest increase in accident rate
# Filtering out the other locations
southfield <- hr_joined %>% 
  filter(location == "Southfield")

# Finding the average overtime hours worked by year
southfield %>%
  group_by(year) %>% 
  summarize(average_overtime_hours = mean(overtime_hours))

# Testing difference in Southfield's overtime hours between years using paired t-test
t.test(overtime_hours ~ year, data = southfield, paired = T)
# Testing difference in Southfield's overtime hours between years using linear mixed-effects model
mixed_reg <- lmer(overtime_hours ~ (1|employee_id) + year, data = southfield)
tidy(mixed_reg)
summary(mixed_reg)

# Let's look at another potential omitted variable that could explain increas in accident rate in Southfield
# Importing the survey data
path_survey_data <- "./00_Data/survey_data_2.csv"
survey_data <- read_csv(path_survey_data)

# Creating the safety dataset
safety_southfield <- hr_joined %>%
  left_join(survey_data, by = c("employee_id", "year")) %>%
  mutate(disengaged = ifelse(engagement <=2, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  filter(location == "Southfield")

# Visualizing the difference in % disengaged by year in Southfield
safety_southfield  %>% 
  ggplot(aes(x = year, fill = factor(disengaged))) +
  geom_bar(position = "fill")

# Testing whether one year had significantly more disengaged employees
chisq.test(safety_southfield $year, safety_southfield $disengaged)

# Is that change isolated to the Southfield location?
# Filtering out Southfield
other_locs <- hr_joined %>%
  left_join(survey_data, by = c("employee_id", "year")) %>%
  mutate(disengaged = ifelse(engagement <=2, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  filter(location != "Southfield")

# Testing whether one year had significantly more overtime hours worked
t.test(overtime_hours ~ year, data = other_locs, paired = T) 

# Testing whether one year had significantly more disengaged employees
chisq.test(other_locs$year, other_locs$disengaged)

# Using multiple regression to test the impact of year and disengaged on accident rate in Southfield
# Should be adjusted to the fact that we have repeated observations
regression <- glm(had_accident ~ year + disengaged, family = "binomial", data = safety_southfield)
tidy(regression)
# The linear model should be adjusted to the fact that we have repeated observations
mixed_reg2 <- glmer(had_accident ~ (1|employee_id) + year + disengaged, family = "binomial", data = safety_southfield)
tidy(mixed_reg2)
summary(mixed_reg2)
