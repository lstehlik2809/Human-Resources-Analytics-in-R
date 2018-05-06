# WHAT IS DRIVIGN LOW EMPLOYEE ENGAGEMENT? ----

library(tidyverse)
library(skimr)

# Import the data
path_survey <- "./00_Data/survey_data.csv"
survey <- read_csv(path_survey)

# Get an overview of the data
glimpse(survey)
summary(survey)
skim(survey)

survey %>%
  select_if(is.character) %>%
  map(~table(.) %>% prop.table())

survey %>%
  count(department)


# Output the average engagement score for each department, sorted
survey %>%
  group_by(department) %>%
  summarise(avg_engagement = mean(engagement)) %>%
  arrange(avg_engagement)

# Create the disengaged variable and assign the result to survey_disengaged
survey_disengaged <- survey %>% 
  mutate(disengaged = ifelse(engagement <=2, 1, 0))

survey_disengaged

# Summarize the three variables by department
survey_summary <- survey_disengaged %>%
  group_by(department) %>%
  summarise(pct_disengaged = mean(disengaged), 
            avg_salary = mean(salary), 
            avg_vacation_days = mean(vacation_days_taken))

survey_summary

# Visualizing engagement data
# Gather data for plotting
survey_gathered <- survey_summary %>% 
  gather(pct_disengaged, avg_salary, avg_vacation_days,
         key = "measure", value = "value")

# Create three bar charts
ggplot(survey_gathered, aes(x = measure, y = value, fill = department)) +
  geom_col(position = "dodge") +
  facet_wrap(~ measure, scales = "free")