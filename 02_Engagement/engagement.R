# WHAT IS DRIVIGN LOW EMPLOYEE ENGAGEMENT? ----
# Gallup defines engaged employees as those who are involved in, enthusiastic 
# about and committed to their work and workplace. There is disagreement about 
# the strength of the connection between employee engagement and business outcomes, 
# but the idea is that employees that are more engaged will be more productive and
# stay with the organization longer. Let's look into potential 
# reasons that one department's engagement scores are lower than the rest.

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

# Testing stat. significance of the difference in disengagement between Sales dpt and other dpts
# Add the in_sales variable
survey_sales <- survey_disengaged %>%
  mutate(in_sales = ifelse(department == "Sales", "Sales", "Other"))

# Test the hypothesis using survey_sales
chisq.test(survey_sales$in_sales, survey_sales$disengaged)

# Testing stat. significance of the difference in # vacation days between Sales dpt and other dpts
# Test the hypothesis using the survey_sales data
t.test(vacation_days_taken ~ in_sales, data = survey_sales)
