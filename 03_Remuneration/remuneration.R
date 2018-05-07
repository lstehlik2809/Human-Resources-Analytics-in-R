# PAYING NEW HIRES FAIRLY ----
# When employers make a new hire, they must determine what the new employee will
# be paid. If the employer is not careful, the new hires can come in with a higher 
# salary than the employees that currently work at the same job, which can cause
# employee turnover and dissatisfaction. Let's check whether
# new hires are really getting paid more than current employees, and how to 
# double-check initial observations.

# Load libraries
library(tidyverse)
library(skimr)
library(broom)

# Import the data
path_pay <- "./00_Data/fair_pay_data.csv"
pay <- read_csv(path_pay)

# Checking data
glimpse(pay)
summary(pay)
skim(pay)

pay %>%
  select_if(is.character) %>%
  map(table)

pay %>%
  select_if(is.character) %>%
  map(~table(.) %>% prop.table())

# Check average salary of new hires and non-new hires
pay %>% 
  group_by(new_hire) %>%
  summarise(avg_salary = mean(salary))

# Perform the statistical test
t.test(salary ~ new_hire, data = pay) %>% 
  tidy()

# Checking possible ommited variables (job level)
pay %>%
  group_by(job_level) %>%
  summarise(avg_salary = mean(salary))

ggplot(data = pay, aes(x = new_hire, fill = job_level)) +
  geom_bar(position = "fill")

# Calculate the average salary for each group of interest
pay_grouped <- pay %>% 
  group_by(new_hire, job_level) %>% 
  summarize(avg_salary = mean(salary))

# Graph the results using facet_wrap()  
pay_grouped %>%
  ggplot(aes(x = new_hire, y = avg_salary, fill = new_hire)) +
  geom_col() +
  facet_wrap(~ job_level)

# Are hourly hires paid more?
pay_filter <- pay %>%
  filter(job_level == "Hourly")

t.test(salary ~ new_hire, data = pay_filter) %>%
  tidy()

# Run the simple regression
lm_model <- lm(salary ~ new_hire + job_level, data = pay)

# Display the summary of model_simple
lm_model %>% 
  summary()

# Display a tidy summary
lm_model %>% 
  tidy()
