# ARE PERFORMANCE RATINGS BEING GIVEN CONSISTENTLY? ----

# Performance management helps an organization keep track of which 
# employees are providing extra value, or below-average value, and 
# compensating them accordingly. Whether performance is a rating or
# the result of a questionnaire, whether employees are rated each year 
# or more often than that, the process is somewhat subjective. 
# An organization should check that ratings are being given with regard 
# to performance, and not individual managers' preferences, or even biases
# (conscious or subconscious). Whenever one demographic group seems to be 
# doing better than another, bias is always a potential reason. Exploring other 
# explanations is another good idea

# Loading libraries
library(tidyverse)
library(broom)
library(skimr)

# Loading and isnpecting data
path_performance_data <- "./00_Data/performance_data.csv"
path_hr_data <- "./00_Data/hr_data.csv"

hr_data <- read_csv(path_hr_data)
performance_data <- read_csv(path_performance)

glimpse(hr_data)
glimpse(performance_data)

summary(hr_data)
summary(performance_data)

skim(hr_data)
skim(performance_data)

# Joining the two tables
joined_data <- left_join(hr_data, performance_data, by = "employee_id")

# Checking whether the average performance rating differs by gender 
joined_data %>%
  group_by(gender) %>%
  summarise(avg_rating = mean(rating))

# Adding the high_performer column
performance <- joined_data %>%  
  mutate(high_performer = ifelse(rating >= 4, 1, 0))

# Testing whether one gender is more likely to be a high performer
chisq.test(performance$gender, performance$high_performer) %>% 
  tidy()

# Visualizing the distribution of high_performer by gender
ggplot(performance, aes(x = gender, fill = factor(high_performer))) +
  geom_bar(position = "fill")

# Visualize the distribution of all ratings by gender
ggplot(performance, aes(x = gender, fill = factor(rating))) +
  geom_bar(position = "fill")

# Checking for omitted variable bias (job level - in many organizations, 
# employees at higher job levels in the organization are more likely to be 
# considered high performers. That is, the distribution of performance ratings
# is not always the same at different job levels)

# Summarizing the distribution of job_level by gender
performance %>% 
  group_by(gender, job_level) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(gender) %>%
  mutate(pct = n/sum(n))

# Visualizing the distribution of job_level by gender
performance %>%
  ggplot(aes(x = gender, fill = job_level)) +
  geom_bar(position = "fill")

# Testing whether men and women have different job level distributions
chisq.test(performance$gender, performance$job_level) 

# Visualizing the distribution of high_performer by gender, faceted by job level
performance %>% 
  ggplot(aes(x = gender, fill = as.factor(high_performer))) +
  geom_bar(position = "fill") +
  facet_wrap(~ job_level)

# Testing whether one gender is more likely to be a high performer when taking into account
# existing differences in job level
glm(high_performer ~ gender + job_level, family = "binomial", data = performance) %>%
  tidy()
