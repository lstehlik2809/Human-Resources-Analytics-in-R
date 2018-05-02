library(tidyverse)

# Uploading the dataset
recruitment <- read_csv("./00_Data/recruitment_data.csv")
head(recruitment)
names(recruitment)
summary(recruitment)

# Number of employees hired through given source
recruitment %>% 
  count(recruiting_source) %>%
  arrange(desc(n))

# Assessing quality of hires through individual sources
