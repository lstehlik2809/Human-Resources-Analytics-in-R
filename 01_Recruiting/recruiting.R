# IDENTIFYING THE BEST RECRRUITING SOURCE ----
# Analyzing and visualizing recruiting data to determine which source of new 
# candidates ultimately produces the best new hires

library(tidyverse)

# Uploading the dataset ----
recruitment <- read_csv("./00_Data/recruitment_data.csv")
head(recruitment)
names(recruitment)
summary(recruitment)

# Number of employees hired through given source ----
recruitment %>% 
  count(recruiting_source) %>%
  arrange(desc(n))

# Assessing quality of hires through individual sources ----
# sales quota
avg_sales <- recruitment %>%
  group_by(recruiting_source) %>%
  summarise(avg_sales_quota_pct = mean(sales_quota_pct)) %>%
  arrange(desc(avg_sales_quota_pct))

# Attrition Rates
avg_attrition <- recruitment %>%
  group_by(recruiting_source) %>%
  summarise(attrition_rate = mean(attrition)) %>%
  arrange(attrition_rate)

# Visualizing the sales performance differences
ggplot(avg_sales, aes(x = recruiting_source, y = avg_sales_quota_pct))+
  geom_col()

# Visualizing the attrition differences
ggplot(avg_attrition, aes(x = recruiting_source, y = attrition_rate))+
  geom_col()
