rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")
library(tidyr)
library(purrr)
library(broom)
#View(data_wide3)

data <- data_wide3 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Subset data from the minimum count value for each site and year combination
data1 <- data %>%
  drop_na(count) %>%
  group_by(site) %>%
  filter(n() >= 2) %>%
  reframe(
    min_count = if (any(year > 2012 & !is.na(count))) min(count[year > 2012], na.rm = TRUE) else NA,
    mini_year = if (any(year > 2012 & !is.na(count))) year[year > 2012][which.min(count[year > 2012])] else NA,
    max_count = max(count, na.rm = TRUE),
    max_year = year[which.max(count)],
    mean_count = if (!is.na(mini_year)) mean(count[year < mini_year], na.rm = TRUE) else NA,
    last_year = max(year, na.rm = TRUE),  # Last year of survey
    last_count = count[which.max(year)]   # Count of the last survey year
  ) %>%
  ungroup() %>%
  drop_na(min_count) %>%
  drop_na(mean_count)


# Get the minimum and maximum count data to normalize the counts
period_df <- data %>% 
  left_join(data1, by = "site") %>% 
  drop_na(count) %>% 
  drop_na(mean_count) %>% 
  mutate(period = ifelse(year < mini_year, "before", "after"))

