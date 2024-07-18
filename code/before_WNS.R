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
data1 <- data %>% drop_na(count) %>% 
mutate(period = ifelse(year < 2016, "before", "after"))
#View(data1)



# Get the minimum and maximum count data to normalize the counts
min_max_count <- data %>% 
group_by(site) %>% 
filter(year < 2016) %>% 
summarize(min_count = min(count, na.rm = TRUE), max_count = max(count, na.rm = TRUE)) %>% 
ungroup()

data_with_min_max <- data %>% 
left_join(min_max_count, by = "site")


before_data <- data_with_min_max %>% 
group_by(site) %>% 
filter(max_count > 6) %>%
filter(max > 0) %>% 
pivot_wider(names_from = year, 
            values_from = count)

#View(before_data)

before_data <- before_data %>%
  mutate(standing_water = ifelse(standing_water == "?", NA, standing_water),
  standing_water = as.factor(standing_water),
  shafts = as.factor(shafts),
  levels = as.factor(levels),
  passage_length = as.numeric(passage_length)
  )

############
# Now below do the same thing but for the max count after 2015

# Get the minimum and maximum count data to normalize the counts
min_max_count1 <- data %>% 
group_by(site) %>% 
filter(year > 2016) %>% 
summarize(min_count = min(count, na.rm = TRUE), max_count = max(count, na.rm = TRUE)) %>% 
ungroup()

data_with_min_max1 <- data %>% 
left_join(min_max_count1, by = "site")


after_data <- data_with_min_max1 %>% 
group_by(site) %>% 
filter(max_count > 0) %>%
filter(max > 0) %>% 
pivot_wider(names_from = year, 
            values_from = count)

#View(after_data)

after_data <- after_data %>%
  mutate(standing_water = ifelse(standing_water == "?", NA, standing_water),
  standing_water = as.factor(standing_water),
  shafts = as.factor(shafts),
  levels = as.factor(levels),
  passage_length = as.numeric(passage_length)
  )

# Define the list of temperature variables
temp_vars <- c("min", "max", "mean_temp", "median_temp", "mode_temp", "temp_diff")