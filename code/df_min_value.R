rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
source("min_year.R")
#View(model_this_data1)
# install.packages("lme4")
# install.packages("car")
# install.packages("lmtest")
library(lme4)
library(car)
library(lmtest)

# Change water to a factor
model_this_data2$standing_water <- as.factor(model_this_data2$water)
model_this_data2$levels <- as.factor(model_this_data2$levels)
model_this_data2$shafts <- as.factor(model_this_data2$shafts)

# Create a variable for mine complexity
#View(model_this_data)
model_with_complexity <- model_this_data2 %>% 
  mutate(levels = ifelse(is.na(levels), 1, levels)) %>% 
  mutate(shafts = ifelse(is.na(shafts), 1, shafts)) %>% 
  mutate(complexity = case_when(
    passage_length > 600 & levels == 1 & shafts == 1 ~ 4,
    passage_length > 200 & shafts >= 2 & levels == 1 ~ 3,
    passage_length > 200 & shafts >= 1 & levels >= 2 ~ 4,
    passage_length >= 200 & shafts >= 1 & levels >= 1 ~ 2,
    TRUE ~ 1  # Default to 1 if no other conditions are met
  ))

# remove collin's adit because max is 1 and min is 0 not a hibernacula
mines_to_remove <- c("Collin's Adit", "Ridge Adit", "Bear Cave")

add_last_count <- model_with_complexity %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

add_last_count1 <- add_last_count %>% 
group_by(site) %>% 
arrange(site, desc(year)) %>% 
mutate(last_count = first(na.omit(count))) %>% 
ungroup()

add_last_count2 <- add_last_count1 %>% 
pivot_wider(names_from = year, values_from = count)


df_min_value <- add_last_count2 %>% 
filter(!site %in% mines_to_remove)

# Add crash intensity
df_min_value$crash <- 1 - (df_min_value$min_count/df_min_value$max_count)
df_min_value$log_max_count <- log(df_min_value$max_count)
df_min_value$log_passage <- log(df_min_value$passage_length)

# LN of explantory variables to get Pseduothreshold
# Add 1 all min values to avoid log(0)
df_min_value <- df_min_value %>% 
mutate(min_value = min + 1, 
      max_value = max + 1) %>% 
mutate(min_value = ifelse(min_value < 0, 1, min_value))

df_min_value$log_min_value <- log(df_min_value$min_value)
df_min_value$log_max_value <- log(df_min_value$max_value)

df_min_value$site
