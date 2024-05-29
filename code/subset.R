rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")

#View(data_wide2)

model_data <- data_wide2 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Subset data from the minimum count value for each site and year combination
# Identify the minimum count value for each site


min_count_year <- model_data %>% 
drop_na(count) %>% 
group_by(site) %>% 
filter(count == min(count)) %>% 
slice(1) %>% 
ungroup() %>% 
select(site, year)

# Filter original data to keep counts from the year of minimum count onwards
data_from_min_count <- model_data %>% 
inner_join(min_count_year, by = "site", suffix = c("", "_min_count")) %>% 
filter(year >= year_min_count) %>% 
select(-year_min_count) %>% 
filter(count >= 0)

#Get the slope of each mine after min count year
sites_with_data <- data_from_min_count %>% 
group_by(site) %>% 
filter(n() >= 2) %>% 
ungroup()

# Nest the data by site
nest_data <- sites_with_data %>% 
group_by(site) %>% 
nest()

library(broom)
library(purrr)
library(tidyr)

# Fit a regression model for each site
nest_data <- nest_data %>% 
mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>% 
    mutate(year = as.numeric(year),
            count = as.numeric(count))
    # Fit the linear model
    lm(count ~ year, data = df)
}))

# Tidy the model outputs
tidy_data <- nest_data %>% 
mutate(tidied = map(model, tidy)) %>% 
unnest(tidied)

# Filter and select the slope for the 'year' term
regress_results <- tidy_data %>% 
filter(term == "year") %>% 
select(site, slope = estimate)

print(regress_results)

# Add the slope to the original data frame
final_data_frame <- sites_with_data %>% 
left_join(regress_results, by = "site")

model_this_data1 <- final_data_frame %>% 
pivot_wider(names_from = year, 
            values_from = count) %>% 
select(site, slope) %>% 
left_join(data_wide2, by = "site")