rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")
library(broom)
library(purrr)
library(tidyr)
#View(data_wide2)

model_data <- data_wide2 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Identify the minimum and maximum count for each site

# ?????????????????????????????????
# Need to find the most recovered mine and use that as recovered
# For example, if the best mine has recovered 50% from its max count then half max count for every site
# If the best site has recovered 25% then divied each max count by 4 
# this will help spread the data out and get a normal spread

min_max_counts <- model_data %>% 
group_by(site) %>% 
summarize(min_count = min(count, na.rm = TRUE), max_count = max(count, na.rm = TRUE)) %>% 
ungroup()
#View(min_max_counts)
#View(model_data)


# Merge the min and max counts back into the original data
model_data_with_min_max <- model_data %>% 
left_join(min_max_counts, by = "site")

# Normalize the counts 
normalized_data <- model_data_with_min_max %>% 
mutate(normalized_count = (count - min_count) / (max_count - min_count))

# To get the normalized count and year relative to the year of minimum count
# Subset data from the minimum count value for each site and year combination
# Identify the minimum count value for each site
min_count_year <- normalized_data %>% 
drop_na(count) %>% 
group_by(site) %>% 
filter(normalized_count == min(normalized_count)) %>% 
slice(1) %>% # in case of ties, take the first occurence
ungroup() %>% 
select(site, min_year = year) %>% 
mutate(min_year = as.numeric(min_year))

# Merge the minimum count year back into the normalized data
model_data_with_min_year <- normalized_data %>% 
mutate(year = as.numeric(year)) %>% 
left_join(min_count_year, by = "site") %>% 
mutate(relative_year = year - min_year)

# Filter original data to keep counts from the year of minimum count onwards
filtered_data <- model_data_with_min_year %>% 
filter(relative_year >= 0) %>% 
filter(normalized_count >= 0) %>% 
select(site, relative_year, normalized_count)

#View(filtered_data)

#Get the slope of each mine after min count year
sites_with_data <- filtered_data %>% 
group_by(site) %>% 
filter(n() >= 2) %>% 
ungroup()


# Nest the data by site
nest_data <- sites_with_data %>% 
group_by(site) %>% 
nest()

# Fit a regression model for each site
nest_data <- nest_data %>% 
mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>% 
    mutate(year = as.numeric(relative_year),
            count = as.numeric(normalized_count))
    # Fit the linear model
    lm(normalized_count ~ relative_year, data = df)
}))

# Tidy the model outputs
tidy_data <- nest_data %>% 
mutate(tidied = map(model, tidy)) %>% 
unnest(tidied)

# Filter and select the slope for the 'year' term
regress_results <- tidy_data %>% 
filter(term == "relative_year") %>% 
select(site, slope = estimate)

regress_results$slope <- round(regress_results$slope, 3)

print(regress_results)





regress_results$slope
# Add the slope to the original data frame
final_data_frame <- sites_with_data %>% 
left_join(regress_results, by = "site")

model_this_data1 <- final_data_frame %>% 
pivot_wider(names_from = relative_year, 
            values_from = normalized_count) %>% 
select(site, slope) %>% 
left_join(data_wide2, by = "site")

