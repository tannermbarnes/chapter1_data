rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")

#View(data_wide3)

data <- data_wide3 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Subset data from the minimum count value for each site and year combination
# Identify the minimum count value for each site
data1 <- data %>% drop_na(count)
#View(data1)

max_decrease_year <- data %>% 
drop_na(count) %>% 
group_by(site) %>% 
arrange(site, year) %>% 
mutate(diff = count - lag(count, order_by = year)) %>% # Calculate year-over-year differences
filter(!is.na(diff)) %>% 
slice(which.min(diff)) %>% 
ungroup() %>% 
select(site, year)

# Filter original data to keep counts from the year of max decrease onwards
data_after_max_decrease <- data %>% 
inner_join(max_decrease_year, by = "site", suffix = c("", "_max_decrease")) %>% 
filter(year >= year_max_decrease) %>% 
select(-year_max_decrease) %>% 
filter(count >= 0)

#View(data_after_max_decrease)
library(tidyr)
library(purrr)
library(broom)

# Filter sites with at least 2 years of data after the minimum count year
sites_with_sufficient_data <- data_after_max_decrease %>% 
group_by(site) %>% 
filter(n() >= 2) %>% 
ungroup()

# Nest the data by site
nested_data <- sites_with_sufficient_data %>% 
group_by(site) %>% 
nest()

#print("Nested data:")
#print(nested_data)

# Fit a linear regression model for each site
nested_data <- nested_data %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(year),
             count = as.numeric(count))
    # Fit the linear model
    lm(count ~ year, data = df)
  }))

# Check the models
# print("Nested data with models:")
# print(nested_data)

# Step 4: Tidy the model outputs
tidied_data <- nested_data %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# Check the tidied data
#print("Tidied data:")
#print(tidied_data)

# Step 5: Filter and select the slope for the 'year' term
regression_results <- tidied_data %>%
  filter(term == "year") %>%
  select(site, slope = estimate)

# Check the regression results
#print("Regression results:")
#print(regression_results)

# Step 6: Add the slope to the original data frame
final_data <- sites_with_sufficient_data %>%
  left_join(regression_results, by = "site")

# Check the final data
#print("Final data with slopes:")
#View(final_data)

# Graph the regression lines
# ggplot(final_data, aes(x = year, y = count, color = site)) +
# geom_point() +
# geom_smooth(method = "lm", se = FALSE, aes(group = site)) + # Regression line per site
# theme_minimal() +
# labs(title = "Yearly Count Data with Regression Lines per Site",
# x = "Year", 
# y = "Count", 
# color = "Site") + 
# theme(legend.position = "none")

# ggsave("E:/chapter1_data/figures/slide_scale_regressions.jpg", width = 8, height=4)

# Filter out tippy dam for better viewing purposes
# final_data %>% filter(count < 5000) %>% filter(year > 2013) %>% 
# ggplot(aes(x = year, y = count, color = site)) +
# geom_point() +
# geom_smooth(method = "lm", se = FALSE, aes(group = site)) + # Regression line per site
# theme_minimal() +
# labs(title = "Yearly Count Data with Regression Lines per Site",
# x = "Year", 
# y = "Count", 
# color = "Site") + 
# theme(legend.position = "none")

# ggsave("E:/chapter1_data/figures/slide_scale_regressions.jpg", width = 8, height=4)

model_this_data <- final_data %>% 
pivot_wider(names_from = year, 
            values_from = count) %>% 
select(site, slope) %>% 
left_join(data_wide2, by = "site")

#View(model_this_data)


