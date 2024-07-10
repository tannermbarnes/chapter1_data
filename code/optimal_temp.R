rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")
library(tidyr)
library(purrr)
library(broom)
library(patchwork)
#View(data_wide3)

data <- data_wide3 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Subset data from the minimum count value for each site and year combination
data1 <- data %>% drop_na(count)
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
filter(max_count > 0) %>%
filter(max > 0) %>% 
pivot_wider(names_from = year, 
            values_from = count)

#View(before_data)

before_data <- before_data %>%
  mutate(standing_water = ifelse(standing_water == "?", NA, standing_water),
  water = as.factor(standing_water),
  shafts = as.factor(shafts),
  levels = as.factor(levels),
  passage_length = as.numeric(passage_length)
  )

# Now below do the same thing but for the max count after 2015

# Get the minimum and maximum count data to normalize the counts
min_max_count <- data %>% 
group_by(site) %>% 
filter(year > 2016) %>% 
summarize(min_count = min(count, na.rm = TRUE), max_count = max(count, na.rm = TRUE)) %>% 
ungroup()

data_with_min_max <- data %>% 
left_join(min_max_count, by = "site")


after_data <- data_with_min_max %>% 
group_by(site) %>% 
filter(max_count > 0) %>%
filter(max > 0) %>% 
pivot_wider(names_from = year, 
            values_from = count)

#View(after_data)

after_data <- after_data %>%
  mutate(standing_water = ifelse(standing_water == "?", NA, standing_water),
  water = as.factor(standing_water),
  shafts = as.factor(shafts),
  levels = as.factor(levels),
  passage_length = as.numeric(passage_length)
  )

min_sites1 <- before_data %>% 
filter(site == "Adventure Adit/ Rock Fall" | site == "Adventure Mine" | site == "Agency Place Mine" | site == "Child's Adit" |
site == "Cushman Adit" | site == "Delaware Mine" | site == "Derby Adit" | site == "Flintsteel Adit" | site == "Ford Exploratory Adit" | 
site == "Glen Adit #1" | site == "Hendrie River Water Cave" | site == "Iron Mountain Iron Mine (Tourist Mine)" |
site == "Jones' Adit" | site == "Keel Ridge Shaft" | site == "Lafayette East Shaft" | site == "Mass C Adit" | 
site == "Norway Mine" | site == "Quinnesec Adit" | site == "Rockport Quarry North Tunnel" | site == "South Bluff Adit" |
site == "South Lake Mine" | site == "Taylor Adit" | site == "Windsor Shaft #3" | site == "the belt mine")

min_sites2 <- after_data %>% 
filter(site == "Adventure Adit/ Rock Fall" | site == "Adventure Mine" | site == "Agency Place Mine" | site == "Child's Adit" |
site == "Cushman Adit" | site == "Delaware Mine" | site == "Derby Adit" | site == "Flintsteel Adit" | site == "Ford Exploratory Adit" | 
site == "Glen Adit #1" | site == "Hendrie River Water Cave" | site == "Iron Mountain Iron Mine (Tourist Mine)" |
site == "Jones' Adit" | site == "Keel Ridge Shaft" | site == "Lafayette East Shaft" | site == "Mass C Adit" | 
site == "Norway Mine" | site == "Quinnesec Adit" | site == "Rockport Quarry North Tunnel" | site == "South Bluff Adit" |
site == "South Lake Mine" | site == "Taylor Adit" | site == "Windsor Shaft #3" | site == "the belt mine")

p1 <- min_sites1 %>%
  ggplot(aes(x = mean_temp, y = max_count)) +
  geom_point() +
  geom_smooth(method = "loess", color = "green") +
  labs(title = "Relationship between Mean Temperature and Max Count (LOESS)",
       x = "Mean Temperature",
       y = "Max Count") +
  theme_bw()

p2 <- min_sites2 %>%
  ggplot(aes(x = mean_temp, y = max_count)) +
  geom_point() +
  geom_smooth(method = "loess", color = "green") +
  labs(title = "Relationship between Mean Temperature and Max Count (LOESS)",
       x = "Mean Temperature",
       y = "Max Count") +
  theme_bw()

combined_plot <- p1 + p2
# Save the combined plot
ggsave("E:/chapter1_data/figures/final/test.png", plot = combined_plot, width = 9, height = 6)

