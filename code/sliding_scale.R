##### Making models for sliding scales ######
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
# Identify the minimum count value for each site
# Get the minimum and maximum count data to normalize the counts
min_max_count <- data %>% 
group_by(site) %>% 
summarize(min_count = min(count, na.rm = TRUE), max_count = max((count/1.5), na.rm = TRUE),
real_max_count = max(count, na.rm = TRUE)) %>% 
ungroup()

# Merge the min and max counts back into the orginal data
data_with_min_max <- data %>% 
left_join(min_max_count, by = "site")

# Normalize the count data
normalize_count <- data_with_min_max %>% 
mutate(normalize_count = (count - min_count) / (max_count - min_count)) %>% 
mutate(normalize_to_min = count / (min_count + 1))

# To get the normalized count and relative year of max decrease for sliding scale
min_count_year1 <- normalize_count%>% 
drop_na(count) %>% 
group_by(site) %>% 
filter(normalize_count == min(normalize_count)) %>% 
slice(1) %>% # in case of ties, take the first occurence
ungroup() %>% 
select(site, min_year = year) %>% 
mutate(min_year = as.numeric(min_year))

# Merge the minimum count year back into the normalized data
data_with_decrease_year <- normalize_count %>% 
mutate(year = as.numeric(year)) %>% 
left_join(min_count_year1, by = "site") %>% 
mutate(relative_year = year - min_year)

# Filter original data to keep counts from the year of max decrease onwards
filter_data <- data_with_decrease_year %>% 
filter(relative_year >= 0) %>% 
filter(normalize_count >= 0) %>% 
select(site, relative_year, normalize_count)

#View(filter_data)

# Filter sites with at least 2 years of data after the minimum count year
# sites_with_sufficient_data <- filter_data %>% 
# group_by(site) %>% 
# filter(n() >= 2) %>% 
# ungroup()

# Nest the data by site
nested_data <- filter_data %>% 
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
      mutate(year = as.numeric(relative_year),
             count = as.numeric(normalize_count))
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
  select(site, slope = estimate) %>% 
  mutate(slope = ifelse(is.na(slope), 0, slope))



# Check the regression results
#print("Regression results:")
#print(regression_results)

# Step 6: Add the slope to the original data frame
final_data <- filter_data %>%
  left_join(regression_results, by = "site")

#View(final_data)

# Merge final slopes with the other metadata in a wide format to model
model_data1 <- final_data %>% 
pivot_wider(names_from = relative_year, 
            values_from = normalize_count) %>% 
select(site, slope) %>% 
left_join(data_wide3, by = "site")


model_data2 <- data_with_decrease_year %>% 
group_by(site) %>% 
summarize(min_count = min(min_count), max_count = max(max_count)) %>% 
inner_join(model_data1, by = "site") %>% 
ungroup()

 sites_to_remove <- c("Adventure Shaft", "Algonquin Adit #2 (Mark's Adit)", "Collin's Adit", "Copper Falls Mine", "Douglas Houghton Adit #1", "Jackson Mine, B Working",
 "Jackson Mine, Tram Tunnel Exit", "Merchant's Adit North", "Merchant's Adit South", "National Mine #7",
 "North American Adit", "Ohio Traprock Mine #1", "Ohio Traprock Mine #2", "Ohio Traprock Mine #3",
 "Ridge Adit", "Rockland Mine, Shaft 3", "Seneca Mine #3", "Trader's Mine", "Unknown Keweenaw",
 "West Vein Adit (Robin's Ore)", "White Pine Mine", "Cushman Adit", "Jackson Mine, Tram Tunnel", "West Evergreen Bluff Mine",
 "Caledonia Mine Complex", "Michigan (A Shaft)", "Ogimaw Mine", "Owl Creek Fissure (Old Copper Falls)",
 "Pewabic Mine (Iron Mountain)", "Piscatauqau Adit", "Nassau Mine", "Goodrich Adit B", "Aztec Mine",
 "Bumblebee Mine", "Millie Mine", "Toltec Mine")

filtered_data1 <- model_data2 %>%
  filter(!site %in% sites_to_remove)

# Change water to a factor
filtered_data1$standing_water <- as.factor(filtered_data1$standing_water)
filtered_data1$levels <- as.factor(filtered_data1$levels)
filtered_data1$shafts <- as.factor(filtered_data1$shafts)


model_with_complexity <- filtered_data1 %>% 
  mutate(levels = ifelse(is.na(levels), 1, levels)) %>% 
  mutate(shafts = ifelse(is.na(shafts), 1, shafts)) %>% 
  mutate(complexity = case_when(
    passage_length > 600 & levels == 1 & shafts == 1 ~ 4,
    passage_length > 200 & shafts >= 2 & levels == 1 ~ 3,
    passage_length > 200 & shafts >= 1 & levels >= 2 ~ 4,
    passage_length >= 200 & shafts >= 1 & levels >= 1 ~ 2,
    TRUE ~ 1  # Default to 1 if no other conditions are met
  ))

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

df_slide_scale <- add_last_count2 #%>% 
#filter(!site %in% sites_to_remove)

# Add crash intensity
df_slide_scale$crash <- 1 - (df_slide_scale$min_count/df_slide_scale$max_count)
df_slide_scale$log_max_count <- log(df_slide_scale$max_count)
df_slide_scale$log_passage <- log(df_slide_scale$passage_length)

# LN of explantory variables to get Pseduothreshold
# Add 1 all min values to avoid log(0)
df_slide_scale <- df_slide_scale %>% 
mutate(min_value = min + 1, 
      max_value = max + 1) %>% 
mutate(min_value = ifelse(min_value < 0, 1, min_value)) %>% 
mutate(bin = ifelse(slope > 0, 1, 0)) %>% 
subset(max_count > 6)

df_slide_scale$log_min_value <- log(df_slide_scale$min_value)
df_slide_scale$log_max_value <- log(df_slide_scale$max_value)



# Check the final data
#print("Final data with slopes:")
#View(final_data)

# #Graph the regression lines
# ggplot(final_data, aes(x = year, y = count, color = site)) +
# geom_point() +
# geom_smooth(method = "lm", se = FALSE, aes(group = site)) + # Regression line per site
# theme_minimal() +
# labs(title = "Yearly Count Data with Regression Lines per Site",
# x = "Year", 
# y = "Count", 
# color = "Site") + 
# theme(legend.position = "none")

#  ggsave("E:/chapter1_data/figures/slide_scale_regressions.jpg", width = 8, height=4)

# # Filter out tippy dam for better viewing purposes
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

# model_this_data <- final_data %>% 
# pivot_wider(names_from = relative_year, 
#             values_from = normalize_count) %>% 
# select(site, slope) %>% 
# left_join(data_wide2, by = "site")

#View(model_this_data)



# # ################################# Test normalized_counts for normality ##########################################
# # # Plot histogram
# ggplot(final_data, aes(x = normalize_count)) +
#   geom_histogram(bins = 30, color = "black", fill = "blue") +
#   ggtitle("Histogram of Normalized Count") +
#   xlab("Normalized Count") +
#   ylab("Frequency")

# # Plot Q-Q plot (Quantile-Quantile plot) compares the quantiles of the normalized count with that of a normal distribution
# ggplot(final_data, aes(sample = normalize_count)) +
#   geom_qq() +
#   geom_qq_line() +
#   ggtitle("Q-Q Plot of Normalized Count")

# # Shapiro-Wilk test A small p-value indicates the null hypothesis can be rejected, meaning the data is not normally distributed
# shapiro_test <- shapiro.test(final_data$normalize_count)
# print(shapiro_test)

# ################## Test slope for normality ###############################
# ggplot(final_data, aes(x = slope)) +
#   geom_histogram(bins = 30, color = "black", fill = "blue") +
#   ggtitle("Histogram of Normalized Count") +
#   xlab("Normalized Count") +
#   ylab("Frequency")

# shapiro_test <- shapiro.test(final_data$slope)
# print(shapiro_test)

# # Plot Q-Q plot (Quantile-Quantile plot) compares the quantiles of the normalized count with that of a normal distribution
# ggplot(final_data, aes(sample = slope)) +
#   geom_qq() +
#   geom_qq_line() +
#   ggtitle("Q-Q Plot of Normalized Count")

# final_data %>% filter(!site %in% "Tippy Dam") %>% 
# ggplot(aes(x=relative_year, y=normalize_count, color = site)) +
# geom_point(show.legend = FALSE) +
# geom_smooth(method = "lm", show.legend = FALSE, se = FALSE)

# ggsave("E:/chapter1_data/figures/sliding_normalize_slope_by_mine.png", width = 6, height=4)
