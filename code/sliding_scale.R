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
  reframe(
    min_count = if (any(year > 2012 & !is.na(count))) min(count[year > 2012], na.rm = TRUE) else NA, 
    mini_year = if (any(year > 2012 & !is.na(count))) year[year > 2012][which.min(count[year > 2012])] else NA,  
    max_count = max(count, na.rm = TRUE), 
    max_year = year[which.max(count)],
    mean_count = if (!is.na(mini_year)) mean(count[year < mini_year], na.rm = TRUE) else NA
  )

# Merge the min and max counts back into the orginal data
data_with_min_max <- data %>% 
left_join(min_max_count, by = "site")

# Normalize the count data
normalize_count <- data_with_min_max %>% 
mutate(normalize_count = (count - min_count) / (mean_count - min_count)) %>% 
mutate(normalize_to_min = count / (min_count + 1))

# To get the normalized count and relative year of max decrease for sliding scale
min_count_year1 <- normalize_count %>% 
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
  reframe(
    min_count = if (any(year > 2012 & !is.na(count))) min(count[year > 2012], na.rm = TRUE) else NA, 
    mini_year = if (any(year > 2012 & !is.na(count))) year[year > 2012][which.min(count[year > 2012])] else NA,  
    max_count = max(count, na.rm = TRUE), 
    max_year = year[which.max(count / 1.5)], 
    mean_count = if (!is.na(mini_year)) mean(count[year < mini_year], na.rm = TRUE) else NA
  ) %>% 
  left_join(model_data1, by = "site")

 sites_to_remove <- c("Adventure Shaft", "Algonquin Adit #2 (Mark's Adit)", "Collin's Adit", "Copper Falls Mine", "Douglas Houghton Adit #1", "Jackson Mine, B Working",
 "Jackson Mine, Tram Tunnel Exit", "Merchant's Adit North", "Merchant's Adit South", "National Mine #7",
 "North American Adit", "Ohio Traprock Mine #1", "Ohio Traprock Mine #2", "Ohio Traprock Mine #3",
 "Ridge Adit", "Rockland Mine, Shaft 3", "Seneca Mine #3", "Trader's Mine", "Unknown Keweenaw",
 "West Vein Adit (Robin's Ore)", "White Pine Mine", "Cushman Adit", "Jackson Mine, Tram Tunnel", "West Evergreen Bluff Mine",
 "Caledonia Mine Complex", "Michigan (A Shaft)", "Ogimaw Mine", "Owl Creek Fissure (Old Copper Falls)",
 "Pewabic Mine (Iron Mountain)", "Piscatauqau Adit", "Nassau Mine", "Goodrich Adit B", "Aztec Mine",
 "Bumblebee Mine", "Millie Mine", "Toltec Mine", "Randville Quarry Mine", "Indiana Mine")

filtered_data1 <- model_data2 %>%
  filter(!site %in% sites_to_remove)

# Change water to a factor
filtered_data1$standing_water <- as.factor(filtered_data1$standing_water)
filtered_data1$levels <- as.factor(filtered_data1$levels)
filtered_data1$shafts <- as.factor(filtered_data1$shafts)

add_last_count <- filtered_data1 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

add_last_count1 <- add_last_count %>% 
group_by(site) %>% 
arrange(site, desc(year)) %>% 
mutate(last_count = first(na.omit(count)),
last_year = first(year[!is.na(count)])) %>% 
ungroup()

df_slide_scale <- add_last_count1 %>% 
pivot_wider(names_from = year, values_from = count)

# Add crash intensity
df_slide_scale$crash_mean <- 1 - (df_slide_scale$min_count/df_slide_scale$mean_count)
df_slide_scale$crash_max <- 1 - (df_slide_scale$min_count/df_slide_scale$max_count)
df_slide_scale$log_max_count <- log(df_slide_scale$max_count)
df_slide_scale$log_passage <- log(df_slide_scale$passage_length)

# LN of explantory variables to get Pseduothreshold
# Add 1 all min values to avoid log(0)
df_slide_scale <- df_slide_scale %>% 
mutate(slope = ifelse(slope < 0, 0, slope)) %>% 
mutate(bin = ifelse(slope > 0, 1, 0)) %>% 
mutate(crash_mean = ifelse(crash_mean < 0, 0, crash_mean)) %>% 
subset(max_count > 6) %>% 
mutate(last_year = as.numeric(last_year), 
recovery_years = last_year - mini_year)

model_df <- df_slide_scale %>% 
mutate(recovery_status = ifelse(slope > 0 & (last_count / max_count > 0.05), "recovering", "not recovering")) %>% 
mutate(recovery_status = as.factor(recovery_status)) %>% 
mutate(site_numeric = as.numeric(factor(site))) %>% 
select(site, site_numeric, bin, min_count, max_count, mean_count, last_count, recovery_status, recovery_years, mini_year, max_year, last_year, slope, standing_water, passage_length, log_passage, levels, shafts, 
crash_max, crash_mean, min, max, median_temp, mean_temp, temp_diff, ore) %>% 
mutate(temp_diff_sqrt = sqrt(temp_diff),
temp_diff_log = log(temp_diff),
bin_numeric = as.numeric(bin),
ore = as.factor(ore),
levels = as.factor(levels),
shafts = as.factor(shafts)) %>% 
mutate(levels_numeric = as.numeric(levels), 
shafts_numeric = as.numeric(shafts)) %>% 
filter(site != "Tippy Dam") %>% 
mutate(mean_temp_squared = mean_temp^2)


data <- data_wide3 %>%
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count") %>% 
mutate(year = as.numeric(year), count = as.numeric(count)) %>% drop_na(count)



# Function to calculate the population crash slope
calculate_population_crash <- function(data) {
  data <- data %>%
    arrange(year) %>%
    filter(year <= year[which.min(count)])
  
  if (nrow(data) > 1) {
    # Normalize count to the range [0, 1]
    data <- data %>%
      mutate(
        normalized_count = (count - min(count)) / (max(count) - min(count)),
        standardized_year = scale(year, center = TRUE, scale = TRUE)
      )
    
    # Fit linear model and extract the slope
    fit <- lm(normalized_count ~ standardized_year, data = data)
    slope <- coef(fit)[["standardized_year"]]
    
    return(slope)
  } else {
    return(NA)  # If not enough data points, return NA
  }
}

# Apply to each site
population_crash_slopes <- data %>%
  group_by(site) %>%
  summarize(population_crash = calculate_population_crash(cur_data())) %>% 
  drop_na(population_crash) %>% filter(!site %in% sites_to_remove)

model_df <- model_df %>% 
left_join(population_crash_slopes, by = "site")

min_year1 <- data %>%
  group_by(site) %>%
  reframe(
    min_count = if (any(year > 2012 & !is.na(count))) min(count[year > 2012], na.rm = TRUE) else NA, 
    min_year = if (any(year > 2012 & !is.na(count))) year[year > 2012][which.min(count[year > 2012])] else NA,
    first_year = min(year, na.rm = TRUE))

# Merge the min and max counts back into the orginal data
data_with_min <- data %>% 
left_join(min_year1, by = "site")

data_with_count <- data_with_min %>% 
  group_by(site) %>% 
  mutate(normalized_count = (count - min_count) / (max(count) - min_count)) %>%
  filter(n() >= 3) %>% 
  filter(!site %in% c("Aztec East Adit", "Aztec Mine", "Aztec Upper Drift", 
                      "Copper Peak Adit", "County Line Adit", "Glen Adit #2",
                      "Indiana Mine", "Kochab Cave", "Lafayette East Adit",
                      "Silas Doty Cave", "Spider Cave", "Vivian Adit", "Algonquin Adit #2 (Mark's Adit)",
                      "Child's Adit", "Collin's Adit", "Glen Adit #3", "Hilton Ohio (Hilton #5 Adit)",
                      "Ohio Traprock #61", "Scott Falls Cave", "Eagle River Adit 3 (Lake Superior & Phoenix)",
                      "Eagle River Adit 2 (Lake Superior & Phoenix)", "Hilton (Shaft 1)", "Ohio Traprock Mine #59 (Norwich Adit)", 
                      "Rockport Quarry South Tunnel", "Randville Quarry Mine")) %>%
  arrange(site, year) %>% 
  mutate(rank = dense_rank(year)) %>%
  mutate(normalized_zero_rank = ifelse(normalized_count == 0, rank, NA)) %>% 
  group_by(site) %>% 
  mutate(min_zero_rank = min(normalized_zero_rank, na.rm = TRUE)) %>% 
  filter(rank <= min_zero_rank) %>% 
  top_n(3, rank) %>% 
  arrange(site, rank) %>% 
  mutate(relative_year = row_number() - 1) %>% 
  ungroup()

# Nest the data by site
nested_data1 <- data_with_count %>% 
group_by(site) %>% 
nest()

#print("Nested data:")
#print(nested_data)

# Fit a linear regression model for each site
nested_data1 <- nested_data1 %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    if(nrow(df) >= 2) {
    df <- df %>%
      mutate(year = as.numeric(relative_year),
             count = as.numeric(normalized_count))
    # Fit the linear model
    lm(count ~ year, data = df)
    } else {
      NULL
    }
  }))

# Check the models
# print("Nested data with models:")
# print(nested_data)

# Step 4: Tidy the model outputs
tidied_data1 <- nested_data1 %>%
  filter(!is.null(model)) %>% 
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# Check the tidied data
#print("Tidied data:")
#print(tidied_data)

# Step 5: Filter and select the slope for the 'year' term
regression_results1 <- tidied_data1 %>%
  filter(term == "year") %>%
  select(site, crash = estimate) %>% 
  mutate(crash = ifelse(is.na(crash), 0, crash))

# Check the regression results
#print("Regression results:")
#print(regression_results)

# Step 6: Add the slope to the original data frame
final_datax1 <- data_with_count %>%
  left_join(regression_results1, by = "site")

model_df <- model_df %>% 
left_join(regression_results1, by = "site")
