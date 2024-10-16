### THIS CODE MAKES THE DATAFRAME FOR SLIDING_SCALE_MODELS.R #
rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/code")
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
    max_year = year[which.max(count)]) %>% 
  mutate(mean_count_year = case_when(
    site == "Adventure Adit/ Rock Fall" ~ 2020,
    site == "Adventure Mine" ~ 2020, 
    site == "Child's Adit" ~ 2018, 
    site == "Delaware Mine" ~ 2016, 
    site == "Derby Adit" ~ 2020, 
    site == "Flintsteel Adit" ~ 2019, 
    site == "Glen Adit #1" ~ 2021, 
    site == "Iron Mountain Iron Mine (Tourist Mine)" ~ 2017, 
    site == "Jones' Adit" ~ 2017, 
    site == "Keel Ridge Mine" ~ 2017, 
    site == "Keel Ridge Shaft" ~ 2017, 
    site == "Lafayette East Shaft" ~ 2018, 
    site == "Mass C Adit" ~ 2021, 
    site == "Mead Adit of Carp Lake Mine" ~ 2017, 
    site == "Merchant Mine" ~ 2017, 
    site == "North Cliff Mine (Shaft #3?)" ~ 2016, 
    site == "Norway Mine" ~ 2017, 
    site == "Ohio Traprock Mine #60 (Norwich Adit)" ~ 2017, 
    site == "Old Flintsteel River Adit B" ~ 2018, 
    site == "Quincy Mine Adit" ~ 2017, 
    site == "Quinnesec Adit" ~ 2017, 
    site == "Silver Mountain Mine" ~ 2017, 
    site == "South Bluff Adit" ~ 2017, 
    site == "South Bluff East Adit" ~ 2017, 
    site == "South Lake Mine" ~ 2021, 
    site == "Taylor Adit" ~ 2021, 
    site == "Windsor Shaft #3" ~ 2018, 
    site == "Young's Adit" ~ 2017, 
    site == "the belt mine" ~ 2020, 
    TRUE ~ NA_real_
  ))
  
  
min_max_count <- min_max_count %>% mutate(mean_count_year = as.numeric(mean_count_year),
mini_year = as.numeric(mini_year), 
mean_count_year = if_else(
    is.na(mean_count_year),
    mini_year, 
    mean_count_year
  ))


# Merge the min and max counts back into the orginal data
data_with_min_max <- data %>% 
left_join(min_max_count, by = "site")

mean_count_data <- data_with_min_max %>% 
filter(year < mean_count_year) %>% 
group_by(site) %>% 
summarize(mean_count = mean(count, na.rm = TRUE))

data_with_min_max2 <- data_with_min_max %>% 
left_join(mean_count_data, by = "site")

# Normalize the count data
normalize_count <- data_with_min_max2 %>% 
mutate(normalize_count = (count - min_count) / (mean_count - min_count))

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

# Nest the data by site
nested_data <- filter_data %>% 
group_by(site) %>% 
nest()

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

# Step 4: Tidy the model outputs
tidied_data <- nested_data %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

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

# Add maximum crash from maximum to minimum 
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
    max_count = if (any(!is.na(count))) max(count, na.rm = TRUE) else NA,
    max_year = if (any(! is.na(count))) year[which.max(count)] else NA,
    mean_count = if (!is.na(min_year)) mean(count[year < min_year], na.rm = TRUE) else NA,
    first_year = min(year, na.rm = TRUE))

# Merge the min and max counts back into the orginal data
data_with_min <- data %>% 
left_join(min_year1, by = "site") %>% 
mutate(normalized_count = (count - min_count) / (max_count - min_count))

### THIS SHOULD NOT BE NEEDED BECAUSE WE AREN'T CALCULATING A ESTIMATE BUT ACTUAL A YEAR TO YEAR DIFFERENCE ######

# data_with_count <- data_with_min %>%
#   filter(!site %in% c("Aztec East Adit", "Aztec Mine", "Aztec Upper Drift", 
#                       "Copper Peak Adit", "County Line Adit", "Glen Adit #2",
#                       "Indiana Mine", "Kochab Cave", "Lafayette East Adit",
#                       "Silas Doty Cave", "Spider Cave", "Vivian Adit", "Algonquin Adit #2 (Mark's Adit)",
#                       "Child's Adit", "Collin's Adit", "Glen Adit #3", "Hilton Ohio (Hilton #5 Adit)",
#                       "Ohio Traprock #61", "Scott Falls Cave", "Eagle River Adit 3 (Lake Superior & Phoenix)",
#                       "Eagle River Adit 2 (Lake Superior & Phoenix)", "Hilton (Shaft 1)", "Ohio Traprock Mine #59 (Norwich Adit)", 
#                       "Rockport Quarry South Tunnel", "Randville Quarry Mine", "B-95 (cave)", "Douglas Houghton Adit #1")) %>%
#   group_by(site) %>%
#   mutate(normalized_count = (count - min_count) / (max(count) - min_count)) %>%
#   filter(n() >= 3) %>%
#   arrange(site, year) %>%
#   mutate(min_year_norm1 = min(year[normalized_count == 1], na.rm = TRUE), 
#          max_year_norm1 = max(year[normalized_count == 0], na.rm =TRUE)) %>%
#   filter(year >= min_year_norm1 & year <= max_year_norm1) %>%
#   mutate(relative_year = year - 1996) %>%
#   ungroup()

#######################################################################################################
######################### CRASH INTENSITY FROM SURVEY TO SURVEY ###############################################
# create the variable crash intensity by site and find the maximum crash intensity (most negative)
data_with_changes <- data_with_min %>% 
arrange(site, year) %>% 
group_by(site) %>%
mutate(
  change_in_bats = (normalized_count - lag(normalized_count))) %>% 
mutate(crash_intensity = if(all(is.na(change_in_bats))) NA_real_ else min(change_in_bats, na.rm = TRUE)) %>%
mutate(
  lag_year = year - lag(year), 
  crash_intensity_year = if(all(is.na(crash_intensity / lag_year))) NA_real_ else min(crash_intensity/lag_year, na.rm = TRUE))%>% 
ungroup()

# only need one per site slice 1 by site
slice <- data_with_changes %>% 
  group_by(site) %>% slice(1) %>% filter(crash_intensity < 0) %>% select(site, crash_intensity)

# add crash intensity to model dataframe
model_df <- model_df %>%
left_join(slice, by = "site")

# Some adjustments before modeling
model_df$standing_water[28] <- "yes"
model_df$standing_water[is.na(model_df$standing_water)] <- "no"

# only include sites that are recovering 
model_df_recover <- model_df %>% filter(slope > 0) %>% filter(site != "Rockport Quarry North Tunnel")

# weight the response variables before modeling
model_df_recover$weight_sqrt <- sqrt(model_df_recover$last_count)
model_df$weight_sqrt1 <- sqrt(model_df$mean_count)
model_df_recover$slope_weighted <- model_df_recover$slope * model_df_recover$weight_sqrt
model_df_recover$recovery_years_weighted <- model_df_recover$recovery_years * model_df_recover$weight_sqrt
model_df$crash_weighted <- model_df$crash_intensity * model_df$weight_sqrt1
model_df$crash_mean_weighted <- model_df$crash_mean * model_df$weight_sqrt1
