### THIS CODE MAKES THE DATAFRAME FOR SLIDING_SCALE_MODELS.R #
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
    max_count = if (any(!is.na(count))) max(count, na.rm = TRUE) else NA,
    max_year = if (any(! is.na(count))) year[which.max(count)] else NA,
    mean_count = if (!is.na(min_year)) mean(count[year < min_year], na.rm = TRUE) else NA,
    first_year = min(year, na.rm = TRUE))


# Merge the min and max counts back into the orginal data
data_with_min <- data %>% 
left_join(min_year1, by = "site") %>% 
mutate(normalized_count = (count - min_count) / (max_count - min_count))

# IN TESTING
data_with_count <- data_with_min %>%
  filter(!site %in% c("Aztec East Adit", "Aztec Mine", "Aztec Upper Drift", 
                      "Copper Peak Adit", "County Line Adit", "Glen Adit #2",
                      "Indiana Mine", "Kochab Cave", "Lafayette East Adit",
                      "Silas Doty Cave", "Spider Cave", "Vivian Adit", "Algonquin Adit #2 (Mark's Adit)",
                      "Child's Adit", "Collin's Adit", "Glen Adit #3", "Hilton Ohio (Hilton #5 Adit)",
                      "Ohio Traprock #61", "Scott Falls Cave", "Eagle River Adit 3 (Lake Superior & Phoenix)",
                      "Eagle River Adit 2 (Lake Superior & Phoenix)", "Hilton (Shaft 1)", "Ohio Traprock Mine #59 (Norwich Adit)", 
                      "Rockport Quarry South Tunnel", "Randville Quarry Mine", "B-95 (cave)", "Douglas Houghton Adit #1")) %>%
  group_by(site) %>%
  mutate(normalized_count = (count - min_count) / (max(count) - min_count)) %>%
  filter(n() >= 3) %>%
  arrange(site, year) %>%
  mutate(min_year_norm1 = min(year[normalized_count == 1], na.rm = TRUE), 
         max_year_norm1 = max(year[normalized_count == 0], na.rm =TRUE)) %>%
  filter(year >= min_year_norm1 & year <= max_year_norm1) %>%
  mutate(relative_year = year - 1996) %>%
  ungroup()
#   filter(relative_year %in% tail(unique(relative_year), 4)) %>% 
#   mutate(relative_year = dense_rank(relative_year) - 1) %>% 
#   ungroup()
#   mutate(normalized_zero_rank = ifelse(normalized_count == 0, rank, NA)) %>% 
#   group_by(site) %>% 
#   mutate(min_zero_rank = min(normalized_zero_rank, na.rm = TRUE)) %>% 
#   filter(rank <= min_zero_rank) %>% 
#   top_n(3, rank) %>% 
#   arrange(site, rank) %>% 
#   mutate(relative_year = row_number() - 1) %>% 
#   ungroup()


# Nest the data by site
nested_data1 <- data_with_count %>% 
group_by(site) %>% 
nest()


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


# Some adjustments 
model_df$standing_water[28] <- "yes"
model_df$standing_water[is.na(model_df$standing_water)] <- "no"

# Add dataset for recovering mines and a dataset for all the crashed mines
model_df_recover <- model_df %>% filter(slope > 0) %>% filter(site != "Rockport Quarry North Tunnel")
model_df_crash <- model_df %>%  filter(crash <= 0) %>% 
mutate(new = ifelse(slope > 0, "recovering", "not recovering"))




model_df_crash %>% filter(max_count > 300) %>% 
  ggplot(aes(x = mean_temp, y = crash)) + 
  geom_point(aes(size = mean_count), alpha = 0.7) +  # Add transparency to points for better visibility
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +  # Linear model with confidence interval
  scale_size_continuous(name = "Mean Count") +  # Clean up the legend title for size
  labs(
    title = "Mines that are colder experienced a slower population crash",
    x = "Mean Temperature (°C)",  # X-axis title
    y = "Estimate of population crash (slope)",  # Y-axis title
    size = "Mean Bat Count"  # Cleaned up legend title
  ) +
  theme_bw() +  # Clean and minimal theme
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered and bold title
    axis.title.x = element_text(size = 12),  # X-axis title font size
    axis.title.y = element_text(size = 12),  # Y-axis title font size
    axis.text = element_text(size = 10),  # X and Y-axis text font size
    legend.title = element_text(size = 10),  # Legend title font size
    legend.text = element_text(size = 8),  # Legend text font size
    panel.grid = element_blank(),  # Remove grid lines for a cleaner look
    panel.background = element_rect(fill = "white", color = NA),  # White background
    legend.position = "right"  # Position the legend on the right
  )

ggsave("E:/chapter1_data/figures/final/1996_plot_important.png", width = 8, height = 6)


crash_null <- glm(crash ~ 1, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_null)


crash_f <- glm(crash ~ mean_temp, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_f)

crash_2 <- glm(crash ~ mean_temp + log_passage, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_2)

crash_3 <- glm(crash ~ log_passage, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_3)

crash_4 <- glm(crash ~ mean_temp + log_passage + standing_water, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_4)





######### STUFF TRIED WITH JOE ################# CRASH INTENSITY BUT NEED TO DIVIDE BY YEARS

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

View(data_with_changes)

slice <- data_with_changes %>% 
  group_by(site) %>% slice(1) %>% filter(crash_intensity_year < 0)


slice %>%
  ggplot(aes(x = mean_temp, y = crash_intensity_year)) + 
  geom_point(aes(size = max_count), alpha = 0.7) +  # Add transparency to points for better visibility
  geom_smooth(method = "lm", color = "#6c07ac", se = TRUE) +  # Linear model with confidence interval
  scale_size_continuous(name = "Max Count") +  # Clean up the legend title for size
  labs(
    title = "Mines that are colder experienced a slower population crash",
    x = "Mean Temperature (°C)",  # X-axis title
    y = "Crash intensity year to year",  # Y-axis title
    size = "Max Bat Count"  # Cleaned up legend title
  ) +
  theme_bw() +  # Clean and minimal theme
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered and bold title
    axis.title.x = element_text(size = 12),  # X-axis title font size
    axis.title.y = element_text(size = 12),  # Y-axis title font size
    axis.text = element_text(size = 10),  # X and Y-axis text font size
    legend.title = element_text(size = 10),  # Legend title font size
    legend.text = element_text(size = 8),  # Legend text font size
    panel.grid = element_blank(),  # Remove grid lines for a cleaner look
    panel.background = element_rect(fill = "white", color = NA),  # White background
    legend.position = "right"  # Position the legend on the right
  )

ggsave("E:/chapter1_data/figures/final/crash_intensity_plot_important.png", width = 8, height = 10)

slice$log_passage <- log(slice$passage_length)

crash_f <- glm(crash_intensity ~ mean_temp, weights = mean_count, family = gaussian, data = slice)
summary(crash_f)

crash_2 <- glm(crash_intensity ~ mean_temp + log_passage, weights = mean_count, family = gaussian, data = slice)
summary(crash_2)

crash_3 <- glm(crash_intensity ~ mean_temp + log_passage + standing_water, weights = mean_count, family = gaussian, data = slice)
summary(crash_3)



Sys.setenv(PATH = paste("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

df <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx", sheet = "model_data")

# groups <- df %>% group_by(site) %>% slice(1)
# head(groups)
# mean(groups$num_measurements)
nest <- df %>% filter(year >= decrease_year) %>% select(site, normalized_to_0, relative_year) %>% group_by(site) %>% nest()

nested_data <- nest %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(relative_year),
             count = as.numeric(normalized_to_0))
    # Fit the linear model
    lm(count ~ year, data = df)
  }))

# Step 3: Tidy the model outputs
tidied_data <- nested_data %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# Step 4: Select the slope and intercept for each site and combine them properly
regression_results <- tidied_data %>%
  filter(term %in% c("year", "(Intercept)")) %>%  # Keep both slope and intercept terms
  pivot_wider(names_from = term, values_from = estimate) %>%  # Spread terms into columns
  select(site, intercept = `(Intercept)`, slope = year)  # Select and rename the columns

# Fix NA values by filling them appropriately
regression_results <- regression_results %>%
  group_by(site) %>%
  summarize(
    intercept = max(intercept, na.rm = TRUE),
    slope = max(slope, na.rm = TRUE)
  ) %>%
  ungroup()

regression_results <- regression_results %>% 
mutate(slope = ifelse(is.na(slope), 0, slope),
intercept = ifelse(is.na(intercept), 0, intercept))

model_data_test <- df %>%
  left_join(regression_results, by = "site") %>% 
  mutate(crash = 1 - (min_count / mean_count), 
  log_passage = log(passage_length)) %>% 
  select(site, slope, intercept, crash, mean_temp, decrease_year, recovery_years, min_count, 
  last_count, mean_count, last_year, passage_length, log_passage) %>%
  filter(site != "Tippy Dam") %>%
  group_by(site) %>% slice(1)


# Hypothesis 1
m1 <- lm(slope ~ crash, model_data)
summary(m1)
colors <- c("blue", "white", "red")
# Visualize with updated theme and annotation position
test <- ggplot(model_data_test, aes(x = crash, y = slope)) + 
  geom_point(shape = 20, color = "black", stroke = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature", guide = "none") +  # Remove color legend
  scale_size_continuous(name = "Last Survey\nPopulation Count", guide = "none") +  # Remove size legend
  labs(x = expression(Population~Crash~Severity~(1 - frac(minimum~count,maximum~count))),
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8, family = "Times New Roman"),
    axis.ticks.length = unit(-0.1, "inches"),  # Move tick marks inside
    axis.ticks = element_line(),  # Ensure tick marks are visible
    axis.ticks.top = element_line(),  # Add tick marks to the top
    axis.ticks.right = element_line(),  # Add tick marks to the right
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman"),  # Use Times New Roman for axis text
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman"),  # Use Times New Roman for axis text
    axis.line = element_line(color = "black"),  # Ensure axis lines are visible
    axis.title.x = element_text(margin = margin(t = 12), family = "Times New Roman"), # Axis title font
    axis.title.y = element_text(margin = margin(r = 12), family = "Times New Roman"),  # Axis title font
    axis.text = element_text(size = 12, family = "Times New Roman"),  # Increase axis text size and set font
    axis.title = element_text(size = 12, family = "Times New Roman")  # Increase axis title size and set font
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)  # Remove numbers on the top axis
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)  # Remove numbers on the right axis
  ) +
  annotate("text", x = Inf, y = -Inf, label = "Adjusted R² = 0.3667", 
           hjust = 3.75, vjust = -12, angle = 333, size = 3.5, color = "black", fontface = "italic", family = "Times New Roman")

# Save the plot using ggsave
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/test.png", 
       plot = test, width = 8, height = 6, dpi = 300)
