library(readxl)
library(tidyverse)
library(broom)
library(tidyr)
library(purrr)

Sys.setenv(PATH = paste("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

df <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx")

nest <- df %>% filter(year >= decrease_year) %>% select(site, count, normalized_count, relative_year, year_from_min) %>% group_by(site) %>% nest()

# STEP 2: Fit a linear regression model for each site
nested_data <- nest %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(year_from_min), 
              count = as.numeric(count))
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
  select(site, intercept_count = `(Intercept)`, slope_count = year)  # Select and rename the columns

# Fix NA values by filling them appropriately
regression_results <- regression_results %>%
  group_by(site) %>%
  summarize(
    intercept_count = max(intercept_count, na.rm = TRUE),
    slope_count = max(slope_count, na.rm = TRUE)
  ) %>%
  ungroup()

regression_results <- regression_results %>% 
mutate(slope_count = ifelse(is.na(slope_count), 0, slope_count),
intercept_count = ifelse(is.na(intercept_count), 0, intercept_count))

# Step 6: Add the slope to the original data frame
final_data <- df %>%
  left_join(regression_results, by = "site")

# Create the trend line data using slope and intercept
your_data <- final_data %>%
  mutate(predicted_count = intercept_count + slope_count * relative_year)

####### FIGURE WHICH MODELS ARE BEST? ###############
library(tidyverse)
library(readxl)
library(brms)
library(rstan)
library(tidyr)
library(purrr)
library(broom)
df <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx")

nest <- df %>% filter(winter_year >= decrease_year) %>% select(site, normalized_count, relative_year) %>% group_by(site) %>% nest()

nested_data <- nest %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(relative_year),
             count = as.numeric(normalized_count))
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

model_data <- df %>%
  left_join(regression_results, by = "site") %>% 
  mutate(crash = 1 - (min_count / mean_count), 
  log_passage = log(passage_length)) %>% 
  select(site, slope, intercept, crash, mean_temp, decrease_year, recovery_years, min_count, 
  last_count, mean_count, last_year, passage_length, log_passage) %>%
  filter(site != "Tippy Dam") %>%
  group_by(site) %>% slice(1)

model_data$slope_weighted <- model_data$slope * sqrt(model_data$last_count)

model_data_recover <- model_data %>% filter(slope > 0)

# Crash model selection

# Define the prior
prior <- c(
  prior(normal(0, 1), class = "b", coef = "mean_temp"),
  prior(normal(0,1), class = "Intercept"))

prior1 <- prior(normal(0,1), class = "Intercept")

null_model_crash <- brm(
  formula = crash ~ 1,
  data = model_data_recover,
  family = Beta(),
  prior = prior1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

crash_model <- brm(
  formula = crash ~ mean_temp + I(mean_temp^2),
  data = model_data_recover,
  family = Beta(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

crash_model_linear <- brm(
  formula = crash ~ mean_temp,
  data = model_data_recover,
  family = Beta(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

crash_model1 <- brm(
  formula = crash ~ mean_temp + log_passage,
  data = model_data_recover,
  family = Beta(),
  prior = prior,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

loo_crash_null <- loo(null_model_crash, moment_match = TRUE)
loo_crash_model <- loo(crash_model, moment_match = TRUE)
loo_crash_model_linear <- loo(crash_model_linear, moment_match = TRUE)
loo_crash_model1 <- loo(crash_model1, moment_match = TRUE)

# Extract ELPD values from each model
elpd_null_crash <- loo_crash_null$estimates["elpd_loo", "Estimate"]
elpd_crash_model <- loo_crash_model$estimates["elpd_loo", "Estimate"]
elpd_crash_model_linear <- loo_crash_model_linear$estimates["elpd_loo", "Estimate"]
elpd_crash_model1 <- loo_crash_model1$estimates["elpd_loo", "Estimate"]

# Get the comparison results (elpd_diff and se_diff)
loo_comparison1 <- loo_compare(loo_crash_null, loo_crash_model, loo_crash_model_linear, loo_crash_model1)

# Create a summary table with ELPD, elpd_diff, and se_diff
comparison_table1 <- data.frame(
  ELPD = c(elpd_null_crash, elpd_crash_model, elpd_crash_model_linear, elpd_crash_model1),
  elpd_diff = loo_comparison1[, "elpd_diff"],
  se_diff = loo_comparison1[, "se_diff"]
)
# Print the table
print(comparison_table1)


# Slope model selection
null_model_slope <- brm(
  formula = slope ~ 1,
  data = model_data_recover,
  family = student(),
  prior = prior1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

slope_model <- brm(
  formula = slope ~ mean_temp + offset(recovery_years),
  data = model_data_recover,
  family = student(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model1 <- brm(
  formula = slope ~ mean_temp + I(mean_temp^2) + offset(recovery_years),
  data = model_data_recover,
  family = student(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model2 <- brm(
  formula = slope ~ mean_temp + log_passage + offset(recovery_years),
  data = model_data_recover,
  family = student(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

no_offset <- brm(
  formula = slope ~ mean_temp,
  data = model_data_recover,
  family = student(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

no_offset1 <- brm(
  formula = slope ~ mean_temp + log_passage,
  data = model_data_recover,
  family = student(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

no_offset2 <- brm(
  formula = slope ~ mean_temp + I(mean_temp^2),
  data = model_data_recover,
  family = student(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

null_model_slope_offset <- brm(
  formula = slope ~ 1 + offset(recovery_years),
  data = model_data_recover,
  family = student(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
############################################################################################
##################### Model Comparison ##################################################
########################################################################################
# LOO-CV tends to be more stable, especially in small datasets or models with influencial observations
loo_null_slope <- loo(null_model_slope, moment_match = TRUE)
loo_slope_model <- loo(slope_model, moment_match = TRUE)
loo_slope_model1 <- loo(slope_model1, moment_match = TRUE)
loo_slope_model2 <- loo(slope_model2, moment_match = TRUE)
loo_no_offset <- loo(no_offset, moment_match = TRUE)
loo_no_offset1 <- loo(no_offset1, moment_match = TRUE)
loo_no_offset2 <- loo(no_offset2, moment_match = TRUE)
loo_null_slope_offset <- loo(null_model_slope_offset, moment_match = TRUE)


# Extract ELPD values from each model
elpd_null_slope <- loo_null_slope$estimates["elpd_loo", "Estimate"]
elpd_slope_model <- loo_slope_model$estimates["elpd_loo", "Estimate"]
elpd_slope_model1 <- loo_slope_model1$estimates["elpd_loo", "Estimate"]
elpd_slope_model2 <- loo_slope_model2$estimates["elpd_loo", "Estimate"]
elpd_no_offset <- loo_no_offset$estimates["elpd_loo", "Estimate"]
elpd_no_offset1 <- loo_no_offset1$estimates["elpd_loo", "Estimate"]
elpd_no_offset2 <- loo_no_offset2$estimates["elpd_loo", "Estimate"]
elpd_null_offset <- loo_null_slope_offset$estimates["elpd_loo", "Estimate"]

loo_comparison <- loo_compare(loo_slope_model, loo_slope_model1, loo_slope_model2,
loo_no_offset, loo_no_offset1, loo_no_offset2, loo_null_slope, loo_null_slope_offset)
# Create a summary table with ELPD, elpd_diff, and se_diff
comparison_table <- data.frame(
  ELPD = c(elpd_slope_model, elpd_slope_model1, elpd_slope_model2, 
  elpd_no_offset, elpd_no_offset1, elpd_no_offset2, elpd_null_slope, elpd_null_offset),
  elpd_diff = loo_comparison[, "elpd_diff"],
  se_diff = loo_comparison[, "se_diff"]
)
# Print the table
print(comparison_table)

# Hypothesis 3
df1 <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx", sheet = "proportions")

overall_stats <- df1 %>%
  group_by(period) %>%
  summarise(
    overall_mean_count = mean(count, na.rm = TRUE),
    overall_sd_count = sd(count, na.rm = TRUE)
  )

pop_proportions <- df1 %>% group_by(site, period) %>%
  summarise(
    mean_count_site = mean(count, na.rm = TRUE),  # Mean count for the site and period
    site_mean_temp = first(mean_temp)  # Assuming site_mean_temp is constant per site
  ) %>%
  left_join(overall_stats, by = "period") %>%  
  ungroup()

combined <- pop_proportions %>%
  pivot_wider(
    names_from = period,
    values_from = c(mean_count_site, site_mean_temp, overall_mean_count, overall_sd_count),
    names_prefix = "period_"
  ) %>% 
  select(site, mean_count_before = mean_count_site_period_before, mean_count_after = mean_count_site_period_after,
  mean_temp = site_mean_temp_period_before, 
  )

mine_proportions <- combined %>% filter(site != "Tippy Dam") %>% 
reframe(site, mean_temp, sum_before = sum(mean_count_before), 
proportion_before = (mean_count_before / sum_before), sum_after = sum(mean_count_after), 
proportion_after = (mean_count_after / sum_after), mean_count_before, mean_count_after) %>% 
mutate(props = (proportion_after - proportion_before), mean_temp_squared = mean_temp^2)


im <- mine_proportions %>% filter(mean_count_before > 100)
# Fit model
null_model <- lm(props ~ 1, data = im)
model <- lm(props ~ mean_temp, data = im)
model1 <- lm(props ~ mean_temp + I(mean_temp^2), data = im)
adj_r_squared1 <- summary(model1)$adj.r.squared
adj_r_squared_text1 <- paste0("Adjusted R² = ", round(adj_r_squared1, 4))
summary(model1)
# Adjusted R-squared for all models
adj_r_squared_null <- summary(null_model)$adj.r.squared
adj_r_squared_model <- summary(model)$adj.r.squared
adj_r_squared_model1 <- summary(model1)$adj.r.squared
# Print results
cat("Null Model Adjusted R²:", round(adj_r_squared_null, 4), "\n")
cat("Model Adjusted R²:", round(adj_r_squared_model, 4), "\n")
cat("Model1 Adjusted R²:", round(adj_r_squared_model1, 4), "\n")

AIC(null_model)
AIC(model)
AIC(model1)
anova(model, model1)

library(MuMIn)
AICc(null_model)
AICc(model)
AICc(model1)
