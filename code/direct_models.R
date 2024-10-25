
# THIS CODE IS FOR MORE DIRECT MODELS USING COUNTS AS RESPONSE VARIABLE 
rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/code")
##################################### Models for mines on the sliding scale method ###################################
source("sliding_scale.R")

counts <- model_data1 %>% filter(count > 0) %>% filter(relative_year >= 0) %>% 
filter(site %in% c("Adventure Adit/ Rock Fall", "Adventure Mine", "Child's Adit", "Delaware Mine", "Derby Adit", 
"Flintsteel Adit", "Glen Adit #1", "Iron Mountain Iron Mine (Tourist Mine)", "Jones' Adit", "Keel Ridge Mine", 
"Keel Ridge Shaft", "Lafayette East Shaft", "Mass C Adit", "Mead Adit of Carp Lake Mine", "Merchant Mine", 
"North Cliff Mine (Shaft #3?)", "Norway Mine", "Ohio Traprock Mine #60 (Norwich Adit)", 
"Old Flintsteel River Adit B", "Quincy Mine Adit", "Quinnesec Adit", "Silver Mountain Mine", "South Bluff Adit", 
"South Bluff East Adit", "South Lake Mine", "Taylor Adit", "Windsor Shaft #3", "Young's Adit", "the belt mine"))

colnames(counts)
linear_model <- lm(count ~ relative_year * mean_temp, data = counts)
summary(linear_model)

exp_model <- lm(log(count) ~ relative_year * mean_temp, data = counts)
summary(exp_model)

poly_model <- lm(count ~ poly(relative_year, 2) * mean_temp, data = counts)
summary(poly_model)

library(ggplot2)

# Create predicted values for each model
counts$linear_pred <- predict(linear_model)
counts$exp_pred <- exp(predict(exp_model))  # For the exponential model, take the exponent of the predicted values
counts$poly_pred <- predict(poly_model)

# Plot observed data with predictions from each model
ggplot(counts, aes(x = mean_temp, y = count)) +
  geom_point(aes(color = "Observed"), size = 2, alpha = 0.6) +  # Observed data
  #geom_line(aes(y = linear_pred, color = "Linear Model"), size = 1) +  # Linear model predictions
  #geom_line(aes(y = exp_pred, color = "Exponential Model"), size = 1, linetype = "dashed") +  # Exponential model predictions
  #geom_line(aes(y = poly_pred, color = "Polynomial Model"), size = 1, linetype = "dotted") +  # Polynomial model predictions
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Observed Count and Model Predictions",
       x = "Mean Temperature (C)",
       y = "Count",
       color = "Model") +
  scale_color_manual(values = c("Observed" = "black", 
                                "Linear Model" = "blue", 
                                "Exponential Model" = "red", 
                                "Polynomial Model" = "green")) + 
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/mean_temp_count.png", width = 8, height=6)

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
library(readxl)
library(tidyverse)
library(broom)
library(tidyr)
library(purrr)
df <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx")

nest <- df %>% filter(year >= decrease_year) %>% select(site, count, normalized_count, relative_year, year_from_min) %>% group_by(site) %>% nest()

# STEP 2: Fit a linear regression model for each site
nested_data <- nest %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(year_from_min), 
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

colors <- c("blue", "white", "red")

your_data %>%
group_by(site) %>% 
mutate(year = as.numeric(year)) %>% 
filter(!is.na(count)) %>% 
  ggplot(aes(x = year, y = normalized_count, group = site)) +
  geom_line(show.legend = FALSE) +
  geom_line(aes(y=predicted_count, color = mean_temp), size = 1.2, show.legend = FALSE) +
  scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
  facet_wrap(~site, scales = "free_y") +  # Facet by site with free y-axis scales
  scale_x_continuous(
    breaks = seq(min(your_data$year), max(your_data$year), by = 4),
    labels = function(x) sprintf("%02d", x %% 100)
  ) +
  geom_vline(xintercept = 2014, color = "red", linetype = "dashed", size = 0.5) +  # Adjust size here
  theme_dark() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Recovering mines with recovery estimate overlayed the normalized count",
       x = "Year",
       y = "Normalized Population Count")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/trend_line_test.png", width = 12, height=12)


###########################################################################################################
############ CREATE A SLOPE AND INTERCEPT FOR THE NORMALIZED DATA ######################################
######################################################################################################

nest1 <- df %>% filter(year >= decrease_year) %>% select(site, normalized_count, year_from_min, relative_year) %>% group_by(site) %>% nest()

df %>% filter(year >= decrease_year) %>% select(site, year_from_min, normalized_count) %>% View()
# STEP 2: Fit a linear regression model for each site
nested_data <- nest1 %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(year_from_min),
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

# Step 6: Add the slope to the original data frame
selection <- your_data %>% select(site, slope_count, intercept_count)

to_model <- df %>%
  left_join(regression_results, by = "site")

to_model <- to_model %>% 
  mutate(crash = 1 - (min_count /mean_count)) %>%  
  select(site, slope, intercept, mean_temp, decrease_year, recovery_years, min_count, last_count, mean_count, last_year, passage_length) %>% 
  filter(site != "Tippy Dam") %>% 
  group_by(site) %>% slice(1)

####################################################################################################################
################################# BAYESIAN MODELING ################################################################
####################################################################################################################
####################################################################################################################
library(brms)
library(rstan)
Sys.setenv(PATH = paste("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

# Define the prior
prior <- c(
  prior(normal(0, 1), class = "b", coef = "mean_temp"),
  prior(normal(0,1), class = "Intercept"))

prior1 <- prior(normal(0,1), class = "Intercept")


to_model <- to_model %>% mutate(slope_shifted = slope + abs(min(slope)) + 0.01) %>% 
filter(slope > 0)


slope_model <- brm(
  formula = slope ~ mean_temp,
  data = to_model,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

summary(slope_model)


colors <- c("blue", "white", "red")
bayesian_r2 <- bayes_R2(slope_model)
print(bayesian_r2)
b_r2_slope <- bayesian_r2[1, "Estimate"]
bayesian_r2_ci <- quantile(bayesian_r2, probs = c(0.025, 0.975))

to_model <- to_model %>% ungroup()
# Create a finer grid of mean_temp (remove 'site' or any other grouping variable)
prediction_grid_s <- to_model %>%
  select(mean_temp, recovery_years) %>%  # Ensure only relevant columns are selected
  tidyr::expand(mean_temp = seq(min(mean_temp, na.rm = TRUE), max(mean_temp, na.rm = TRUE), length.out = 100)) %>%
  mutate(recovery_years = median(to_model$recovery_years, na.rm = TRUE))

# Generate predictions using the fitted values from the model
predicted_slope_values <- fitted(slope_model, newdata = prediction_grid_s, summary = TRUE)[, "Estimate"]

# Add the predicted values to the dataset
prediction_grid_s <- prediction_grid_s %>%
  mutate(predicted_slope = predicted_slope_values)


# Plot the slope model
ggplot(to_model, aes(x = mean_temp, y = slope_count)) +
  geom_point(alpha = 0.5) +  # Plot observed data
  geom_line(data=prediction_grid_s, aes(mean_temp, y = predicted_slope), color = "darkblue", linewidth = 1) +
  scale_size_continuous(name = "Last Survey\nPopulation Count") + 
  labs(title = "Mines with colder mean temperatures have higher recovery rates for Myotis bats",
       x = "Mean Temperature (C)",
       y = "Recovery Rate (Slope)") +
  annotate("text", x = Inf, y = Inf, label = paste("Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 2.75, vjust = 1.5, size = 4, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12),        # Title font size
    axis.title.x = element_text(size = 10),      # X-axis title font size
    axis.title.y = element_text(size = 10),      # Y-axis title font size
    axis.text.x = element_text(size = 8),        # X-axis text font size
    axis.text.y = element_text(size = 8),        # Y-axis text font size
    legend.title = element_text(size = 10),      # Legend title font size
    legend.text = element_text(size = 8),
    panel.grid = element_blank()
  )

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_mean_temp.png", width = 8, height=6)


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

model_data_recover <- model_data %>% filter(slope > 0)

slope_model <- brm(
  formula = slope ~ mean_temp + offset(recovery_years),
  data = model_data_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model1 <- brm(
  formula = slope ~ mean_temp + I(mean_temp^2) + offset(recovery_years),
  data = model_data_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model2 <- brm(
  formula = slope ~ mean_temp + log_passage + offset(recovery_years),
  data = model_data_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

no_offset <- brm(
  formula = slope ~ mean_temp,
  data = model_data_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

no_offset1 <- brm(
  formula = slope ~ mean_temp + log_passage,
  data = model_data_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

no_offset2 <- brm(
  formula = slope ~ mean_temp + I(mean_temp^2),
  data = model_data_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

############################################################################################
##################### Model Comparison ##################################################
# LOO-CV tends to be more stable, especially in small datasets or models with influencial observations
loo_slope_model <- loo(slope_model, moment_match = TRUE)
loo_slope_model1 <- loo(slope_model1, moment_match = TRUE)
loo_slope_model2 <- loo(slope_model2, moment_match = TRUE)
loo_no_offset <- loo(no_offset, moment_match = TRUE)
loo_no_offset1 <- loo(no_offset1, moment_match = TRUE)
loo_no_offset2 <- loo(no_offset2, moment_match = TRUE)

# Extract ELPD values from each model
elpd_slope_model <- loo_slope_model$estimates["elpd_loo", "Estimate"]
elpd_slope_model1 <- loo_slope_model1$estimates["elpd_loo", "Estimate"]
elpd_slope_model2 <- loo_slope_model2$estimates["elpd_loo", "Estimate"]
elpd_no_offset <- loo_no_offset$estimates["elpd_loo", "Estimate"]
elpd_no_offset1 <- loo_no_offset1$estimates["elpd_loo", "Estimate"]
elpd_no_offset2 <- loo_no_offset2$estimates["elpd_loo", "Estimate"]

loo_comparison <- loo_compare(loo_slope_model, loo_slope_model1, loo_slope_model2,
loo_no_offset, loo_no_offset1, loo_no_offset2)
# Create a summary table with ELPD, elpd_diff, and se_diff
comparison_table <- data.frame(
  ELPD = c(elpd_slope_model, elpd_slope_model1, elpd_slope_model2, 
  elpd_no_offset, elpd_no_offset1, elpd_no_offset2),
  elpd_diff = loo_comparison[, "elpd_diff"],
  se_diff = loo_comparison[, "se_diff"]
)
# Print the table
print(comparison_table)

summary(slope_model)
bayesian_r2 <- bayes_R2(slope_model)
print(bayesian_r2)

summary(no_offset)
bayesian_r2 <- bayes_R2(no_offset)
print(bayesian_r2)

colors <- c("blue", "white", "red")
b_r2_slope <- bayesian_r2[1, "Estimate"]
bayesian_r2_ci <- quantile(bayesian_r2, probs = c(0.025, 0.975))

model_data_ungroup <- model_data_recover %>% ungroup()

# Create a grid of mean_temp
prediction_grid_s <- model_data_ungroup %>%
  select(mean_temp, recovery_years) %>%  # Ensure only relevant columns are selected
  tidyr::expand(mean_temp = seq(min(mean_temp, na.rm = TRUE), max(mean_temp, na.rm = TRUE), length.out = 100)) %>%
  mutate(recovery_years = median(model_data_ungroup$recovery_years, na.rm = TRUE), 
  log_passage = median(model_data_ungroup$log_passage))

# Generate predictions using the fitted values from the model
predicted_slope_values <- fitted(no_offset, newdata = prediction_grid_s, summary = TRUE)[, "Estimate"]

# Add the predicted values to the dataset
prediction_grid_s <- prediction_grid_s %>%
  mutate(predicted_slope = predicted_slope_values)

  # Plot the slope model
ggplot(model_data_ungroup, aes(x = mean_temp, y = slope)) +
  geom_point(alpha = 0.5) +  # Plot observed data
  geom_line(data=prediction_grid_s, aes(mean_temp, y = predicted_slope), color = "darkblue", linewidth = 1) +
  scale_size_continuous(name = "Last Survey\nPopulation Count") + 
  labs(title = "Mines with colder mean temperatures have higher recovery rates for Myotis bats",
       x = "Mean Temperature (C)",
       y = "Recovery Rate (Slope)") +
  annotate("text", x = Inf, y = Inf, label = paste("Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 2.75, vjust = 1.5, size = 4, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12),        # Title font size
    axis.title.x = element_text(size = 10),      # X-axis title font size
    axis.title.y = element_text(size = 10),      # Y-axis title font size
    axis.text.x = element_text(size = 8),        # X-axis text font size
    axis.text.y = element_text(size = 8),        # Y-axis text font size
    legend.title = element_text(size = 10),      # Legend title font size
    legend.text = element_text(size = 8),
    panel.grid = element_blank()
  )
