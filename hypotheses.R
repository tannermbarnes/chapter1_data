library(tidyverse)
library(readxl)
library(brms)
library(rstan)
library(tidyr)
library(purrr)
library(broom)
library(lmtest)

Sys.setenv(PATH = paste("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

df <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx", sheet = "model_data")

nest <- df %>% filter(year >= decrease_year) %>% select(site, normalized_count, relative_year) %>% group_by(site) %>% nest()

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

# Hypothesis 1
m1 <- lm(slope ~ crash, model_data)
summary(m1)
colors <- c("blue", "white", "red")
# Visualize
ggplot(model_data, aes(x=crash, y=slope)) + 
geom_point(aes(fill = mean_temp, size = last_count), shape = 21, color = "black", stroke = 0.5) + 
geom_smooth(method = "lm", se = FALSE, color = "black") +
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
scale_size_continuous(name = "Last Survey\nPopulation Count") + 
labs(title = "The recovery rate and the population crash are related",
x = "Population Crash (1 - (minimum / mean before WNS))",
y = "Slope (Recovery Rate)") + 
theme_bw() +
theme(
  panel.grid = element_blank(),
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)) +
  annotate("text", x = Inf, y = Inf, label = "Adjusted R² = 0.3667", 
           hjust = 2.5, vjust = 2, size = 3, color = "black", fontface = "italic")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_population_crash.png", width = 8, height=6)

# Hypothesis 1 comparison to null and model assumptions
m0 <- lm(slope ~ 1, data = model_data)
AIC(m0, m1) # lower AIC (more negative) indicates a better fit
anova(m0, m1) # p-value is signficant suggesting full model is a better fit
png("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/h1_mod_assumptions.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(m1)
dev.off()
# fitted vs residuals indicate variables don't have a constant variance
shapiro.test(residuals(m1)) # p-value indicates residuals or normally distributed
dwtest(m1)

# Hypothesis 2 Recovery Rate
# Only include sites that are recovering
model_data_recover <- model_data %>% filter(slope > 0)

# Define the prior
prior <- c(
  prior(normal(0, 1), class = "b", coef = "mean_temp"),
  prior(normal(0,1), class = "Intercept"))

prior1 <- prior(normal(0,1), class = "Intercept")

slope_model <- brm(
  formula = slope ~ mean_temp,
  data = model_data_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

summary(slope_model)
bayesian_r2 <- bayes_R2(slope_model)
print(bayesian_r2)

colors <- c("blue", "white", "red")
b_r2_slope <- bayesian_r2[1, "Estimate"]
bayesian_r2_ci <- quantile(bayesian_r2, probs = c(0.025, 0.975))

model_data_ungroup <- model_data_recover %>% ungroup()

# Create a grid of mean_temp
prediction_grid_s <- model_data_ungroup %>%
  select(mean_temp, recovery_years) %>%  # Ensure only relevant columns are selected
  tidyr::expand(mean_temp = seq(min(mean_temp, na.rm = TRUE), max(mean_temp, na.rm = TRUE), length.out = 100)) %>%
  mutate(recovery_years = median(model_data_ungroup$recovery_years, na.rm = TRUE))

# Generate predictions using the fitted values from the model
predicted_slope_values <- fitted(slope_model, newdata = prediction_grid_s, summary = TRUE)[, "Estimate"]

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

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_mean_temp.png", width = 8, height=6)


# Hypothesis 2 Crash Rate
crash_model_linear <- brm(
  formula = crash ~ mean_temp,
  data = model_data_recover,
  family = Beta(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99))

summary(crash_model_linear)
bayesian_r2.1 <- bayes_R2(crash_model_linear)
print(bayesian_r2.1)

b2_crash <- bayesian_r2.1[1, "Estimate"]

# Step 1: Create a finer grid of mean_temp for smoother prediction lines (without 'site')
prediction_grid <- tibble(mean_temp = seq(min(model_data_recover$mean_temp), max(model_data_recover$mean_temp), length.out = 100))

# Step 2: Generate predictions using the fitted values from the linear model
predicted_slope_values1 <- fitted(crash_model_linear, newdata = prediction_grid, summary = TRUE)[, "Estimate"]

# Step 3: Add the predicted values to the dataset
prediction_grid <- prediction_grid %>%
  mutate(predicted_crash = predicted_slope_values1)

# Step 4: Plot with a straight line for the linear model
ggplot(model_data_recover, aes(x = mean_temp, y = crash)) +
  geom_point(aes(size = mean_count), alpha = 0.5) +  # Plot observed data
  geom_line(data = prediction_grid, aes(x=mean_temp, y = predicted_crash), color = "darkgreen", linewidth = 1) +
  scale_size_continuous(name = "Mean\nPopulation\nSize") + 
  labs(title = "Mines with colder mean temperatures have lower crash rates for Myotis Bats",
       x = "Mean Temperature (C)",
       y = "Crash Rate (1 - (minimum count / maximum count))") +
annotate("text", x = max(model_data_recover$mean_temp) - 0.5, 
         y = max(model_data_recover$crash) - 0.05, 
         label = paste("Bayesian R² =", round(b2_crash, 4)), 
         hjust = 3.25, vjust = 3.75, size = 4, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12),        # Title font size
    axis.title.x = element_text(size = 10),      # X-axis title font size
    axis.title.y = element_text(size = 10),      # Y-axis title font size
    axis.text.x = element_text(size = 8),        # X-axis text font size
    axis.text.y = element_text(size = 8),        # Y-axis text font size
    legend.title = element_text(size = 10),      # Legend title font size
    legend.text = element_text(size = 8),
    panel.grid = element_blank()        # Legend text font size
  )

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/crash_rate_mean_temp.png", width = 8, height=6)

# Hypothesis 2 combined plots
# Combine datasets for predictions
combined_prediction <- prediction_grid %>%
  rename(predicted_crash = predicted_crash) %>%
  mutate(predicted_slope = prediction_grid_s$predicted_slope)

# Combined plot
ggplot() +
  # Recovery Rate (Left Y-Axis)
  geom_point(data = model_data_ungroup, aes(x = mean_temp, y = slope), alpha = 0.5, color = "blue") +
  geom_line(data = combined_prediction, aes(x = mean_temp, y = predicted_slope), color = "darkblue", linewidth = 1) +
  # Crash Rate (Right Y-Axis)
  geom_point(data = model_data_recover, aes(x = mean_temp, y = crash), alpha = 0.5, color = "darkgreen") +
  geom_line(data = combined_prediction, aes(x = mean_temp, y = predicted_crash), color = "green", linewidth = 1) +
  # Add Secondary Axis
  scale_y_continuous(
    name = "Recovery Rate (Slope)",
    sec.axis = sec_axis(~ ., name = "Crash Rate (1 - (minimum count / maximum count))")
  ) +
  labs(
    title = "Recovery and Crash Rates in Relation to Mean Temperature",
    x = "Mean Temperature (°C)"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("Recovery Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 2.75, vjust = 1.5, size = 4, color = "blue") +
  annotate("text", x = max(model_data_recover$mean_temp) - 0.5, 
           y = max(model_data_recover$crash) - 0.05, 
           label = paste("Crash Bayesian R² =", round(b2_crash, 4)), 
           hjust = 3.25, vjust = 3.75, size = 4, color = "darkgreen") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 10),
    axis.title.y.left = element_text(size = 10, color = "darkblue"),
    axis.title.y.right = element_text(size = 10, color = "darkgreen"),
    axis.text.x = element_text(size = 8),
    axis.text.y.left = element_text(size = 8, color = "darkblue"),
    axis.text.y.right = element_text(size = 8, color = "darkgreen"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.grid = element_blank()
  )

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/combined_hypothesis2.png", width = 8, height=6)

#######################################################################################################
# Adjust code #
# Determine the maximum values
max_slope <- 0.3  # Desired maximum for recovery rate y-axis
max_crash <- max(model_data_recover$crash, na.rm = TRUE)

# Calculate scaling factor
scale_factor <- max_slope / max_crash

# Apply scaling to crash data and predictions
model_data_recover <- model_data_recover %>%
  mutate(scaled_crash = crash * scale_factor)

combined_prediction <- combined_prediction %>%
  mutate(scaled_predicted_crash = predicted_crash * scale_factor)

# Modified plot with rescaled crash rate
ggplot() +
  # Recovery Rate (Left Y-Axis)
  geom_point(data = model_data_ungroup, aes(x = mean_temp, y = slope), alpha = 0.5, color = "blue") +
  geom_line(data = combined_prediction, aes(x = mean_temp, y = predicted_slope), color = "darkblue", linewidth = 1) +
  # Scaled Crash Rate (Left Y-Axis)
  geom_point(data = model_data_recover, aes(x = mean_temp, y = scaled_crash), alpha = 0.5, color = "darkgreen") +
  geom_line(data = combined_prediction, aes(x = mean_temp, y = scaled_predicted_crash), color = "green", linewidth = 1) +
  # Set Left Y-Axis Limits
  scale_y_continuous(
    name = "Recovery Rate (Slope) / Scaled Crash Rate",
    limits = c(0, 0.3)
  ) +
  labs(
    title = "Recovery and Crash Rates in Relation to Mean Temperature",
    x = "Mean Temperature (°C)"
  ) +
  # Adjusted Annotations
  annotate("text", x = max(model_data_ungroup$mean_temp) - 0.5, y = 0.28, 
           label = paste("Recovery Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 1, vjust = 1, size = 4, color = "blue") +
  annotate("text", x = max(model_data_ungroup$mean_temp) - 0.5, y = 0.25, 
           label = paste("Crash Bayesian R² =", round(b2_crash, 4)), 
           hjust = 1, vjust = 1, size = 4, color = "darkgreen") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "none",  # Hide legend if not needed
    panel.grid = element_blank()
  )

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/combined_hypothesis2_adjusted.png", width = 8, height=6)

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
# Fit your custom model
model1 <- lm(props ~ mean_temp + I(mean_temp^2), data = im)
adj_r_squared1 <- summary(model1)$adj.r.squared
adj_r_squared_text1 <- paste0("Adjusted R² = ", round(adj_r_squared1, 4))
summary(model1)
# Create the plot using the custom model
ggplot(data = im, aes(x = mean_temp, y = props)) +
  geom_point(aes(color = ifelse(props > 0, "positive", "negative")), alpha = 0.7) +  # Color points by positive/negative values
  # Add a line using the custom model fitted values
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "black", size = 1.2) +
  labs(
    title = "Proportionally more bats in cold sites after WNS",
    x = "Mean Temperature", 
    y = "Proportion of bats (After WNS - Before WNS)", 
    size = "Mean Count\n(After WNS)",
    color = "Proportions\n(After WNS -\nBefore WNS)"  # Legend title for color
  ) + 
  annotate("text", x = Inf, y = Inf, label = adj_r_squared_text1, 
           hjust = 1.1, vjust = 1.5, size = 5, color = "black") +
  theme_bw() +  # Base theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),  # Centered and bold title
    axis.title = element_text(size = 12),  # Larger axis titles
    legend.title = element_text(size = 10),  # Larger legend title
    legend.text = element_text(size = 8),  # Larger legend text
    panel.grid = element_blank()  # Remove grid lines
  ) +
  scale_color_manual(values = c("positive" = "blue", "negative" = "red"))  # Custom color scale

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/proportion_bats_important.png", width = 8, height = 6)
