library(tidyverse)
library(readxl)
library(brms)
library(rstan)
library(tidyr)
library(purrr)
library(broom)
library(lmtest)
library(grid)  # For unit()

Sys.setenv(PATH = paste("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

df <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx", sheet = "model_data")

# groups <- df %>% group_by(site) %>% slice(1)
# head(groups)
# mean(groups$num_measurements)
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
null <- lm(slope ~ 1, model_data)
summary(m1)
AIC(m1)
AIC(null)
anova(null, m1)
colors <- c("blue", "white", "red")
# Visualize with updated theme and annotation position
plot <- ggplot(model_data, aes(x = crash, y = slope)) + 
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
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_population_crash.png", 
       plot = plot, width = 8, height = 6, dpi = 300)

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

# Create a grid of mean_temp for predictions
prediction_grid_s <- model_data_ungroup %>%
  select(mean_temp) %>%  # Ensure only relevant columns are selected
  tidyr::expand(mean_temp = seq(min(mean_temp, na.rm = TRUE), max(mean_temp, na.rm = TRUE), length.out = 100)) %>%
  mutate(recovery_years = median(model_data_ungroup$recovery_years, na.rm = TRUE))

fitted_values <- fitted(slope_model, newdata = prediction_grid_s, summary = TRUE)
# Extract the estimated values and standard errors
predicted_slope_values <- fitted_values[, "Estimate"]
se_values <- fitted_values[, "Est.Error"]

# Add the predicted values and standard error to the prediction grid
prediction_grid_s <- prediction_grid_s %>%
  mutate(predicted_slope = predicted_slope_values,
         se_slope = se_values,
         upper = predicted_slope + 1.96 * se_slope,  # Upper bound of 95% credible interval
         lower = predicted_slope - 1.96 * se_slope)  # Lower bound of 95% credible interval

library(extrafont)

# Plot the slope model
plot1 <- ggplot(model_data_ungroup, aes(x = mean_temp, y = slope)) + 
  geom_point(shape = 20, color = "black", stroke = 0.5) + 
  geom_line(data = prediction_grid_s, aes(x = mean_temp, y = predicted_slope), color = "black", linewidth = 1) +
  labs(x = "Mean Temperature (\u00B0C)",
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) + 
  annotate("text", x = 6, y = 0.04, label = paste("Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 1, vjust = 0, size = 4, angle = 328, color = "black", fontface = "italic", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.length = unit(-0.1, "inches"),  # Move tick marks inside
    axis.ticks = element_line(),  # Ensure tick marks are visible
    axis.ticks.x = element_line(),  # Add tick marks to the bottom axis
    axis.ticks.y = element_line(),  # Add tick marks to the left axis
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman"),  # Use Times New Roman for axis text
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman"),  # Use Times New Roman for axis text
    axis.line = element_line(color = "black"),  # Ensure axis lines are visible
    axis.title.x = element_text(margin = margin(t = 12), family = "Times New Roman"), # Axis title font
    axis.title.y = element_text(margin = margin(r = 12), family = "Times New Roman"),  # Axis title font
    axis.text = element_text(size = 12, family = "Times New Roman"),  # Increase axis text size and set font
    axis.title = element_text(size = 12, family = "Times New Roman")  # Increase axis title size and set font
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the top axis
    limits = c(2, 10)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the right axis
    limits = c(0, 0.12)
  )

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_mean_temp.png", 
       plot = plot1, width = 8, height = 6, dpi = 300)

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
plot2 <- ggplot(model_data_recover, aes(x = mean_temp, y = crash)) +
  geom_point(shape = 20, color = "black", stroke = 0.5) + 
  geom_line(data = prediction_grid, aes(x=mean_temp, y = predicted_crash), color = "black", linewidth = 1) +
  scale_size_continuous(name = "Mean\nPopulation\nSize") + 
  labs(x = "Mean Temperature (\u00B0C)",
       y = expression(Population~Crash~Severity~(1 - frac(minimum~count,maximum~count)))) +
annotate("text", x = 4.5, 
         y = 0.85, 
         label = paste("Bayesian R² =", round(b2_crash, 4)), angle = 17,
         hjust = 1, vjust = 1.5, size = 4, color = "black", fontface = "italic", family = "Times New Roman") +
  theme_bw() +
  theme(
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 12), family = "Times New Roman"),
    axis.title.y = element_text(margin = margin(r = 12), family = "Times New Roman"), 
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman"), 
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman"), 
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 12),      # Legend title font size
    legend.text = element_text(size = 12),
    panel.grid = element_blank()        # Legend text font size
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  )

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/crash_rate_mean_temp.png", 
       plot = plot2, width = 8, height = 6, dpi = 300)

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


im <- mine_proportions

im10 <- mine_proportions %>% filter(proportion_before > 0.05 | proportion_after > 0.05)
# Fit your custom model

model1 <- lm(props ~ mean_temp + I(mean_temp^2), data = im10)
modelx <- lm(props ~ mean_temp, data = im10)
anova(modelx, model1)
AIC(modelx)
AIC(model1)
adj_r_squared1 <- summary(modelx)$adj.r.squared
adj_r_squared_text1 <- paste0("Adjusted R² = ", round(adj_r_squared1, 4))
summary(modelx)
summary(model1)
anova(modelx, model1)
# Create the plot using the custom model
plot3 <- ggplot(data = im10, aes(x = mean_temp, y = props)) +
  geom_point(aes(shape = ifelse(props > 0, "positive", "negative")), alpha = 0.7, size = 3) +
  # Add a line using the custom model fitted values
  stat_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black", linewidth = 1) +
  labs(
    x = "Mean Temperature (\u00B0C)", 
    y = "Proportion of bats (After WNS - Before WNS)", 
    shape = "Proportions\n(After WNS -\nBefore WNS)"
  ) +
  annotate("text", x = 4.75, y = 0.10, label = adj_r_squared_text1, 
           hjust = 0, vjust = 0, angle = 335, size = 4, color = "black", family = "Times New Roman") +
  theme_bw() +
  theme(
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 12), family = "Times New Roman"),
    axis.title.y = element_text(margin = margin(r = 12), family = "Times New Roman"), 
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman"), 
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman"), 
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 12, family = "Times New Roman"),      # Legend title font size
    legend.text = element_text(size = 12, family = "Times New Roman"),
    panel.grid = element_blank(),        # Legend text font size
    legend.position = c(0.9, 0.85)
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_shape_manual(values = c("positive" = 16, "negative" = 1))  # Custom color scale

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/proportion_bats_important.png",
       plot = plot3, width = 8, height = 6, dpi = 300)


