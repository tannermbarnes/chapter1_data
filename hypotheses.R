library(tidyverse)
library(readxl)
library(brms)
library(rstan)
library(tidyr)
library(purrr)
library(broom)
library(lmtest)
library(grid)  # For unit()
library(extrafont)
library(bayesplot)

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

# Hypothesis 1 ######################################################################################################################3
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

# Hypothesis 1: Bayesian
# Define the prior
prior <- c(
  prior(normal(0, 1), class = "b", coef = "crash"),
  prior(normal(0,1), class = "Intercept"))

hypothesis1 <- brm(
  formula = slope ~ crash,
  data = model_data,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

hypothesis1_null <- brm(
  formula = slope ~ 1,
  data = model_data,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)



summary(hypothesis1)
bayesian_r2 <- bayes_R2(hypothesis1)
print(bayesian_r2)

model_data_ungroup <- model_data %>% ungroup()

predictions1 <- predict(hypothesis1, newdata = model_data_ungroup)
fitted_values1 <- fitted(hypothesis1, newdata = model_data_ungroup)

hypoth1_plot <- ggplot(model_data, aes(x = crash, y = slope)) +
  geom_point(aes(fill = mean_temp), shape = 21, alpha = 0.7, size = 4, stroke = 0.5) +  # Raw data points
  geom_line(aes(x = crash, y = fitted_values1[, "Estimate"]), color = "black", linewidth = 1) +  # Fitted values line
  geom_ribbon(aes(x = crash, ymin = fitted_values1[, "Q2.5"], ymax = fitted_values1[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
  labs(x = expression(Population~Crash~Severity~(1 - frac(minimum~count,maximum~count))), 
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) +
  annotate("text", x = 0.67, y = 0.059, label = paste("Bayesian R² = 0.3730"), 
           hjust = 0, vjust = 0, angle = 337, size = 4, color = "black", fontface = "italic", family = "Times New Roman") +
  theme_bw() +
  theme(
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 14), family = "Times New Roman", size = 14, color = "black"),
    axis.title.y = element_text(margin = margin(r = 14), family = "Times New Roman", size = 14, color = "black"), 
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman", size = 14, color = "black"), 
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman", size = 14, color = "black"), 
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 14, family = "Times New Roman"),      # Legend title font size
    legend.position = c(0.9, 0.85),
    legend.text = element_text(size = 14, family = "Times New Roman"),
    panel.grid = element_blank(),        # Legend text font size
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_crash_severity.png", 
       plot = hypoth1_plot, width = 8, height = 6, dpi = 300)

loo_hypothesis1 <- loo(hypothesis1, moment_match = TRUE)
loo_hypothesis1_null <- loo(hypothesis1_null, moment_match = TRUE)
elpd_hypothesis1 <- loo_hypothesis1$estimates["elpd_loo", "Estimate"]
elpd_hypothesis1_null <- loo_hypothesis1_null$estimates["elpd_loo", "Estimate"]
loo_compare(loo_hypothesis1, loo_hypothesis1_null)
loo_hypothesis1
loo_hypothesis1_null

library(patchwork)
posterior_samples <- as.array(hypothesis1)
# Posterior density plot
posterior_density_plot1 <- mcmc_dens(posterior_samples, pars = c("b_Intercept", "b_crash")) +
  labs(title = "Hypothesis 1: Posterior Distribution of the Model Coefficients")
# Posterior predictive check
pp_check_plot1 <- pp_check(hypothesis1, ndraws = 100)
# Combine the two plots into a single figure
combined_plot1 <- posterior_density_plot1 + pp_check_plot1 + plot_layout(ncol = 2)

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/H1_posterior.png", 
       plot = combined_plot1, width = 8, height = 6, dpi = 300)

# Hypothesis 2 Recovery Rate ######################################################################################################
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

b_r2_slope <- bayesian_r2[1, "Estimate"]
bayesian_r2_ci <- quantile(bayesian_r2, probs = c(0.025, 0.975))

model_data_ungroup <- model_data_recover %>% ungroup()

# Create a grid of mean_temp for predictions
prediction_grid_s <- model_data_ungroup %>%
  select(mean_temp) %>%  # Ensure only relevant columns are selected
  tidyr::expand(mean_temp = seq(min(mean_temp, na.rm = TRUE), max(mean_temp, na.rm = TRUE), length.out = 100)) %>%
  mutate(recovery_years = median(model_data_ungroup$recovery_years, na.rm = TRUE))

fitted_values <- fitted(slope_model, newdata = prediction_grid_s, summary = TRUE)


# Add the predicted values and standard error to the prediction grid
prediction_grid_s <- prediction_grid_s %>%
  mutate(predicted_slope = fitted_values[, "Estimate"],
         se_slope = fitted_values[, "Est.Error"],
         upper = fitted_values[, "Q97.5"],  # 95% upper bound
         lower = fitted_values[, "Q2.5"])  # 95% lower bound

library(viridis)
# Plot the slope model
plot1 <- ggplot(model_data_ungroup, aes(x = mean_temp, y = slope)) + 
  geom_point(aes(fill = mean_temp), shape = 21, stroke = 0.5, size = 4) + 
  geom_line(data = prediction_grid_s, aes(x = mean_temp, y = predicted_slope, color = mean_temp), linewidth = 1) +
  labs(x = "Mean Temperature (\u00B0C)",
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) +
  annotate("text", x = 6.5, y = 0.035, label = paste("Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 1, vjust = 0, size = 4, angle = 328, color = "black", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.length = unit(-0.1, "inches"),  # Move tick marks inside
    axis.ticks = element_line(),  # Ensure tick marks are visible
    axis.ticks.x = element_line(),  # Add tick marks to the bottom axis
    axis.ticks.y = element_line(),  # Add tick marks to the left axis
    axis.text.x = element_text(margin = margin(t = 10), family = "Times New Roman", size = 14, color = "black"),  # Use Times New Roman for axis text
    axis.text.y = element_text(margin = margin(r = 10), family = "Times New Roman", size = 14, color = "black"),  # Use Times New Roman for axis text
    axis.line = element_line(color = "black"),  # Ensure axis lines are visible
    axis.title.x = element_text(margin = margin(t = 10), family = "Times New Roman", size = 14, color = "black"), # Axis title font
    axis.title.y = element_text(margin = margin(r = 10), family = "Times New Roman", size = 14, color = "black"),  # Axis title font
    axis.text = element_text(size = 12, family = "Times New Roman"),  # Increase axis text size and set font
    axis.title = element_text(size = 12, family = "Times New Roman"),  # Increase axis title size and set font
    legend.position = c(0.9, 0.85), 
    legend.title = element_text(size = 14, family = "Times New Roman"),
    legend.text = element_text(size = 12, family = "Times New Roman"),
    legend.key.size = unit(0.5, "cm")
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the top axis
    limits = c(2, 10)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the right axis
    limits = c(0, 0.12)
  ) + 
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_mean_temp.png", 
       plot = plot1, width = 8, height = 6, dpi = 300)


posterior_samples2 <- as.array(slope_model)
# Posterior density plot
posterior_density_plot2 <- mcmc_dens(posterior_samples2, pars = c("b_Intercept", "b_mean_temp")) +
  labs(title = "Hypothesis 2: Recovery Slope Posterior Distribution of the Model Coefficients")
# Posterior predictive check
pp_check_plot2 <- pp_check(slope_model, ndraws = 100)
# Combine the two plots into a single figure
combined_plot2 <- posterior_density_plot2 + pp_check_plot2 + plot_layout(ncol = 2)

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/H2_slope_posterior.png", 
       plot = combined_plot2, width = 8, height = 6, dpi = 300)

# Hypothesis 2 Crash Rate ##################################################################################################
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
prediction_grid2 <- tibble(mean_temp = seq(min(model_data_recover$mean_temp), max(model_data_recover$mean_temp), length.out = 100))

# Step 2: Generate predictions using the fitted values from the linear model
predicted_slope_values1 <- fitted(crash_model_linear, newdata = prediction_grid2, summary = TRUE)[, "Estimate"]

# Step 3: Add the predicted values to the dataset
prediction_grid2 <- prediction_grid2 %>%
  mutate(predicted_crash = predicted_slope_values1)

# Step 4: Plot with a straight line for the linear model
plot2 <- ggplot(model_data_recover, aes(x = mean_temp, y = crash)) +
  geom_point(aes(fill = mean_temp), shape = 21, color = "black", stroke = 0.5, size = 4) +
  geom_line(data = prediction_grid2, aes(x=mean_temp, y = predicted_crash, color = mean_temp), linewidth = 1) +
  #geom_ribbon(aes(x = mean_temp, ymin = predicted_slope_values1[, "Q2.5"], ymax = predicted_slope_values1[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
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
    legend.position = c(0.9, 0.15), 
    legend.text = element_text(size = 14, family = "Times New Roman"),
    legend.key.size = unit(0.5, "cm"),
    panel.grid = element_blank()        # Legend text font size
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/crash_rate_mean_temp.png", 
       plot = plot2, width = 8, height = 6, dpi = 300)


posterior_samples3 <- as.array(crash_model_linear)
# Posterior density plot
posterior_density_plot3 <- mcmc_dens(posterior_samples3, pars = c("b_Intercept", "b_mean_temp")) +
  labs(title = "Hypothesis 2: Crash Severity Posterior Distribution of the Model Coefficients")
# Posterior predictive check
pp_check_plot3 <- pp_check(slope_model, ndraws = 100)
# Combine the two plots into a single figure
combined_plot3 <- posterior_density_plot3 + pp_check_plot3 + plot_layout(ncol = 2)

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/H2_crash_posterior.png", 
       plot = combined_plot3, width = 8, height = 6, dpi = 300)


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


# Hypothesis 3 ###########################################################################################################
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


# Hypothesis 3: Bayesian #########################################################################################################
hypothesis3_simple <- brm(
  formula = props ~ mean_temp,
  data = im10,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

hypothesis3_quadratic <- brm(
  formula = props ~ mean_temp + I(mean_temp^2),
  data = im10,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0, 10), class = "Intercept")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

hypothesis3_null <- brm(
  formula = props ~ 1,
  data = im10,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "Intercept")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

loo(hypothesis3_simple, moment_match = TRUE)
loo(hypothesis3_quadratic, moment_match = TRUE)
loo(hypothesis3_null, moment_match = TRUE)

summary(hypothesis3_simple)
predictions <- predict(hypothesis3_simple, newdata = im10)
fitted_values <- fitted(hypothesis3_simple, newdata = im10)

bayesian_r2.3 <- bayes_R2(hypothesis3_simple)
print(bayesian_r2.3)

hypoth3 <- ggplot(im10, aes(x = mean_temp, y = props)) +
  geom_point(aes(fill = mean_temp), shape = 21, alpha = 0.7, stroke = 0.5, size = 4) +  # Raw data points
  geom_line(aes(x = mean_temp, y = fitted_values[, "Estimate"], color = mean_temp), linewidth = 1) +  # Fitted values line
  geom_ribbon(aes(x = mean_temp, ymin = fitted_values[, "Q2.5"], ymax = fitted_values[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
  labs(x = "Mean Temperature (\u00B0C)", y = "Proportion of Bats (After WNS - Before WNS)") +
    annotate("text", x = 4.75, y = 0.09, label = "Bayesian R² = 0.6536", 
           hjust = 0, vjust = 0, angle = 338, size = 4, color = "black", family = "Times New Roman") +
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
    legend.title = element_text(size = 14, family = "Times New Roman"),      # Legend title font size
    legend.text = element_text(size = 14, family = "Times New Roman"),
    panel.grid = element_blank(),        # Legend text font size
    legend.position = c(0.9, 0.80),
    legend.key.size = unit(0.5, "cm")
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/proportion_bats_important.png",
       plot = hypoth3, width = 8, height = 6, dpi = 300)


posterior_samples4 <- as.array(hypothesis3_simple)
# Posterior density plot
posterior_density_plot4 <- mcmc_dens(posterior_samples4, pars = c("b_Intercept", "b_mean_temp")) +
  labs(title = "Hypothesis 3: Posterior Distribution of the Model Coefficients")
# Posterior predictive check
pp_check_plot4 <- pp_check(slope_model, ndraws = 100)
# Combine the two plots into a single figure
combined_plot4 <- posterior_density_plot4 + pp_check_plot4 + plot_layout(ncol = 2)

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/H3_proportion_posterior.png", 
       plot = combined_plot4, width = 8, height = 6, dpi = 300)


# COMBINING 4 PLOTS INTO ONE A 4 PANEL GRID
hypoth1_plot
plot1
plot2
hypoth3

# Load necessary libraries
library(ggplot2)
library(patchwork)
library(grid)
library(jpeg)

# Load the background image
img <- readJPEG("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/background_little_brown_bat.jpg")
g <- rasterGrob(img, interpolate = TRUE)  # Convert the image to a grob (graphical object)

# Create the combined 2x2 grid of plots with the image in the background
combined_plot <- hypoth1_plot + plot1 + plot2 + hypoth3 +
  plot_layout(ncol = 2) + 
  plot_annotation(
    theme = theme(
      plot.background = element_blank(),  # Ensure no additional background color
      plot.margin = margin(0, 0, 0, 0)   # No extra margin around the plots
    )
  ) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)  # Place the image as a background

# Display the combined plot
print(combined_plot)

# Optionally save the combined plot to a file
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/test.png", 
combined_plot, width = 12, height = 12, dpi = 300)



#####################################################################################################################################
# New visualizations

h1 <- ggplot(model_data, aes(x = crash, y = slope)) +
  geom_point(aes(fill = mean_temp), shape = 21, alpha = 0.7, size = 4, stroke = 0.5) +  # Raw data points
  geom_line(aes(x = crash, y = fitted_values1[, "Estimate"]), color = "white", linewidth = 1) +  # Fitted values line
  geom_ribbon(aes(x = crash, ymin = fitted_values1[, "Q2.5"], ymax = fitted_values1[, "Q97.5"]), fill = "lightgray", alpha = 0.2) +  # CI ribbon
  labs(x = expression(Population~Crash~Severity~(1 - frac(minimum~count,maximum~count))), 
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) +
  annotate("text", x = 0.6, y = 0.071, label = paste("Bayesian R² = 0.3730"), 
           hjust = 0, vjust = 0, angle = 330, size = 4, color = "white", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "black"), 
    plot.background = element_rect(fill = "black"),
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(color = "white"),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.y = element_line(color = "white"),
    axis.title.x = element_text(margin = margin(t = 14), family = "Times New Roman", size = 14, color = "white"),
    axis.title.y = element_text(margin = margin(r = 14), family = "Times New Roman", size = 14, color = "white"), 
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman", size = 14, color = "white"), 
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman", size = 14, color = "white"), 
    axis.line = element_line(color = "white"),
    legend.title = element_text(color = "white", size = 14, family = "Times New Roman"),      # Legend title font size
    legend.position = c(0.85, 0.84),
    legend.text = element_text(color = "white", size = 14, family = "Times New Roman"),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "black", color= "black"),
    legend.key = element_rect(fill = "black", color = "black")
  ) +
  #scale_x_continuous(
  #  sec.axis = sec_axis(~ ., labels = NULL)
  #) +
  #scale_y_continuous(
  #  sec.axis = sec_axis(~ ., labels = NULL)
  #) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))


# Plot the slope model
h2a <- ggplot(model_data_ungroup, aes(x = mean_temp, y = slope)) + 
  geom_point(aes(fill = mean_temp), shape = 21, stroke = 0.5, size = 4) + 
  geom_line(data = prediction_grid_s, aes(x = mean_temp, y = predicted_slope, color = mean_temp), linewidth = 1) +
  labs(x = "Mean Temperature (\u00B0C)",
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) +
  annotate("text", x = 5.8, y = 0.04, label = paste("Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 1, vjust = 0, size = 4, angle = 315, color = "white", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.ticks.length = unit(-0.1, "inches"),  # Move tick marks inside
    axis.ticks = element_line(color = "white"),  # Ensure tick marks are visible
    axis.ticks.x = element_line(),  # Add tick marks to the bottom axis
    axis.ticks.y = element_line(),  # Add tick marks to the left axis
    axis.text.x = element_text(margin = margin(t = 10), family = "Times New Roman", size = 14, color = "white"),  # Use Times New Roman for axis text
    axis.text.y = element_text(margin = margin(r = 10), family = "Times New Roman", size = 14, color = "white"),  # Use Times New Roman for axis text
    axis.line = element_line(color = "white"),  # Ensure axis lines are visible
    axis.title.x = element_text(margin = margin(t = 10), family = "Times New Roman", size = 14, color = "white"), # Axis title font
    axis.title.y = element_text(margin = margin(r = 10), family = "Times New Roman", size = 14, color = "white"),  # Axis title font
    axis.text = element_text(size = 12, family = "Times New Roman"),  # Increase axis text size and set font
    axis.title = element_text(size = 12, family = "Times New Roman"),  # Increase axis title size and set font
    legend.position = c(0.85, 0.84), 
    legend.background = element_rect(fill = "black", color = "black"),
    legend.key = element_rect(fill = "black", color = "black"),
    legend.title = element_text(size = 14, family = "Times New Roman", color = "white"),
    legend.text = element_text(size = 12, family = "Times New Roman", color = "white"),
    legend.key.size = unit(0.5, "cm")
  ) +
  # scale_x_continuous(
  #   sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the top axis
  #   limits = c(2, 10)
  # ) +
  # scale_y_continuous(
  #   sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the right axis
  #   limits = c(0, 0.12)
  # ) + 
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))


# Step 4: Plot with a straight line for the linear model
h2b <- ggplot(model_data_recover, aes(x = mean_temp, y = crash)) +
  geom_point(aes(fill = mean_temp), shape = 21, color = "black", stroke = 0.5, size = 4) +
  geom_line(data = prediction_grid2, aes(x=mean_temp, y = predicted_crash, color = mean_temp), linewidth = 1) +
  #geom_ribbon(aes(x = mean_temp, ymin = predicted_slope_values1[, "Q2.5"], ymax = predicted_slope_values1[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
  scale_size_continuous(name = "Mean\nPopulation\nSize") +
  labs(x = "Mean Temperature (\u00B0C)",
       y = expression(Population~Crash~Severity~(1 - frac(minimum~count,maximum~count)))) +
annotate("text", x = 5.7, 
         y = 0.885, 
         label = paste("Bayesian R² =", round(b2_crash, 4)), angle = 21,
         hjust = 1, vjust = 1.5, size = 4, color = "white", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(color = "white"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 12), family = "Times New Roman", color = "white", size = 14),
    axis.title.y = element_text(margin = margin(r = 12), family = "Times New Roman", color = "white", size = 14), 
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman", color = "white", size = 14), 
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman", color = "white", size = 14), 
    axis.line = element_line(color = "white"),
    legend.title = element_text(size = 12, family = "Times New Roman", color = "white"),      # Legend title font size
    legend.position = c(0.84, 0.16), 
    legend.text = element_text(size = 14, family = "Times New Roman", color = "white"),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_rect(fill = "black", color = "black"),
    legend.key = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank()        # Legend text font size
  ) +
  # scale_x_continuous(
  #   sec.axis = sec_axis(~ ., labels = NULL)
  # ) +
  # scale_y_continuous(
  #   sec.axis = sec_axis(~ ., labels = NULL)
  # ) +
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))


h3 <- ggplot(im10, aes(x = mean_temp, y = props)) +
  geom_point(aes(fill = mean_temp), shape = 21, alpha = 0.7, stroke = 0.5, size = 4) +  # Raw data points
  geom_line(aes(x = mean_temp, y = fitted_values[, "Estimate"], color = mean_temp), linewidth = 1) +  # Fitted values line
  geom_ribbon(aes(x = mean_temp, ymin = fitted_values[, "Q2.5"], ymax = fitted_values[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
  labs(x = "Mean Temperature (\u00B0C)", y = "Proportion of Bats (After WNS - Before WNS)") +
    annotate("text", x = 4.65, y = 0.09, label = "Bayesian R² = 0.1819", 
           hjust = 0, vjust = 0, angle = 332, size = 4, color = "white", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(color = "white"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 12), family = "Times New Roman", color = "white", size = 14),
    axis.title.y = element_text(margin = margin(r = 12), family = "Times New Roman", color = "white", size = 14), 
    axis.text.x = element_text(margin = margin(t = 12), family = "Times New Roman", color = "white", size = 14), 
    axis.text.y = element_text(margin = margin(r = 12), family = "Times New Roman", color = "white", size = 14), 
    axis.line = element_line(color = "white"),
    legend.title = element_text(size = 14, family = "Times New Roman", color = "white"),      # Legend title font size
    legend.text = element_text(size = 14, family = "Times New Roman", color = "white"),
    panel.grid = element_blank(),        # Legend text font size
    legend.position = c(0.85, 0.80),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_rect(fill = "black", color = "black"),
    legend.key = element_rect(fill = "black", color = "black")
  ) +
  # scale_x_continuous(
  #   sec.axis = sec_axis(~ ., labels = NULL)
  # ) +
  # scale_y_continuous(
  #   sec.axis = sec_axis(~ ., labels = NULL)
  # ) +
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))

# Load the background image
img <- readPNG("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/background_LBB.png")
g <- rasterGrob(img, interpolate = TRUE)  # Convert the image to a grob (graphical object)

library(patchwork)

combined_plot <- h1 + h2a + h2b + h3 +
  plot_layout(ncol = 2) +  # Arrange plots in two columns
  plot_annotation(
    theme = theme(
      plot.background = element_blank(),  # Remove background color from plot
      plot.margin = margin(0, 0, 0, 0),   # No extra margin around the plots
      panel.spacing = unit(0, "cm"),      # Remove space between plots
      panel.grid.major = element_blank(), # Remove gridlines
      panel.grid.minor = element_blank(), # Remove minor gridlines
      axis.ticks = element_line(color = "white"),  # Adjust the axis tick color
      axis.line = element_line(color = "white")    # Adjust axis line color
    )
  )

  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)  # Place the image as a background

# Display the combined plot
print(combined_plot)

# Optionally save the combined plot to a file
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/test.png", 
plot = combined_plot, width = 12, height = 12, dpi = 300)


####################################################################################################################
# Switch Black and White
h1 <- ggplot(model_data, aes(x = crash, y = slope)) +
  geom_point(aes(fill = mean_temp), shape = 21, alpha = 0.7, size = 4, stroke = 0.5) +  # Raw data points
  geom_line(aes(x = crash, y = fitted_values1[, "Estimate"]), color = "black", linewidth = 1) +  # Fitted values line
  geom_ribbon(aes(x = crash, ymin = fitted_values1[, "Q2.5"], ymax = fitted_values1[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
  labs(x = expression(Population~Crash~Severity~(1 - frac(minimum~count,maximum~count))), 
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) +
  annotate("text", x = 0.6, y = 0.071, label = paste("Bayesian R² = 0.3730"), 
           hjust = 0, vjust = 0, angle = 330, size = 4, color = "black", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(margin = margin(t = 8), family = "Times New Roman", size = 14, color = "black"),
    axis.title.y = element_text(margin = margin(r = 8), family = "Times New Roman", size = 14, color = "black"), 
    axis.text.x = element_text(margin = margin(t = 8), family = "Times New Roman", size = 14, color = "black"), 
    axis.text.y = element_text(margin = margin(r = 8), family = "Times New Roman", size = 14, color = "black"), 
    axis.line = element_line(color = "black"),
    legend.title = element_text(color = "black", size = 14, family = "Times New Roman"),      # Legend title font size
    legend.position = c(0.85, 0.84),
    legend.text = element_text(color = "black", size = 14, family = "Times New Roman"),
    panel.grid = element_blank(),
    legend.background = element_rect(fill = "white", color= "white"),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  scale_x_continuous(
   sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
   sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))


# Plot the slope model
h2a <- ggplot(model_data_ungroup, aes(x = mean_temp, y = slope)) + 
  geom_point(aes(fill = mean_temp), shape = 21, stroke = 0.5, size = 4) + 
  geom_line(data = prediction_grid_s, aes(x = mean_temp, y = predicted_slope, color = mean_temp), linewidth = 1) +
  labs(x = "Mean Temperature (\u00B0C)",
       y = expression(paste("Recovery Rate (", beta, " Estimate)"))) +
  annotate("text", x = 5.7, y = 0.044, label = paste("Bayesian R² =", round(b_r2_slope, 4)), 
           hjust = 1, vjust = 0, size = 4, angle = 312, color = "black", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.ticks.length = unit(-0.1, "inches"),  # Move tick marks inside
    axis.ticks = element_line(color = "black"),  # Ensure tick marks are visible
    axis.ticks.x = element_line(),  # Add tick marks to the bottom axis
    axis.ticks.y = element_line(),  # Add tick marks to the left axis
    axis.text.x = element_text(margin = margin(t = 8), family = "Times New Roman", size = 14, color = "black"),  # Use Times New Roman for axis text
    axis.text.y = element_text(margin = margin(r = 8), family = "Times New Roman", size = 14, color = "black"),  # Use Times New Roman for axis text
    axis.line = element_line(color = "black"),  # Ensure axis lines are visible
    axis.title.x = element_text(margin = margin(t = 8), family = "Times New Roman", size = 14, color = "black"), # Axis title font
    axis.title.y = element_text(margin = margin(r = 8), family = "Times New Roman", size = 14, color = "black"),  # Axis title font
    axis.text = element_text(size = 12, family = "Times New Roman"),  # Increase axis text size and set font
    axis.title = element_text(size = 12, family = "Times New Roman"),  # Increase axis title size and set font
    legend.position = c(0.85, 0.84), 
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key = element_rect(fill = "white", color = "white"),
    legend.title = element_text(size = 14, family = "Times New Roman", color = "black"),
    legend.text = element_text(size = 14, family = "Times New Roman", color = "black"),
    legend.key.size = unit(0.5, "cm")
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the top axis
    limits = c(2, 10)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL),  # Remove numbers on the right axis
    limits = c(0, 0.12)
  ) + 
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))


# Step 4: Plot with a straight line for the linear model
h2b <- ggplot(model_data_recover, aes(x = mean_temp, y = crash)) +
  geom_point(aes(fill = mean_temp), shape = 21, color = "black", stroke = 0.5, size = 4) +
  geom_line(data = prediction_grid2, aes(x=mean_temp, y = predicted_crash, color = mean_temp), linewidth = 1) +
  #geom_ribbon(aes(x = mean_temp, ymin = predicted_slope_values1[, "Q2.5"], ymax = predicted_slope_values1[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
  scale_size_continuous(name = "Mean\nPopulation\nSize") +
  labs(x = "Mean Temperature (\u00B0C)",
       y = expression(Population~Crash~Severity~(1 - frac(minimum~count,maximum~count)))) +
annotate("text", x = 5.7, 
         y = 0.885, 
         label = paste("Bayesian R² =", round(b2_crash, 4)), angle = 21,
         hjust = 1, vjust = 1.5, size = 4, color = "black", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 8), family = "Times New Roman", color = "black", size = 14),
    axis.title.y = element_text(margin = margin(r = 8), family = "Times New Roman", color = "black", size = 14), 
    axis.text.x = element_text(margin = margin(t = 8), family = "Times New Roman", color = "black", size = 14), 
    axis.text.y = element_text(margin = margin(r = 8), family = "Times New Roman", color = "black", size = 14), 
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 14, family = "Times New Roman", color = "black"),      # Legend title font size
    legend.position = c(0.84, 0.16), 
    legend.text = element_text(size = 14, family = "Times New Roman", color = "black"),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank()        # Legend text font size
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))


h3 <- ggplot(im10, aes(x = mean_temp, y = props)) +
  geom_point(aes(fill = mean_temp), shape = 21, alpha = 0.7, stroke = 0.5, size = 4) +  # Raw data points
  geom_line(aes(x = mean_temp, y = fitted_values[, "Estimate"], color = mean_temp), linewidth = 1) +  # Fitted values line
  geom_ribbon(aes(x = mean_temp, ymin = fitted_values[, "Q2.5"], ymax = fitted_values[, "Q97.5"]), fill = "gray", alpha = 0.2) +  # CI ribbon
  labs(x = "Mean Temperature (\u00B0C)", y = "Proportion of Bats (After WNS - Before WNS)") +
    annotate("text", x = 4.75, y = 0.0745, label = "Bayesian R² = 0.3325", 
           hjust = 0, vjust = 0, angle = 332, size = 4, color = "black", fontface = "bold", family = "Times New Roman") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.ticks.length = unit(-0.1, "inches"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 8), family = "Times New Roman", color = "black", size = 14),
    axis.title.y = element_text(margin = margin(r = 8), family = "Times New Roman", color = "black", size = 14), 
    axis.text.x = element_text(margin = margin(t = 8), family = "Times New Roman", color = "black", size = 14), 
    axis.text.y = element_text(margin = margin(r = 8), family = "Times New Roman", color = "black", size = 14), 
    axis.line = element_line(color = "black"),
    legend.title = element_text(size = 14, family = "Times New Roman", color = "black"),      # Legend title font size
    legend.text = element_text(size = 14, family = "Times New Roman", color = "black"),
    panel.grid = element_blank(),        # Legend text font size
    legend.position = c(0.85, 0.8),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  scale_x_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., labels = NULL)
  ) +
  scale_color_gradientn(colors = colors) +
  scale_fill_gradientn(colors = colors) +
  guides(fill = guide_legend(title = "Mean\nTemperature"), color = guide_legend(title = "Mean\nTemperature"))

combined_plot <- h1 + h2b + h2a + h3 +
  plot_layout(ncol = 2) +  # Arrange plots in two columns
  plot_annotation(
    theme = theme(
      plot.background = element_blank(),  # Remove background color from plot
      plot.margin = margin(0, 0, 0, 0),   # No extra margin around the plots
      panel.spacing = unit(0, "cm"),      # Remove space between plots
      panel.grid.major = element_blank(), # Remove gridlines
      panel.grid.minor = element_blank(), # Remove minor gridlines
      axis.ticks = element_line(color = "white"),  # Adjust the axis tick color
      axis.line = element_line(color = "white")    # Adjust axis line color
    )
  )

# Optionally save the combined plot to a file
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/combined_plot.png", 
plot = combined_plot, width = 12, height = 12, dpi = 300)

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/proportion_bats_01.png", 
plot = h3, width = 12, height = 12, dpi = 300)
