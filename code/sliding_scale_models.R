# THIS CODE IS MODELS FOR SLOPE OF RECOVERY AND POPULATION CRASH IT PRODUCEs ESTIMATES AND FIGURES
rm(list = ls())
setwd("E:/chapter1_data/code")
library(lavaan)
library(lme4)
library(car)
library(lmtest)
library(caTools)
library(nlme)
library(corrplot)
library(ggplot2)
##################################### Models for mines on the sliding scale method ###################################
source("sliding_scale.R")

# remove unimportant sites from population crash they could fluctuate for other reasons
model_df_crash <- model_df %>% filter(mean_count > 300) %>% 
mutate(new = ifelse(slope > 0, "recovering", "not recovering"),
percent_crash = crash_mean * 100)

###############################################################################################################
############ Hypothesis 1 crash_intensity and slope are related ###############################################
###############################################################################################################

# Fit the linear model
m1 <- lm(slope ~ crash_mean, model_df_recover)
summary(m1)

# Extract adjusted R-squared value
adj_r_squared <- summary(m1)$adj.r.squared
adj_r_squared_text <- paste0("Adjusted R² = ", round(adj_r_squared, 3))

# Define colors for the plot
colors <- c("blue", "white", "red")

# Create the plot with dynamic R-squared annotation
ggplot(model_df_recover, aes(x = percent_crash, y = slope)) + 
  geom_point(aes(fill = mean_temp, size = last_count), shape = 21, color = "black", stroke = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
  scale_size_continuous(name = "Last Survey\nPopulation Count") + 
  labs(
    title = "The recovery rate and the population crash are highly related",
    x = "Population Crash (%)",
    y = "Recovery Rate (Slope)"
  ) + 
  theme_bw() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
  ) +
  annotate("text", x = Inf, y = Inf, label = adj_r_squared_text, 
           hjust = 1.1, vjust = 1.5, size = 3, color = "black", fontface = "italic")

ggsave("E:/chapter1_data/figures/final/recovery_rate_population_crash.png", width = 6, height=4)

ggplot(model_df_recover, aes(x=recovery_years, y=slope)) + 
geom_point(aes(fill = mean_temp, size = last_count), shape = 21, color = "black", stroke = 0.5) + 
geom_smooth(method = "lm", se = FALSE, color = "black") +
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
scale_size_continuous(name = "Last Population\nCount") + 
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8))

###################################################################################################################
#################### First plot Frequentist #######################################################################
###################################################################################################################
#########################################################################################################################
#######################################################################################################################
null_f <- glm(slope ~ 1 + offset(recovery_years), family = gaussian, weights = last_count, data = model_df_recover)
summary(null_f)

slope_1 <- glm(slope ~ mean_temp + offset(recovery_years), family = gaussian, weights = last_count, data = model_df_recover)
summary(slope_1)

slope_2 <- glm(slope ~ mean_temp + log_passage + offset(recovery_years), family = gaussian, weights = last_count, data = model_df_recover)
summary(slope_2)

slope_3 <- glm(slope ~ mean_temp + log_passage + standing_water + offset(recovery_years), family = gaussian, weights = last_count, data = model_df_recover)
summary(slope_3)

crash_null <- glm(crash_mean ~ 1, family = gaussian, data = model_df_crash)
summary(crash_null)

crash_1 <- glm(crash_mean ~ mean_temp, family = gaussian, data = model_df_crash)
summary(crash_1)

crash_2 <- glm(crash_mean ~ mean_temp + log_passage, family = gaussian, data = model_df_crash)
summary(crash_2)

crash_3 <- glm(crash_mean ~ mean_temp + log_passage + standing_water, family = gaussian, data = model_df_crash)
summary(crash_3)

################################# BAYESIAN MODELING ################################################################
####################################################################################################################
####################################################################################################################
library(brms)
library(rstan)
Sys.setenv(PATH = paste("E:/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "E:/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))
# Define the prior
prior <- c(
  prior(normal(0, 1), class = "b", coef = "mean_temp"),
  prior(normal(0,1), class = "Intercept"))

prior1 <- prior(normal(0,1), class = "Intercept")


# Fit the Bayesian model
null_model_slope <- brm(
  formula = slope_weighted ~ 1,  # Only the intercept
  data = model_df_recover,
  family = gaussian(),
  prior = prior1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model <- brm(
  formula = slope_weighted ~ mean_temp + I(mean_temp^2) + offset(recovery_years),
  data = model_df_recover,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
summary(slope_model)

slope_model1 <- brm(
  formula = slope_weighted ~ poly(mean_temp, 2) + offset(recovery_years),
  data = model_df_recover,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
summary(slope_model1)

slope_model2 <- brm(
  formula = slope_weighted ~ mean_temp + log_passage + offset(recovery_years),
  data = model_df_recover,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model3 <- brm(
  formula = slope_weighted ~ mean_temp + log_passage + standing_water + offset(recovery_years),
  data = model_df_recover,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

############################################################################################
##################### Model Comparison ##################################################
# LOO-CV tends to be more stable, especially in small datasets or models with influencial observations

loo_null_slope <- loo(null_model_slope, moment_match = TRUE)
loo_slope_model <- loo(slope_model, moment_match = TRUE)
loo_slope_model1 <- loo(slope_model1, moment_match = TRUE)
#loo_slope_model2 <- loo(slope_model2, moment_match = TRUE)
loo_compare(loo_null_slope, loo_slope_model, loo_slope_model1)

# Extract ELPD values from each model
elpd_null_slope <- loo_null_slope$estimates["elpd_loo", "Estimate"]
elpd_slope_model <- loo_slope_model$estimates["elpd_loo", "Estimate"]
elpd_slope_model1 <- loo_slope_model1$estimates["elpd_loo", "Estimate"]
elpd_slope_model2 <- loo_slope_model2$estimates["elpd_loo", "Estimate"]
# Get the comparison results (elpd_diff and se_diff)
loo_comparison <- loo_compare(loo_null_slope, loo_slope_model, loo_slope_model1, loo_slope_model2)
# Create a summary table with ELPD, elpd_diff, and se_diff
comparison_table <- data.frame(
  Model = c("slope_model", "slope_model1", "slope_model2", "null_model_slope"),
  ELPD = c(elpd_slope_model, elpd_slope_model1, elpd_slope_model2, elpd_null_slope),
  elpd_diff = loo_comparison[, "elpd_diff"],
  se_diff = loo_comparison[, "se_diff"]
)
# Print the table
print(comparison_table)


################### VISUALIZE ######################################################################

colors <- c("blue", "white", "red")
bayesian_r2 <- bayes_R2(slope_model)
print(bayesian_r2)
b_r2_slope <- bayesian_r2[1, "Estimate"]
bayesian_r2_ci <- quantile(bayesian_r2, probs = c(0.025, 0.975))

# Create a finer grid of mean temp for smoother prediction lines
prediction_grid_s <- model_df_recover %>% 
 tidyr::expand(mean_temp = seq(min(mean_temp), max(mean_temp), length.out = 100))
# left_join(model_df_recover %>% select(mean_temp, recovery_years), by = "mean_temp") 

# Use the median recovery_years for prediction (or can use a specific value if preferred)
median_recovery_years <- median(model_df_recover$recovery_years)
prediction_grid_s <- prediction_grid_s %>% 
mutate(recovery_years = median_recovery_years)

# Generate predictions using the fitted values from the model
predicted_slope_values <- fitted(slope_model, newdata = prediction_grid_s, summary = TRUE)[, "Estimate"]
# Add the predicted values to the dataset
prediction_grid_s <- prediction_grid_s %>%
  mutate(predicted_slope = predicted_slope_values)
# Plot the slope model
ggplot(model_df_recover, aes(x = mean_temp, y = slope_weighted)) +
  geom_point(aes(size = last_count), alpha = 0.5) +  # Plot observed data
  geom_line(data=prediction_grid_s, aes(mean_temp, y = predicted_slope), color = "darkblue", linewidth = 1) +
  scale_size_continuous(name = "Last Survey\nPopulation Count") + 
  labs(title = "Mines with colder mean temperatures have higher recovery rates for Myotis bats",
       x = "Mean Temperature (C)",
       y = "Weighted Recovery Rate (Slope)") +
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
ggsave("E:/chapter1_data/figures/final/recovery_rate_mean_temp.png", width = 8, height=6)


################# Posterior predictive checks (PPCs) ###########################################
slope_dens <- pp_check(slope_model1, type = "dens_overlay")
pp_check(slope_model, type = "hist")
pp_check(slope_model, type = "scatter")
pp_check(slope_model, type = "intervals")

ggsave("E:/chapter1_data/figures/final/slope_dens.png", plot = slope_dens, width = 7, height=5, dpi = 300)

##################################################################################################################
##########################CRASH MODELS #############################################################
####################################################################################################

# Fit the second Bayesian model
null_model_crash <- brm(
  formula = crash_mean ~ 1,
  data = model_df_crash,
  family = gaussian(),
  prior = prior1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

crash_model <- brm(
  formula = crash_mean ~ poly(mean_temp, 2),
  data = model_df_crash,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
summary(crash_model)

crash_model1 <- brm(
  formula = crash_mean ~ mean_temp + log_passage,
  data = model_df_crash,
  family = gaussian(),
  prior = prior,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

crash_model2 <- brm(
  formula = crash_mean ~ mean_temp + log_passage + standing_water,
  data = model_df_crash,
  family = gaussian(),
  prior = prior,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

############################################################################################
##################### Model Comparison ##################################################
# LOO-CV tends to be more stable, especially in small datasets or models with influencial observations
loo_crash_null <- loo(null_model_crash, moment_match = TRUE)
loo_crash_model <- loo(crash_model, moment_match = TRUE)
loo_crash_model1 <- loo(crash_model1, moment_match = TRUE)
loo_crash_model2 <- loo(crash_model2, moment_match = TRUE)
loo_compare(loo_crash_null, loo_crash_model, loo_crash_model1, loo_crash_model2)

# Extract ELPD values from each model
elpd_null_crash <- loo_crash_null$estimates["elpd_loo", "Estimate"]
elpd_crash_model <- loo_crash_model$estimates["elpd_loo", "Estimate"]
elpd_crash_model1 <- loo_crash_model1$estimates["elpd_loo", "Estimate"]
elpd_crash_model2 <- loo_crash_model2$estimates["elpd_loo", "Estimate"]

# Get the comparison results (elpd_diff and se_diff)
loo_comparison1 <- loo_compare(loo_crash_null, loo_crash_model, loo_crash_model1, loo_crash_model2)

# Create a summary table with ELPD, elpd_diff, and se_diff
comparison_table1 <- data.frame(
  Model = c("crash_model", "slope_model1", "slope_model2", "null_model_crash"),
  ELPD = c(elpd_crash_model, elpd_crash_model1, elpd_crash_model2, elpd_null_crash),
  elpd_diff = loo_comparison1[, "elpd_diff"],
  se_diff = loo_comparison1[, "se_diff"]
)

# Print the table
print(comparison_table1)


######################## VISUALIZE ########################################################
bayesian_r2.1 <- bayes_R2(crash_model)
print(bayesian_r2.1)
b2_crash <- bayesian_r2.1[1, "Estimate"]

# Create a finer grid of mean_temp for smoother prediction lines
prediction_grid <- model_df_crash %>% 
tidyr::expand(mean_temp = seq(min(mean_temp), max(mean_temp), length.out = 100))

# Generate predictions using the fitted values from the model
predicted_slope_values1 <- fitted(crash_model, newdata = prediction_grid, summary = TRUE)[, "Estimate"]

# Add the predicted values to the dataset
prediction_grid <- prediction_grid %>%
  mutate(predicted_crash = predicted_slope_values1)

# Plot the crash model
ggplot(model_df_crash, aes(x = mean_temp, y = crash_mean)) +
  geom_point(aes(size = mean_count), alpha = 0.5) +  # Plot observed data
  geom_line(data = prediction_grid, aes(x=mean_temp, y = predicted_crash), color = "darkgreen", linewidth = 1) +
  scale_size_continuous(name = "Mean\nPopulation\nSize") + 
  labs(title = "Mines with colder mean temperatures have lower crash rates for Myotis Bats",
       x = "Mean Temperature (C)",
       y = "Crash Rate (1 - (minimum count / maximum count))") +
  annotate("text", x = Inf, y = Inf, label = paste("Bayesian R² =", round(b2_crash, 4)), 
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
    panel.grid = element_blank()        # Legend text font size
  )
ggsave("E:/chapter1_data/figures/final/crash_rate_mean_temp.png", width = 8, height=6)


################# Posterior predictive checks (PPCs) ###########################################
crash_dens <- pp_check(crash_model, type = "dens_overlay")
pp_check(crash_model, type = "hist")
pp_check(crash_model, type = "scatter")
pp_check(crash_model, type = "intervals")

ggsave("E:/chapter1_data/figures/final/pp_check_crash.png", plot = crash_dens, width = 7, height = 5, dpi = 300)




############################
# Same models using frequentists statistics and the weighted response
slope_weight_model <- glm(slope_weighted ~ mean_temp + offset(recovery_years), family = gaussian, data = model_df_recover)
summary(slope_weight_model)