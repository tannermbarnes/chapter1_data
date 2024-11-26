# THIS CODE IS MODELS FOR SLOPE OF RECOVERY AND POPULATION CRASH IT PRODUCEs ESTIMATES AND FIGURES
rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/code")
library(lavaan)
library(lme4)
library(car)
library(lmtest)
library(caTools)
library(nlme)
library(corrplot)
##################################### Models for mines on the sliding scale method ###################################
source("sliding_scale.R")

# remove unimportant sites from population crash they could fluctuate for other reasons
model_df_crash <- model_df %>% filter(mean_count > 89) %>% 
mutate(new = ifelse(slope > 0, "recovering", "not recovering"), 
crash = crash_mean)

###############################################################################################################
############ Hypothesis 1 crash_intensity and slope are related ###############################################
###############################################################################################################
m1 <- lm(slope ~ crash_mean, model_df_recover)
summary(m1)

colors <- c("blue", "white", "red")

ggplot(model_df_recover, aes(x=crash_mean, y=slope)) + 
geom_point(aes(fill = mean_temp, size = last_count), shape = 21, color = "black", stroke = 0.5) + 
geom_smooth(method = "lm", se = FALSE, color = "black") +
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
scale_size_continuous(name = "Last Survey\nPopulation Count") + 
labs(title = "The recovery rate and the population crash are related",
x = "Slope (Population Crash)",
y = "Slope (Recovery Rate") + 
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)) +
  annotate("text", x = Inf, y = Inf, label = "Adjusted R² = 0.4302", 
           hjust = 2.5, vjust = 2, size = 3, color = "black", fontface = "italic")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_population_crash.png", width = 6, height=4)

ggplot(model_df_recover, aes(x=recovery_years, y=slope)) + 
geom_point(aes(fill = mean_temp, size = last_count), shape = 21, color = "black", stroke = 0.5) + 
geom_smooth(method = "lm", se = FALSE, color = "black") +
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
scale_size_continuous(name = "Last Population\nCount") + 
theme_bw() +
theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8))

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

crash_null <- glm(crash_mean ~ 1, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_null)

crash_1 <- glm(crash_mean ~ mean_temp, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_1)

crash_2 <- glm(crash_mean ~ mean_temp + log_passage, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_2)

crash_3 <- glm(crash_mean ~ mean_temp + log_passage + standing_water, weights = mean_count, family = gaussian, data = model_df_crash)
summary(crash_3)

###################################################################################################################
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


# Fit the Bayesian model
null_model_slope <- brm(
  formula = slope_weighted ~ 1,  # Only the intercept
  data = model_df_recover,
  family = Gamma(link = "log"),
  prior = prior1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model <- brm(
  formula = slope_weighted ~ mean_temp + offset(recovery_years),
  data = model_df_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
summary(slope_model)

slope_model1 <- brm(
  formula = slope_weighted ~ mean_temp + I(mean_temp^2) + offset(recovery_years),
  data = model_df_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
summary(slope_model1)

slope_model_nonweighted <- brm(
  formula = slope ~ mean_temp + offset(recovery_years),
  data = model_df_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

slope_model2 <- brm(
  formula = slope_weighted ~ mean_temp + log_passage + offset(recovery_years),
  data = model_df_recover,
  family = Gamma(link = "log"),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

summary(slope_model2)

slope_model3 <- brm(
  formula = slope_weighted ~ mean_temp + log_passage + standing_water + offset(recovery_years),
  data = model_df_recover,
  family = Gamma(link = "log"),
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
loo_slope_model2 <- loo(slope_model2, moment_match = TRUE)
#loo_slope_model2 <- loo(slope_model2, moment_match = TRUE)
loo_compare(loo_slope_model, loo_slope_model1)

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
bayesian_r2 <- bayes_R2(slope_model3)
print(bayesian_r2)
b_r2_slope <- bayesian_r2[1, "Estimate"]
bayesian_r2_ci <- quantile(bayesian_r2, probs = c(0.025, 0.975))

# Create a finer grid of mean temp for smoother prediction lines
prediction_grid_s <- model_df_recover %>% 
 tidyr::expand(mean_temp = seq(min(mean_temp), max(mean_temp), length.out = 100))
# left_join(model_df_recover %>% select(mean_temp, recovery_years), by = "mean_temp") 

# Use the median recovery_years for prediction (or can use a specific value if preferred)
median_recovery_years <- median(model_df_recover$recovery_years)
median_log_passage <- median(model_df_recover$log_passage)
median_standing_water <- 1

prediction_grid_s <- prediction_grid_s %>% 
mutate(recovery_years = median_recovery_years,
      log_passage = median_log_passage, 
      standing_water = median_standing_water)

# Generate predictions using the fitted values from the model
predicted_slope_values <- fitted(slope_model3, newdata = prediction_grid_s, summary = TRUE)[, "Estimate"]
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
ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/recovery_rate_mean_temp.png", width = 8, height=6)


################# Posterior predictive checks (PPCs) ###########################################
slope_dens_linear <- pp_check(slope_model, type = "dens_overlay") +
  theme_bw() +  # White background with gridlines
  theme(
    panel.grid.major = element_line(color = "gray90"),  # Light gray gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.title = element_text(size = 12),  # Increase axis title font size
    axis.text = element_text(size = 10),   # Increase axis text size
    plot.background = element_rect(fill = "white")  # Ensure the background is white
  )

slope_dens_linear

slope_dens_quadratic <- pp_check(slope_model1, type = "dens_overlay") +
  theme_bw() +  # White background with gridlines
  theme(
    panel.grid.major = element_line(color = "gray90"),  # Light gray gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.title = element_text(size = 12),  # Increase axis title font size
    axis.text = element_text(size = 10),   # Increase axis text size
    plot.background = element_rect(fill = "white")  # Ensure the background is white
  )
slope_dens_quadratic

pp_check(slope_model, type = "hist")
pp_check(slope_model, type = "scatter")
pp_check(slope_model, type = "intervals")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/slope_dens.png", plot = slope_dens_quadratic, width = 7, height=5, dpi = 300)

##################################################################################################################
##########################CRASH MODELS #############################################################
####################################################################################################
source("direct_models.R")

model_data_crash <- model_data %>% filter(mean_count > 89)

# Fit the second Bayesian model
null_model_crash <- brm(
  formula = crash ~ 1,
  data = model_data,
  family = zero_one_inflated_beta(),
  prior = prior1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

crash_model <- brm(
  formula = crash ~ mean_temp + I(mean_temp^2),
  data = model_data_recover,
  family = zero_one_inflated_beta(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
summary(crash_model)

crash_model_linear <- brm(
  formula =  crash ~ mean_temp,
  data = model_data_recover,
  family = gaussian(),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)
summary(crash_model_linear)

crash_model1 <- brm(
  formula = crash ~ mean_temp + log_passage,
  data = model_data,
  family = zero_one_inflated_beta(),
  prior = prior,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  control = list(adapt_delta = 0.99)
)

# crash_model2 <- brm(
#   formula = crash ~ mean_temp + log_passage + standing_water,
#   data = model_data,
#   family = zero_one_inflated_beta(),
#   prior = prior,
#   chains = 4,
#   iter = 4000,
#   warmup = 1000,
#   control = list(adapt_delta = 0.99)
# )

############################################################################################
##################### Model Comparison ##################################################
# LOO-CV tends to be more stable, especially in small datasets or models with influencial observations
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


######################## VISUALIZE ########################################################
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

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/crash_rate_mean_temp.png", width = 8, height=6)

################# Posterior predictive checks (PPCs) ###########################################
crash_dens <- pp_check(crash_model0.5, type = "dens_overlay") + theme_bw() +
theme(
    panel.grid.major = element_line(color = "gray90"),  # Light gray gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.title = element_text(size = 12),  # Increase axis title font size
    axis.text = element_text(size = 10),   # Increase axis text size
    plot.background = element_rect(fill = "white")  # Ensure the background is white
  )
crash_dens
pp_check(crash_model0.5, type = "hist")
pp_check(crash_model0.5, type = "scatter")
pp_check(crash_model0.5, type = "intervals")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/pp_check_crash.png", plot = crash_dens, width = 7, height = 5, dpi = 300)

############################
# Same models using frequentists statistics and the weighted response
slope_weight_model <- glm(slope_weighted ~ mean_temp + offset(recovery_years), family = gaussian, data = model_df_recover)
summary(slope_weight_model)