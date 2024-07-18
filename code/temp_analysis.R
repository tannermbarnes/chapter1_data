rm(list = ls())
setwd("E:/chapter1_data/code")
source("before_WNS.R")
library(tidyr)
library(purrr)
library(broom)

before_data %>%
  ggplot(aes(x = max)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Median Temperatures",
       x = "Median Temperature",
       y = "Count") +
  theme_bw()

ggsave("E:/chapter1_data/figures/final/before_wns_median_temp.png", width = 6, height=4)

after_data %>%
  ggplot(aes(x = max)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Median Temperatures",
       x = "Median Temperature",
       y = "Count") +
  theme_bw()

ggsave("E:/chapter1_data/figures/final/after_wns_median_temp.png", width = 6, height=4)

library(ggplot2)
library(patchwork)

# Create the first histogram for before_data
p1 <- before_data %>%
filter(site != "Tippy Dam") %>% 
  ggplot(aes(x = median_temp)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Median Temperatures (Before)",
       x = "Median Temperature",
       y = "Count") +
  theme_bw()

# Create the second histogram for after_data
p2 <- after_data %>%
filter(site != "Tippy Dam") %>% 
  ggplot(aes(x = median_temp)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Median Temperatures (After)",
       x = "Median Temperature",
       y = "Count") +
  theme_bw()

# Combine the two plots side by side
combined_plot <- p1 + p2

# Save the combined plot
ggsave("E:/chapter1_data/figures/final/before_after_median_temp.png", plot = combined_plot, width = 12, height = 6)


before_data %>%
  ggplot(aes(x = median_temp, y = max_count)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "blue") +
  labs(title = "Relationship between Median Temperature and Max Count (GAM)",
       x = "Median Temperature",
       y = "Max Count") +
  theme_bw()

p1 <- before_data %>%
filter(site != "Tippy Dam") %>% 
  ggplot(aes(x = mean_temp, y = max_count)) +
  geom_point() +
  geom_smooth(method = "loess", color = "green") +
  labs(title = "Relationship between Mean Temperature and Max Count (LOESS)",
       x = "Mean Temperature",
       y = "Max Count") +
  theme_bw()

p2 <- after_data %>%
filter(site != "Tippy Dam") %>% 
  ggplot(aes(x = mean_temp, y = max_count)) +
  geom_point() +
  geom_smooth(method = "loess", color = "green") +
  labs(title = "Relationship between Mean Temperature and Max Count (LOESS)",
       x = "Mean Temperature",
       y = "Max Count") +
  theme_bw()

combined_plot <- p1 + p2
# Save the combined plot
ggsave("E:/chapter1_data/figures/final/mean_scatter.png", plot = combined_plot, width = 9, height = 6)

# p1 <- before_data %>%
#   ggplot(aes(x = min, y = max_count)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "purple") +
#   labs(title = "Relationship between Minimum Temperature and Max Count (Polynomial Regression)",
#        x = "Minimum Temperature",
#        y = "Max Count") +
#   theme_bw()

# p2 <- after_data %>%
#   ggplot(aes(x = min, y = max_count)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "purple") +
#   labs(title = "Relationship between Minimum Temperature and Max Count (Polynomial Regression)",
#        x = "Minimum Temperature",
#        y = "Max Count") +
#   theme_bw()


# Prepare before_data for plotting
before_plot <- before_data %>%
  group_by(standing_water) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = standing_water, y = count, fill = standing_water)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Before Data: Standing Water Count",
       x = "Standing Water",
       y = "Count") +
  scale_fill_manual(values = c("no" = "blue", "yes" = "green", "NA" = "grey")) +
  theme_minimal()

# Prepare after_data for plotting
after_plot <- after_data %>%
  group_by(standing_water) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = standing_water, y = count, fill = standing_water)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "After Data: Standing Water Count",
       x = "Standing Water",
       y = "Count") +
  scale_fill_manual(values = c("no" = "blue", "yes" = "green", "NA" = "grey")) +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(before_plot, after_plot, ncol = 2)

combined_plot <- before_plot + after_plot
# Save the combined plot
ggsave("E:/chapter1_data/figures/final/median_scatter.png", plot = combined_plot, width = 9, height = 6)


##############################################################################################################3
# Install and load necessary packages
install.packages("brms")
install.packages("rstan")
library(brms)
library(rstan)
Sys.setenv(PATH = paste("E:/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "E:/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

# Sample with replacement to create a bootstrapped dataset
before_samples <- sample(before_data$mean_temp, size = 1000, replace = TRUE)
after_samples <- sample(after_data$mean_temp, size = 1000, replace = TRUE)

# Create a data frame for brms
sampled_data <- data.frame(mean_temp = c(before_samples, after_samples),
                           period = rep(c("before", "after"), each = 1000))

# Define a simple Bayesian model to sample from the mean_temp data
formula <- bf(mean_temp ~ period)

# Fit the model using brms
fit <- brm(formula, data = sampled_data, family = gaussian(), 
           prior = c(set_prior("normal(0, 10)", class = "b")),
           iter = 2000, warmup = 1000, chains = 4, seed = 123)

# Summary of the model
summary(fit)

png(filename = "E:/chapter1_data/figures/final/brms_fit_plot.png", width = 800, height = 600)

plot(fit)

dev.off()


# Create histograms for before_data and after_data
p1 <- before_data %>%
  ggplot(aes(x = mean_temp)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.5) +
  labs(title = "Before WNS", x = "Mean Temperature", y = "Count") +
  theme_bw()

p2 <- after_data %>%
  ggplot(aes(x = mean_temp)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.5) +
  labs(title = "After WNS", x = "Mean Temperature", y = "Count") +
  theme_bw()

# Save plots
ggsave("E:/chapter1_data/figures/final/before_mean_temp.png", plot = p1, width = 6, height = 4)
ggsave("E:/chapter1_data/figures/final/after_mean_temp.png", plot = p2, width = 6, height = 4)

before_data <- before_data %>% mutate(shafts = as.numeric(shafts))
after_data <- after_data %>% mutate(shafts = as.numeric(shafts))
# Combine the data into one dataframe
combined_data <- bind_rows(
  before_data %>% mutate(period = "Before"),
  after_data %>% mutate(period = "After")
)

# Overlay histograms
p_combined <- combined_data %>%
  ggplot(aes(x = mean_temp, fill = period)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.5, position = "identity") +
  labs(title = "Mean Temperature Distribution Before and After WNS", x = "Mean Temperature", y = "Count") +
  scale_fill_manual(values = c("Before" = "blue", "After" = "red")) +
  theme_bw()

# Save combined plot
ggsave("E:/chapter1_data/figures/final/test.png", plot = p_combined, width = 8, height = 6)

# Perform Kolmogorov-Smirnov test
ks_test <- ks.test(before_data$mean_temp, after_data$mean_temp)

# Print test result
print(ks_test)


################################################# Weighted Mean Analysis ####################################################
library(dplyr)
library(ggplot2)
library(brms)

# Function to calculate weighted mean temperature for each site
weighted_mean_temp_per_site <- function(data) {
  data %>%
    group_by(site) %>%
    summarise(weighted_mean_temp = sum(mean_temp * max_count) / sum(max_count)) %>%
    ungroup()
}

# Define the number of bootstrap samples
n_boot <- 1000

# Initialize vectors to store bootstrap results
bootstrap_before <- numeric(n_boot)
bootstrap_after <- numeric(n_boot)

# Calculate site-level weighted mean temperatures
weighted_before_data <- weighted_mean_temp_per_site(before_data)
weighted_after_data <- weighted_mean_temp_per_site(after_data)

# Perform bootstrapping
set.seed(123)  # Set a seed for reproducibility
for (i in 1:n_boot) {
  # Resample (with replacement) for the 'before' period
  sampled_before <- weighted_before_data %>% sample_n(size = nrow(weighted_before_data), replace = TRUE)
  bootstrap_before[i] <- mean(sampled_before$weighted_mean_temp)
  
  # Resample (with replacement) for the 'after' period
  sampled_after <- weighted_after_data %>% sample_n(size = nrow(weighted_after_data), replace = TRUE)
  bootstrap_after[i] <- mean(sampled_after$weighted_mean_temp)
  
  if (i <= 5) { # Print the first few samples for debugging
    cat("Sample", i, "before weighted mean:", bootstrap_before[i], "\n")
    cat("Sample", i, "after weighted mean:", bootstrap_after[i], "\n")
  }
}

# Check standard deviations for debugging
cat("Standard deviation of bootstrap_before:", sd(bootstrap_before), "\n")
cat("Standard deviation of bootstrap_after:", sd(bootstrap_after), "\n")

# Calculate 95% confidence intervals
ci_before <- quantile(bootstrap_before, c(0.025, 0.975))
ci_after <- quantile(bootstrap_after, c(0.025, 0.975))

# Print results
cat("95% CI for weighted mean temperature before white-nose syndrome:", ci_before, "\n")
cat("95% CI for weighted mean temperature after white-nose syndrome:", ci_after, "\n")

# Perform Mann-Whitney U test
test_result <- wilcox.test(bootstrap_before, bootstrap_after)
# Print test results
cat("Mann-Whitney U test result:\n")
print(test_result)

# Perform Welch's t-test
t_test_result <- t.test(bootstrap_before, bootstrap_after, var.equal = FALSE)
# Print t-test results
cat("Welch's t-test result:\n")
print(t_test_result)

# Combine bootstrap results into a single dataframe for plotting
bootstrap_results <- data.frame(
  Period = rep(c("Before", "After"), each = n_boot),
  WeightedMeanTemp = c(bootstrap_before, bootstrap_after)
)

# Define the density plot with confidence intervals
p <- ggplot(bootstrap_results, aes(x = WeightedMeanTemp, fill = Period)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = data.frame(ci = ci_before, Period = "Before"), aes(xintercept = ci, color = Period), linetype = "dashed") +
  geom_vline(data = data.frame(ci = ci_after, Period = "After"), aes(xintercept = ci, color = Period), linetype = "dashed") +
  labs(title = "Bootstrapped Distributions of Weighted Mean Temperatures",
       x = "Weighted Mean Temperature",
       y = "Density") +
  scale_fill_manual(values = c("Before" = "blue", "After" = "red")) +
  scale_color_manual(values = c("Before" = "blue", "After" = "red")) +
  theme_bw()

# Save and display the plot
ggsave("E:/chapter1_data/figures/final/bootstrapped_distributions_weighted_mean_temp.png", plot = p, width = 8, height = 6)
print(p)

# Combine the bootstrap samples
combined_samples <- c(bootstrap_before, bootstrap_after)
n_before <- length(bootstrap_before)
n_after <- length(bootstrap_after)

# Define the number of bootstrap samples for hypothesis testing
n_boot_ht <- 1000

# Initialize vector to store bootstrap differences
bootstrap_diff <- numeric(n_boot_ht)

# Perform bootstrap hypothesis testing
set.seed(124)  # Set a seed for reproducibility
for (i in 1:n_boot_ht) {
  # Shuffle combined samples
  shuffled_samples <- sample(combined_samples)
  
  # Split the shuffled samples
  sample_before <- shuffled_samples[1:n_before]
  sample_after <- shuffled_samples[(n_before + 1):(n_before + n_after)]
  
  # Calculate the difference in means
  bootstrap_diff[i] <- mean(sample_before) - mean(sample_after)
}

# Calculate the observed difference in means
obs_diff <- mean(bootstrap_before) - mean(bootstrap_after)

# Calculate the p-value
p_value <- mean(abs(bootstrap_diff) >= abs(obs_diff))

# Print the p-value
cat("Bootstrap hypothesis test p-value:", p_value, "\n")

# Install the package if not already installed
if (!requireNamespace("BayesFactor", quietly = TRUE)) {
  install.packages("BayesFactor")
}

library(BayesFactor)

# Perform Bayesian t-test
bayes_t_test_result <- ttestBF(x = bootstrap_before, y = bootstrap_after)

# Print Bayesian t-test results
cat("Bayesian t-test result:\n")
print(bayes_t_test_result)

#################################### Bayesian Model binned by Before and After ###########################################
library(dplyr)

# Add a column to each dataset to indicate the period
before_data <- before_data %>% mutate(period = "before")
after_data <- after_data %>% mutate(period = "after")
combined_data <- bind_rows(before_data, after_data) %>% 
select(site, mean_temp, min, max, period, max_count) %>% 
drop_na(max_count)

unique_sites <- unique(data1$site)
print(unique_sites)
site_mapping <- setNames(seq_along(unique_sites), unique_sites)
print(site_mapping)
data1$site_numeric <- as.numeric(factor(data1$site, levels = unique_sites))
print(data1)

both_periods <- data1 %>% 
group_by(site) %>% 
filter(n_distinct(period) == 2) %>% 
ungroup()

df1 <- data1 %>% 
select(site, site_numeric, period, year, min, max, mean_temp, count) %>% 
group_by(site) %>% 
filter(n_distinct(period) == 2) %>% 
ungroup()

# Load necessary libraries
library(brms)
library(rstan)
Sys.setenv(PATH = paste("E:/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "E:/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

# Define the formula including weights
formula <- bf(count ~ period + mean_temp + (1 | site_numeric))

# Fit the Bayesian model
fit <- brm(mean_temp | weights(count) ~ period, 
           data = df1, family = gaussian(),
           prior = c(set_prior("normal(0, 10)", class = "b")),
           iter = 4000, warmup = 1000, chains = 4, seed = 123,
           control = list(adapt_delta = 0.95))

# Print the summary of the model
summary(fit)
# Plot the model diagnostics
plot(fit)
pp_check(fit)
# Check the posterior distributions
posterior_samples <- posterior_samples(fit)
head(posterior_samples)

ggplot(df1, aes(x = mean_temp, fill = period)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Mean Temperature by Period",
       x = "Mean Temperature",
       y = "Density") +
  theme_minimal()

# Create a new data frame for predictions
pred_data <- expand.grid(period = unique(df1$period),
                          mean_temp = seq(min(df1$mean_temp), max(df1$mean_temp), length.out = 100))

# Get predictions
predictions <- posterior_predict(fit, newdata = pred_data)
pred_data$predicted_mean_temp <- apply(predictions, 2, mean)

ggplot(pred_data, aes(x = mean_temp, y = predicted_mean_temp, color = period)) +
  geom_line() +
  labs(title = "Predicted Mean Temperature by Period",
       x = "Mean Temperature",
       y = "Predicted Mean Temperature") +
  theme_minimal()




library(lme4)
m1 <- lm(mean_temp ~ period, weights = count, data = df1)
summary(m1)

m2 <- lmer(mean_temp ~ period + (1 | site_numeric), weights = count, data = df1)

summary(m2)

library(effects)

# Compute the effects
effects_model <- effect("period", m2)

# Plot the effects
p8 <- plot(effects_model,
     main = "Effect of Period on Mean Temperature",
     xlab = "Period",
     ylab = "Mean Temperature")
p8

png("E:/chapter1_data/figures/final/effects_model.png", width = 800, height = 600)
print(p8)
dev.off()
