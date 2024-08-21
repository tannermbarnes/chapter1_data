# THIS CODE IS FOR MODELS LOOKING AT THE DIFFERENCE IN TEMPERATURE SELECTION BETWEEN PERIODS (BEFORE AND AFTERS) WNS
rm(list = ls())
setwd("E:/chapter1_data/code")
#source("before_WNS.R")
source("survey_data.R")
library(tidyr)
library(purrr)
library(broom)
library(lme4)
library(patchwork)
before_data <- fin_filter %>% filter(period == "before") %>% 
filter(site != "Tippy Dam") %>% 
 mutate(mean_count = mean(count),
 sd = sd(count), 
 z_value = (count - mean_count) / sd) %>% 
 group_by(site) %>% 
 mutate(mean_count_site = mean(count),
 sd_site = sd(count), 
 z_value_site = (count - mean_count_site) / sd_site,
 z_overall = sum(z_value), 
 mean_temp_site = mean(mean_temp)) %>% 
 ungroup()

after_data <- fin_filter %>% filter(period == "after") %>% 
filter(site != "Tippy Dam") %>% 
 mutate(mean_count = mean(count),
 sd = sd(count), 
 z_value = (count - mean_count) / sd) %>% 
 group_by(site) %>% 
 mutate(mean_count_site = mean(count),
 sd_site = sd(count), 
 z_value_site = (count - mean_count_site) / sd_site, 
 z_overall = sum(z_value), 
 mean_temp_site = mean(mean_temp)) %>% 
 ungroup()

combined <- before_data %>% group_by(site) %>% slice(1) %>% 
select(site, mean_temp_site, z_overall, mean_count_site)
combinedx <- after_data %>% group_by(site) %>% slice(1) %>% 
select(site, mean_temp_site, z_overall, mean_count_site) %>% left_join(combined, by = "site")


ggplot(data = combined, aes(x = mean_temp_site.x, y = z_both)) +
geom_point()
# Plot points from both before_data and after_data on the same graph
# Plot points and overlay bell-shaped curves (density plots)
ggplot() +
  geom_point(data = before_data, aes(x = mean_temp, y = z_value), color = "blue") +
  geom_point(data = after_data, aes(x = mean_temp, y = z_value), color = "red") +
  geom_histogram(data = before_data, aes(x = mean_temp, weight = z_value), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_histogram(data = after_data, aes(x = mean_temp, weight = z_value), binwidth = 0.5, fill = "red", color = "black", alpha = 0.7) +
  geom_smooth(data = before_data, aes(x = mean_temp, y = z_value), color = "blue", method = "loess", se = FALSE) +
  geom_smooth(data = after_data, aes(x = mean_temp, y = z_value), color = "red", method = "loess", se = FALSE) +
  ggtitle("Z-values with Bell-Shaped Curves for Before and After Periods") +
  xlab("Mean Temperature") +
  ylab("Z-value") +
  theme_minimal()


# Correct the period based on the year: if year is before 2013, set period to "before"
# Check if the changes were applied correctly
table(fin_filter$period, fin_filter$year)  # This will give you a table of periods by year for a quick verification

##############################################################################################################3
##################################################################################################################
################## USE LINEAR MIXED-EFFECTS MODEL #######################################################
###############################################################################################################
# Fit a linear mixed-effects model
model <- lmer(mean_temp ~ period + (1|site), data = fin_filter, weights = count)
# Summary of the model
summary(model)
##########################################################################################
########## PAIRED T-TEST #############################################################
#####################################################################################

########### PERMUTATION TEST #######################
######################################################
weighted_temp_per_site_period <- fin_filter %>%
  group_by(site, period) %>%
  summarise(weighted_mean_temp = sum(mean_temp * count) / sum(count)) %>%
  ungroup()

observed_diff <- weighted_temp_per_site_period %>%
  spread(period, weighted_mean_temp) %>%
  mutate(diff = after - before) %>%
  summarise(observed_diff = mean(diff, na.rm = TRUE))

set.seed(123)  # For reproducibility

n_permutations <- 1000
permuted_diffs <- replicate(n_permutations, {
  permuted_data <- fin %>%
    mutate(period = sample(period)) %>%  # Shuffle the period labels
    group_by(site, period) %>%
    summarise(weighted_mean_temp = sum(mean_temp * count) / sum(count)) %>%
    spread(period, weighted_mean_temp) %>%
    mutate(diff = after - before) %>%
    summarise(diff = mean(diff, na.rm = TRUE))
  
  permuted_data$diff
})

# Combine observed difference with permuted differences
all_diffs <- c(observed_diff$observed_diff, permuted_diffs)

# Calculate p-value
p_value <- mean(abs(permuted_diffs) >= abs(observed_diff$observed_diff))

hist(all_diffs, main = "Permutation Test for Temperature Selection",
     xlab = "Difference in Weighted Mean Temperature", 
     col = "lightblue", border = "black")
abline(v = observed_diff$observed_diff, col = "red", lwd = 2)





library(brms)
library(rstan)
Sys.setenv(PATH = paste("E:/rtools44/x86_64-w64-mingw32.static.posix/bin",
                        "E:/rtools44/usr/bin", 
                        Sys.getenv("PATH"), 
                        sep = ";"))

# Sample with replacement to create a bootstrapped dataset
set.seed(123)
before_samples <- sample(before_data$mean_temp, size = 1000, replace = TRUE)
after_samples <- sample(after_data$mean_temp, size = 1000, replace = TRUE)

# Create a data frame for the sampled data
sampled_data <- data.frame(
  median_temp = c(before_samples, after_samples),
  period = rep(c("before", "after"), each = 1000)
)

# Calculate the difference between the after and before periods
temp_diff_distribution <- after_samples - before_samples
# Summary statistics of the differences
summary(temp_diff_distribution)

# Plot the distribution of temperature differences
hist(temp_diff_distribution, breaks = 50, main = "Distribution of Temperature Differences", 
     xlab = "Temperature Difference (After - Before)", col = "lightblue")

# Define a simple Bayesian model to sample from the mean_temp data
formula <- bf(mean_temp | weights(count) ~ period)

# Fit the model using brms
fit <- brm(formula, data = fin_filter, family = gaussian(), 
           prior = c(set_prior("normal(0, 10)", class = "b")),
           iter = 2000, warmup = 1000, chains = 4, seed = 123)

# Summary of the model
summary(fit)
plot(fit)
png(filename = "E:/chapter1_data/figures/final/brms_fit_plot_test.png")
dev.off()

##################################################################################################################
########## Visulizing the mean temps chose in each period ########################################################################
##################################################################################################################
# Histogram of mean temperatures for the 'before' period
# Histogram of mean temperatures for the 'before' period, weighted by bat count
p1 <- before_data %>% filter(site != "Tippy Dam") %>% drop_na(mean_temp) %>% 
ggplot(aes(x = mean_temp, weight = count)) +
  geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7) +
  ggtitle("Total Bats by Mean Temperature (Before & After Periods)") +
  xlab("") +
  ylab("Total Bats Hibernating") +
  scale_x_continuous(limits = c(-4, 12), breaks = seq(-2, 12, by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Histogram of mean temperatures for the 'after' period, weighted by bat count
p2 <- after_data %>% filter(site != "Tippy Dam") %>% drop_na(mean_temp) %>% filter(year > 2012) %>% 
ggplot(aes(x = mean_temp, weight = count)) +
  geom_histogram(binwidth = 0.5, fill = "red", alpha = 0.7) +
  ggtitle("") +
  xlab("Mean Temperature (Binned by 0.5 Degrees)") +
  ylab("Total Bats Hibernating") +
  scale_x_continuous(limits = c(-2, 12), breaks = seq(-4, 12, by = 2)) +
  theme_minimal()

combined_plot <- p1 + p2 + plot_layout(ncol = 1, heights = c(1,1))

ggsave("E:/chapter1_data/figures/final/bat_hibe_temp.png", plot = combined_plot, width = 8, height = 10)

combined_data <- fin %>% drop_na(mean_temp) %>% 
 mutate(period = as.factor(period))

combined_data %>% 
 ggplot(aes(x = mean_temp, weight = count), fill = period) + 
  geom_histogram(binwidth = 0.5, alpha = 0.7) +
  scale_fill_manual(values = c("blue", "red")) +  # Custom colors for periods
  facet_wrap(~ period) +  # Facet by period to create separate panels
  ggtitle("Total Bats by Mean Temperature (Before vs After)") +
  xlab("Mean Temperature (Binned by 0.5 Degrees)") +
  ylab("Total Bats Hibernating") +
  theme_minimal()

#################################### ANOVA TEST USING 5 DEGREES #################################################
fin_filter <- fin_filter %>% 
 mutate(temp_category = ifelse(mean_temp >= 5, "Above 5°C", "Below 5°C"))

# Run 2-way ANOVA
anova_result <- aov(count ~ period * temp_category, data = fin_filter)

# Summary of the ANOVA
summary(anova_result)
# Tukey's Honest Significant Difference (HSD) test for post-hoc analysis
TukeyHSD(anova_result)


zb <- before_data %>% group_by(site) %>%
reframe(period = period, mean_count = mean(count, na.rm = TRUE), site_mean_temp = site_mean_temp) %>% group_by(site) %>% 
slice(1) %>% ungroup()

zb <- zb %>% mutate(
  min_mean_count = min(mean_count, na.rm = TRUE),
  max_mean_count = max(mean_count, na.rm = TRUE),
  normalized_value = (mean_count - min_mean_count) / (max_mean_count - min_mean_count)
)

za <- after_data %>% group_by(site) %>%
reframe(period = period, mean_count = mean(count, na.rm = TRUE), site_mean_temp = site_mean_temp) %>% group_by(site) %>% 
slice(1) %>% ungroup()

za <- za %>% mutate(
  min_mean_count = min(mean_count, na.rm = TRUE),
  max_mean_count = max(mean_count, na.rm = TRUE),
  normalized_value = (mean_count - min_mean_count) / (max_mean_count - min_mean_count)
)

xx <- zb %>% left_join(za, by = "site")

fin_normal <- xx %>% select(mean_count_before = mean_count.x, mean_count_after = mean_count.y, 
mean_temp = site_mean_temp.x, normalized_value_before = normalized_value.x, normalized_value_after = normalized_value.y) %>% 
mutate(normalized_difference = normalized_value_after - normalized_value_before)

fin_normal %>% ggplot(aes(x=mean_temp, y = normalized_value_before, color = "blue")) +
geom_point() +
geom_point(aes(y=normalized_value_after, color = "red"))

ggsave("E:/chapter1_data/figures/final/test.png", width = 8, height = 10)
 