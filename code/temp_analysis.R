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
library(dplyr)

# Step 1: Calculate overall mean and standard deviation for each period
overall_stats <- fin_filter %>%
  filter(site != "Tippy Dam") %>%
  group_by(period) %>%
  summarise(
    overall_mean_count = mean(count, na.rm = TRUE),
    overall_sd_count = sd(count, na.rm = TRUE)
  )

# Step 2: Merge overall stats with the site-specific data
combined_data <- fin_filter %>%
  filter(site != "Tippy Dam") %>%
  group_by(site, period) %>%
  summarise(
    mean_count_site = mean(count, na.rm = TRUE),  # Mean count for the site and period
    site_mean_temp = first(site_mean_temp)  # Assuming site_mean_temp is constant per site
  ) %>%
  left_join(overall_stats, by = "period") %>%  # Join overall mean and SD
  mutate(
    z_value = (mean_count_site - overall_mean_count) / overall_sd_count  # Z-score calculation
  ) %>%
  ungroup()

# Step 3: Pivot wider so each site has one row, with separate columns for each period's stats
combined <- combined_data %>%
  pivot_wider(
    names_from = period,
    values_from = c(mean_count_site, z_value, site_mean_temp, overall_mean_count, overall_sd_count),
    names_prefix = "period_"
  ) %>% 
    mutate(
    z_value = ifelse(
      z_value_period_before < 0, 
      z_value_period_after + z_value_period_before,  # If before is negative, add
      z_value_period_after - z_value_period_before   # If before is positive, subtract
    )) %>% 
  select(site, mean_count_before = mean_count_site_period_before, mean_count_after = mean_count_site_period_after, 
  z_value_before = z_value_period_before, z_value_after = z_value_period_after, z_value, mean_temp = site_mean_temp_period_before, 
  )

important <- combined %>% filter(mean_count_after > 200)

# graph it
model <- lm(z_value ~ mean_temp, data = combined)
adj_r_squared <- summary(model)$adj.r.squared
adj_r_squared_text <- paste0("Adjusted R² = ", round(adj_r_squared, 4))


ggplot(data = combined, aes(x = mean_temp, y = z_value, size = mean_count_after)) +
geom_point(alpha = 0.7) +
geom_smooth(method = "lm", se = FALSE, color = "darkorange", size = 1.2) +
labs(
  title = "Mine Importance Decreases as Mean Temperature Increases",
  x = "Mean Temperature", 
  y = "Z-value-After WNS - Z-value-Before WNS)", 
  size = "Mean Count\n(After WNS)"
) + 
  annotate("text", x = Inf, y = Inf, label = adj_r_squared_text, 
           hjust = 1.1, vjust = 1.5, size = 5, color = "black") +
  theme_bw() +  # Base theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),  # Centered and bold title
    axis.title = element_text(size = 12),  # Larger axis titles
    legend.title = element_text(size = 10),  # Larger legend title
    legend.text = element_text(size = 8),  # Larger legend text
    panel.grid = element_blank(),  # Remove grid lines
  )
ggsave("E:/chapter1_data/figures/final/z-value.png", width = 8, height = 8)

pop_proportions <- combined %>% reframe(site, mean_temp, sum_before = sum(mean_count_before), proportion_before = (mean_count_before / sum_before),
sum_after = sum(mean_count_after), proportion_after = (mean_count_after / sum_after), mean_count_before, mean_count_after) %>% 
mutate(props = (proportion_after - proportion_before), mean_temp_squared = mean_temp^2)

im <- pop_proportions %>% filter(mean_count_before > 300) %>% filter(site != "Millie Mine")
# Fit your custom model
model1 <- lm(props ~ mean_temp + I(mean_temp^2), data = im)

# Extract adjusted R-squared value
adj_r_squared1 <- summary(model1)$adj.r.squared
adj_r_squared_text1 <- paste0("Adjusted R² = ", round(adj_r_squared1, 4))

# Create the plot using the custom model
ggplot(data = im, aes(x = mean_temp, y = props)) +
  geom_point(aes(color = ifelse(props > 0, "positive", "negative")), alpha = 0.7) +  # Color points by positive/negative values
  # Add a line using the custom model fitted values
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "#e100ffca", size = 1.2) +
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

ggsave("E:/chapter1_data/figures/final/proportions_bats-important.png", width = 8, height = 8)


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

##################################################################################
# z-value after - z-value before #
#####################################################
bs <- before_data %>% group_by(site) %>% slice(1) %>% 
select(site, site_mean_temp, mean_count_site) %>% ungroup()

bs <- bs %>% 
mutate(mean_overall_b = mean(mean_count_site), 
       sd_overall_b = sd(mean_count_site), 
       z_value_overall_b = (mean_count_site - mean_overall_b) / (sd_overall_b)) %>% 
select(site, site_mean_temp, z_value_overall_b)

as <- after_data %>% group_by(site) %>% slice(1) %>% 
select(site, site_mean_temp, mean_count_site) %>% ungroup()

as <- as %>% 
mutate(mean_overall_a = mean(mean_count_site), 
       sd_overall_a = sd(mean_count_site), 
       z_value_overall_a = (mean_count_site - mean_overall_a) / (sd_overall_a)) %>% 
select(site, z_value_overall_a)

cs <- bs %>% left_join(as, by = "site") %>% 
 mutate(z_value_c = z_value_overall_a - z_value_overall_b)

ggplot(data = cs, aes(x = site_mean_temp, y = z_value_c)) +
geom_point() +
geom_smooth(method = "lm")

summary(lm(z_value_c ~ site_mean_temp, data = cs))







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
 