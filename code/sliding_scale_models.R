rm(list = ls())
setwd("E:/chapter1_data/code")
library(lavaan)
library(lme4)
library(car)
library(lmtest)
library(caTools)
library(quantmod)
library(MASS)
library(corrplot)
##################################### Models for mines on the sliding scale method ###################################
source("sliding_scale.R")

#View(df_slide_scale)

#Path anaylsis
model_df <- df_slide_scale %>% 
mutate(recovery_status = ifelse(bin == 1, "recovering", "not recovering")) %>% 
mutate(recovery_status = as.factor(recovery_status)) %>% 
mutate(site_numeric = as.numeric(factor(site))) %>% 
select(site, site_numeric, bin, recovery_status, recovery_years, mini_year, max_year, last_year, slope, standing_water, passage_length, log_passage, levels, shafts, 
crash, min, max, median_temp, mean_temp, temp_diff, ore) %>% 
mutate(temp_diff_sqrt = sqrt(temp_diff),
temp_diff_log = log(temp_diff),
bin_numeric = as.numeric(bin),
ore = as.factor(ore),
levels = as.factor(levels),
shafts = as.factor(shafts)) %>% 
mutate(levels_numeric = as.numeric(levels), 
shafts_numeric = as.numeric(shafts))

# Some adjustments 
model_df$standing_water[28] <- "yes"
model_df$standing_water[is.na(model_df$standing_water)] <- "no"
# Change row 37 to yes
model_df$mean_temp_ranked <- rank(model_df$mean_temp)
model_df$mean_temp_transformed <- scale(model_df$mean_temp_ranked)

model_df_crash <- model_df %>% mutate(new_crash = ifelse(slope > 0, crash, 1))

# CFI = comparative fit index = models greater than 0.9, conservatively 0.95 indicate good fit
# TLI = values greater than 0.9 indicating good fit. CFI is always greater than TLI
# RMSEA <= 0.05 close-fit, >= 0.10 poor fit, between 0.5 & 0.8 reasonable approximate fit does not compare to baseline model
# The baseline model is the worst fitting model assumes no covariances between variables wanna compare our model to baseline model
# Saturated model is best model df = 0
# Your model is somewhere between baseline and saturated model
model_df_filter <- model_df %>% filter(site != "Tippy Dam")

summary(lm(slope ~ recovery_years, data = model_df_filter))

model_df_filter %>% 
ggplot(aes(x = recovery_years, y = slope)) +
geom_point() + 
geom_smooth(method = "lm")

binary_model <- '
# Latent variable (measurement model)
complex =~ log_passage + levels + shafts
# Regressions (structured models)
slope ~ complex
'

fit1 <- sem(binary_model, data = model_df, ordered = c("bin", "levels", "shafts"))
summary(fit1, fit.measures = TRUE)
fitMeasures(fit1, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit1, standardized = TRUE)

# Best model according to model specifics
binary_model2 <- '
# Latent variable (measurement model)
complex =~ temp_diff_log + log_passage + levels_numeric + shafts_numeric
# Regressions
crash ~ complex
bin_numeric ~ mean_temp + complex + crash
'

# Maximum likelihood estimator is not supported for ordered data
fit2 <- sem(binary_model2, data = model_df_crash)
summary(fit2, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit2, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit2, standardized = TRUE)

binary_model3 <- '
# Latent variable (measurement model)
complex =~ temp_diff_log + log_passage + levels + shafts
# regressions (structured models)
crash ~ complex + max
bin ~ complex + crash + max
'

fit3 <- sem(binary_model3, data = model_df, ordered = c("bin", "levels", "shafts"))
summary(fit3, fit.measures = TRUE)
fitMeasures(fit3, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))


# Extract path coefficients
path_coefficients <- parameterEstimates(fit3, standardized = TRUE)
# Extract residual variances
residual_variances <- inspect(fit3, "resid")  # Theta matrix contains residual variances and covariances
# Extract variance of the latent variable
latent_variance <- inspect(fit3, "cov.lv")
# Calculate standardized error variance for the latent variable
std_error_variance <- 1 - diag(latent_variance)
# Print the results
print("Path Coefficients:")
print(path_coefficients)

print("Residual Variances:")
print(residual_variances)

print("Latent Variable Variance:")
print(latent_variance)

# Calculate the standardized error variance if the latent variable variance is standardized
if (!is.null(latent_variance)) {
  std_error_variance <- diag(latent_variance)  # Extract the diagonal elements which are the variances
  print("Standardized Error Variance:")
  print(std_error_variance)
} else {
  print("Latent variable variance is not available.")
}

############################### CHECK RELIABILITY INDEX FOR PREDICTORS OF COMPLEX #########################3
corelation_matrix <- model_df %>% select(temp_diff_log, log_passage, levels, shafts) %>% 
mutate(levels = as.numeric(levels),
        shafts = as.numeric(shafts))
cor(corelation_matrix)

measurement_forumula <- 'complex =~ temp_diff_log + log_passage + levels + shafts'

measurement_model <- sem(measurement_forumula, data = corelation_matrix)
summary(measurement_model)
print(modindices(measurement_model))
# Fail to reject our latent construct of complexity, which we can now use to evalute the structured model


model_df %>% 
ggplot(aes(x = temp_diff, y = slope)) + 
geom_point() +
geom_smooth()


data_x <- model_df %>% select(levels_numeric, shafts_numeric, log_passage, temp_diff)
var <- cor(data_x)
var_inv <- ginv(var)
colnames(var_inv) <- colnames(data_x)
rownames(var_inv) <- colnames(data_x)
corrplot(var_inv, method = 'number', is.corr = F)



#### Check model parameters ###
######## Normality #########
# Histograms
ggplot(model_df, aes(x = temp_diff)) + geom_histogram(binwidth = 1) + ggtitle("Histogram of Levels")
ggplot(model_df, aes(x = shafts_numeric)) + geom_histogram(binwidth = 1) + ggtitle("Histogram of Shafts")
ggplot(model_df, aes(x = levels_numeric)) + geom_histogram(binwidth = 1) + ggtitle("Histogram of Temp Diff")
ggplot(model_df, aes(x = log_passage)) + geom_histogram(binwidth = 1) + ggtitle("Histogram of Log Passage")

# Q-Q plots
qqnorm(model_df$levels); qqline(model_df$levels, col = "red")
qqnorm(model_df$shafts); qqline(model_df$shafts, col = "red")
qqnorm(model_df$temp_diff); qqline(model_df$temp_diff, col = "red")
qqnorm(model_df$log_passage); qqline(model_df$log_passage, col = "red")

# Shapiro-Wilk test
shapiro.test(model_df$mean_temp_transformed)
shapiro.test(model_df$shafts)
shapiro.test(model_df$temp_diff)
shapiro.test(model_df$log_passage)

# Kolmogorov-Smirnov test
ks.test(model_df$levels, "pnorm", mean=mean(model_df$levels), sd=sd(model_df$levels))
ks.test(model_df$shafts, "pnorm", mean=mean(model_df$shafts), sd=sd(model_df$shafts))
ks.test(model_df$temp_diff, "pnorm", mean=mean(model_df$temp_diff), sd=sd(model_df$temp_diff))
ks.test(model_df$log_passage, "pnorm", mean=mean(model_df$log_passage), sd=sd(model_df$log_passage))

library(e1071)

# Skewness and Kurtosis
skewness(model_df$levels)
kurtosis(model_df$levels)
skewness(model_df$shafts)
kurtosis(model_df$shafts)
skewness(model_df$temp_diff)
kurtosis(model_df$temp_diff)
skewness(model_df$log_passage)
kurtosis(model_df$log_passage)

# Box-Cox transformation (requires the 'MASS' package)
library(MASS)
boxcox_result <- boxcox(temp_diff ~ 1, data = model_df)
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
model_df$temp_diff_boxcox <- (model_df$temp_diff^lambda - 1) / lambda


# Log transformation
shapiro.test(model_df$temp_diff_log)
ggplot(model_df, aes(x = temp_diff_log)) + geom_histogram(binwidth = 0.1) + ggtitle("Histogram of Log Transformed Temp Diff")
qqnorm(model_df$temp_diff_log); qqline(model_df$temp_diff_log, col = "red")

# Square root transformation
shapiro.test(model_df$temp_diff_sqrt)
ggplot(model_df, aes(x = temp_diff_sqrt)) + geom_histogram(binwidth = 0.1) + ggtitle("Histogram of Square Root Transformed Temp Diff")
qqnorm(model_df$temp_diff_sqrt); qqline(model_df$temp_diff_sqrt, col = "red")

# Box-Cox transformation
shapiro.test(model_df$temp_diff_boxcox)
ggplot(model_df, aes(x = temp_diff_boxcox)) + geom_histogram(binwidth = 0.1) + ggtitle("Histogram of Box-Cox Transformed Temp Diff")
qqnorm(model_df$temp_diff_boxcox); qqline(model_df$temp_diff_boxcox, col = "red")







######## COMPARE USING AICc ######
# Function to calculate AICc
# Function to calculate AICc
calculate_aicc <- function(fit) {
  # Extract log-likelihood
  loglik <- logLik(fit)
  # Extract number of observations
  n <- lavInspect(fit, "nobs")
  # Extract number of parameters
  k <- fitMeasures(fit, "df")
  
  # Calculate AIC
  aic <- AIC(fit)
  # Calculate AICc
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  return(aicc)
}

# Calculate AICc for each model
aicc1 <- calculate_aicc(fit1)
aicc2 <- calculate_aicc(fit2)
aicc3 <- calculate_aicc(fit3)

# Compare the AICc values
aicc_values <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  AICc = c(aicc1, aicc2, aicc3)
)

print(aicc_values)




########################### VISULIZATION ############################################################################

model_df %>% 
filter(site != "Tippy Dam") %>%
ggplot(aes(x = crash, y = slope)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = FALSE, color = "blue") + # Add logistic regression line
  labs(x = "Crash", y = "Recovery Rate", title = "Effect of Crash on Recovery Rate") +
  theme_minimal()

colors <- c("blue", "white", "red")

model_df %>% 
filter(site != "Tippy Dam") %>%
ggplot(aes(x = crash, y = slope)) +
  geom_point(aes(fill = mean_temp), shape = 21, color = "black", stroke = 0.5, alpha = 0.5, size = 4) +
  scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = FALSE, color = "black") + # Add logistic regression line
  labs(x = "Crash", y = "Recovery Rate", title = "Effect of Crash on Recovery Rate") +
  theme_bw()

ggsave("E:/chapter1_data/figures/final/test.png", width = 6, height=4)

library(mgcv) # For GAM

# Filter and plot using GAM
model_df %>%
  filter(site != "Tippy Dam") %>%
  ggplot(aes(x = crash, y = slope)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, color = "blue") +
  labs(x = "Crash", y = "Recovery Rate", title = "Effect of Crash on Recovery Rate (GAM)") +
  theme_minimal()


# Filter and plot using LOESS
model_df %>%
  filter(site != "Tippy Dam") %>%
  ggplot(aes(x = crash, y = slope)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Crash", y = "Recovery Rate", title = "Effect of Crash on Recovery Rate (LOESS)") +
  theme_minimal()
