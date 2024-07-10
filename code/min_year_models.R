rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
source("df_min_value.R")
#View(model_this_data1)
library(lme4)
library(car)
library(lmtest)
library(lavaan)

df_min_value$log_passage <- log(df_min_value$passage_length)

#Path anaylsis
df_to_model <- df_min_value %>% 
select(slope, standing_water, complexity, passage_length, levels, shafts, 
crash, min, max, median_temp, mean_temp, temp_diff, ore, log_passage)


# Define the path model
df_to_model$ore <- as.factor(df_to_model$ore)
df_to_model$levels <- as.numeric(df_to_model$levels)
df_to_model$shafts <- as.numeric(df_to_model$shafts)
df_to_model$complexity <- as.numeric(df_to_model$complexity)
df_to_model$standing_water[25] <- "yes"
df_to_model$standing_water[is.na(df_to_model$standing_water)] <- "no"

#View(df_min_value)

# Most important component of SEM is covariance 
#cov(df_min_value) # create covariance matrix with variables we are using
# positive numbers positive association, negative numbers negative assocation
# the purpose of SEM is to reproduce the variance-covariance matrix using parameters
# latent variable - a variable that is constructed and does not exist in the data
# exogenous variable - a variable either observed (x) or latent that explains an endogenous variable
# endongenous variale - a variable either observed (y) or latent that has a causal path leading to it
# measurement model - a model that links observed variables with latent variables
# ~ predict
# =~ indicator for measurement model
# ~~ covariance
# interactions use enmeans (decomposing interactions R)
# colinearity is accounted for by the covariance of the SEM model
# modindices() add variables one by one modification index
# when you add a parameter you decrease a degree of freedom
# Residual covariance the closer to 0. The better fit of the model
# CFI = comparative fit index = models greater than 0.9, conservatively 0.95 indicate good fit
# TLI = values greater than 0.9 indicating good fit. CFI is always greater than TLI
# RMSEA <= 0.05 close-fit, >= 0.10 poor fit, between 0.5 & 0.8 reasonable approximate fit does not compare to baseline model
# The baseline model is the worst fitting model assumes no covariances between variables wanna compare our model to baseline model
# Saturated model is best model df = 0
# Your model is somewhere between baseline and saturated model
# IS THIS PATH ANALYSIS BECAUSE CRASH AND SLOPE ARE ACTUALLY OBSERVED VARIABLES
# latent exogenous variables do not have covariance


# Convert factors to dummy variables
df_min_value_numeric <- model.matrix(~ . - 1, data = df_to_model)

# Convert the matrix back to a dataframe
df_min_value_numeric <- as.data.frame(df_min_value_numeric)

cov(df_min_value_numeric)
# Standardize the numeric columns
df_min_value_scaled <- as.data.frame(scale(df_min_value_numeric))

# Define your models
# Define the measurement model for complexity
# Use complexity as a latent (unobserved variable in the model)
# PATH ANLYSIS MODEL the STRUCTURAL MODEL matrix mutlivariate models
measurement_model <- '
  # Latent variable definition
  complex =~ passage_length + levels + shafts

  # Structural model
  crash ~ standing_wateryes + complex
  slope ~ standing_wateryes + complex + crash
'

# Fit the model to your data
fit <- sem(measurement_model, data = df_min_value_numeric)

# Summarize the results
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Inspect the variances of the variables
varTable(fit)

min.model <- '
complex =~ passage_length + levels + shafts
crash ~ min + complex + standing_wateryes + log_passage
slope ~ min + crash + standing_wateryes + complex + log_passage
'
max.model <- '
complex =~ passage_length + levels + shafts
crash ~ max + complex + standing_wateryes + log_passage
slope ~ max + crash + standing_wateryes + complex + log_passage
'

mean.model <- '
complex =~ passage_length + levels + shafts
crash ~ mean_temp + complex + standing_wateryes + log_passage
slope ~ mean_temp + crash + standing_wateryes + log_passage + complex
'

fit.m1d <- sem(min.model, data = df_min_value_numeric)
fit.m2d <- sem(max.model, data = df_min_value_numeric)
fit.m3d <- sem(mean.model, data = df_min_value_numeric)
summary(fit.m1d, fit.measures = TRUE)
summary(fit.m2d, fit.measures = TRUE)
summary(fit.m3d, fit.measures = TRUE)


#anova(fit.m1d, fit.m2d, fit.m3d) # models have same degrees of freedom so cannot compare
modindices(fit.m1d, sort = TRUE) #1 degree of freedom test, mi = modification index (want around t-stat), epc = how much you expect the parameter to change
modindices(fit.m2d, sort = TRUE)
modindices(fit.m3d, sort = TRUE)

# Add mean_temp
mean.model2 <- '
crash ~ mean_temp + complexity + standing_wateryes
slope ~ crash + standing_wateryes + mean_temp
'
fit.m4d <- sem(mean.model2, data = df_min_value_numeric)
summary(fit.m4d, fit.measures = TRUE)
anova(fit.m3d, fit.m4d)

# Add complexity
complex.model <- '
crash ~ mean_temp + complexity + standing_wateryes
slope ~ crash + standing_wateryes + mean_temp + complexity
'
fit.m5d <- sem(complex.model, data = df_min_value_numeric)
summary(fit.m5d, fit.measures = TRUE)
anova(fit.m5d, fit.m3d)

temp_diff.model <- '
crash ~ temp_diff + complexity + standing_wateryes
slope ~ crash
'
fit.m6d <- sem(temp_diff.model, data = df_min_value_numeric)
summary(fit.m6d, fit.measures = TRUE)

test.model <- '
crash ~ min + max + complexity + standing_wateryes
slope ~ crash + min + max
'
fit.m7d <- sem(test.model, data = df_min_value_numeric)
summary(fit.m7d, fit.measures = TRUE)


# Fit the models to the standardized data
fit.path1 <- sem(complexity_model, data = df_min_value_numeric)
fit.path2 <- sem(climate_model, data = df_min_value_numeric)
fit.path3 <- sem(complete_model, data = df_min_value_numeric)

# Compare the models
anova(fit.path1, fit.path2, fit.path3)

# Summarize the results
summary(fit.path1, fit.measures = TRUE, standardized = TRUE)
summary(fit.path2, fit.measures = TRUE, standardized = TRUE)
summary(fit.path3, fit.measures = TRUE, standardized = TRUE)

# Inspect the variances of the variables in one of the fits
varTable(fit.path2)
#anova(fit.m1d, fit.m2d, fit.m3d)
modindices(fit.m1d, sort = TRUE) #1 degree of freedom test, mi = modification index (want around t-stat), epc = how much you expect the parameter to change
modindices(fit.m2d, sort = TRUE)






# Check and see if temp_diff and complexity are colinear
library(car)
m1 <- lm(slope ~ temp_diff + complexity, data = df_min_value)
vif_values <- vif(m1)
print(vif_values) # 1 < VIF < 5: moderate correlation
cor_matrix <- cor(df_min_value[, c("temp_diff", "complexity")], use = "complete.obs")
print(cor_matrix)
# Min and max 1 < VIF < 5: moderate correlation
m2 <- lm(slope ~ min + max, data = df_min_value)
vif_values <- vif(m2)
print(vif_values)

# Check and see if log_min and log_max are colinear 
log_min_max_ <- lm(slope ~ log_min_value + log_max_value, data = df_min_value)
vif(log_min_max_) # log_min and log_max 1 < VIF < 5: moderate correlation

best_model <- lm(slope ~ crash, df_min_value)
summary(best_model)

# Linear Models
null <- lm(slope ~ 1, data = df_min_value)
temp_diff_ <- lm(slope ~ temp_diff, df_min_value)
meanrh_ <- lm(slope ~ mean_rh, df_min_value)
complexity_ <- lm(slope ~ complexity, df_min_value)
temp_diff_complexity_ <- lm(slope ~ temp_diff + complexity, df_min_value)
min_ <- lm(slope ~ min, df_min_value)
max_ <- lm(slope ~ max, df_min_value)
min_max_ <- lm(slope ~ min + max, data = df_min_value)
min_meanrh_ <- lm(slope ~ min + mean_rh, df_min_value)
max_meanrh_ <- lm(slope ~ max + mean_rh, df_min_value)
min_meanrh_complex_ <- lm(slope ~ min + mean_rh + complexity, df_min_value)
max_meanrh_complex_ <- lm(slope ~ max + mean_rh + complexity, df_min_value)
min_complex_ <- lm(slope ~ min + complexity, df_min_value)
max_complex_ <- lm(slope ~ max + complexity, df_min_value)
temp_diff_meanrh_complex_ <- lm(slope ~ temp_diff + mean_rh + complexity, data = df_min_value)
log_min_ <- lm(slope ~ log_min_value, data = df_min_value)
log_max_ <- lm(slope ~ log_max_value, data = df_min_value)
log_min_max_ <- lm(slope ~ log_min_value + log_max_value, data = df_min_value)


model_list <- list(null, temp_diff_, meanrh_, complexity_, temp_diff_complexity_, min_, max_, min_max_,
min_meanrh_, max_meanrh_, min_meanrh_complex_, max_meanrh_complex_, min_complex_, max_complex_, 
temp_diff_meanrh_complex_, log_min_, log_max_, log_min_max_)
aic_values <- sapply(model_list, AIC)
bic_values <- sapply(model_list, BIC)
comparison_table <- data.frame(
  Model = c("null", "temp_diff_", "meanrh_", "complexity_", "temp_diff_complexity_", "min_", "max_", "min_max_",
"min_meanrh_", "max_meanrh_", "min_meanrh_complex_", "max_meanrh_complex_", "min_complex_", "max_complex_", 
"temp_diff_meanrh_complex_", "log_min_", "log_max_", "log_min_max_"),
  AIC = aic_values,
  BIC = bic_values
)
print(comparison_table)

# Corrected AIC values 
library(AICcmodavg)
# List of models
model_list <- list(
  null = null,
  temp_diff_ = temp_diff_,
  meanrh_ = meanrh_,
  complexity_ = complexity_,
  temp_diff_complexity_ = temp_diff_complexity_,
  min_ = min_,
  max_ = max_,
  min_max_ = min_max_,
  min_meanrh_ = min_meanrh_,
  max_meanrh_ = max_meanrh_,
  min_meanrh_complex_ = min_meanrh_complex_,
  max_meanrh_complex_ = max_meanrh_complex_,
  min_complex_ = min_complex_,
  max_complex_ = max_complex_,
  temp_diff_meanrh_complex_ = temp_diff_meanrh_complex_,
  log_min_ = log_min_,
  log_max_ = log_max_,
  log_min_max_ = log_min_max_
)

# Calculate AICc values
aic_values <- sapply(model_list, AIC)
aicc_values <- sapply(model_list, AICc)
bic_values <- sapply(model_list, BIC)

# Create comparison table
comparison_table <- data.frame(
  Model = names(model_list),
  AIC = aic_values,
  AICc = aicc_values,
  BIC = bic_values
)

# Print comparison table
print(comparison_table)

write.csv(comparison_table, file = "E:/chapter1_data/comparison_table.csv", row.names = FALSE)

##################################################################################################################
# Use crash as an offset variable

null <- lm(slope ~ 1, data = df_min_value)
null1 <- lm(slope ~ 1 + offset(crash), data = df_min_value)
temp_diff_ <- lm(slope ~ temp_diff + offset(crash), df_min_value)
meanrh_ <- lm(slope ~ mean_rh + offset(crash), df_min_value)
complexity_ <- lm(slope ~ complexity + offset(crash), df_min_value)
temp_diff_complexity_ <- lm(slope ~ temp_diff + complexity + offset(crash), df_min_value)
min_ <- lm(slope ~ min + offset(crash), df_min_value)
max_ <- lm(slope ~ max + offset(crash), df_min_value)
min_max_ <- lm(slope ~ min + max + offset(crash), data = df_min_value)
min_meanrh_ <- lm(slope ~ min + mean_rh + offset(crash), df_min_value)
max_meanrh_ <- lm(slope ~ max + mean_rh + offset(crash), df_min_value)
min_meanrh_complex_ <- lm(slope ~ min + mean_rh + complexity + offset(crash), df_min_value)
max_meanrh_complex_ <- lm(slope ~ max + mean_rh + complexity + offset(crash), df_min_value)
min_complex_ <- lm(slope ~ min + complexity + offset(crash), df_min_value)
max_complex_ <- lm(slope ~ max + complexity + offset(crash), df_min_value)
temp_diff_meanrh_complex_ <- lm(slope ~ temp_diff + mean_rh + complexity + offset(crash), data = df_min_value)
log_min_ <- lm(slope ~ log_min_value + offset(crash), data = df_min_value)
log_max_ <- lm(slope ~ log_max_value + offset(crash), data = df_min_value)
log_min_max_ <- lm(slope ~ log_min_value + log_max_value + offset(crash), data = df_min_value)
log_min_complex <- lm(slope ~ log_min_value + complexity + offset(crash), df_min_value)

model_list <- list(null, null1, temp_diff_, meanrh_, complexity_, temp_diff_complexity_, min_, max_, min_max_,
min_meanrh_, max_meanrh_, min_meanrh_complex_, max_meanrh_complex_, min_complex_, max_complex_, 
temp_diff_meanrh_complex_, log_min_, log_max_, log_min_max_, log_min_complex)
aic_values <- sapply(model_list, AIC)
bic_values <- sapply(model_list, BIC)
comparison_table <- data.frame(
  Model = c("null", "null1", "temp_diff_", "meanrh_", "complexity_", "temp_diff_complexity_", "min_", "max_", "min_max_",
"min_meanrh_", "max_meanrh_", "min_meanrh_complex_", "max_meanrh_complex_", "min_complex_", "max_complex_", 
"temp_diff_meanrh_complex_", "log_min_", "log_max_", "log_min_max_", "log_min_complex"),
  AIC = aic_values,
  BIC = bic_values
)
print(comparison_table)

# Corrected AIC values 
library(AICcmodavg)
# List of models
model_list <- list(
  null1 = null1,
  temp_diff_ = temp_diff_,
  meanrh_ = meanrh_,
  complexity_ = complexity_,
  temp_diff_complexity_ = temp_diff_complexity_,
  min_ = min_,
  max_ = max_,
  min_max_ = min_max_,
  min_meanrh_ = min_meanrh_,
  max_meanrh_ = max_meanrh_,
  min_meanrh_complex_ = min_meanrh_complex_,
  max_meanrh_complex_ = max_meanrh_complex_,
  min_complex_ = min_complex_,
  max_complex_ = max_complex_,
  temp_diff_meanrh_complex_ = temp_diff_meanrh_complex_,
  log_min_ = log_min_,
  log_max_ = log_max_,
  log_min_max_ = log_min_max_,
  log_min_complex = log_min_complex
)

# Calculate AICc values
aic_values <- sapply(model_list, AIC)
aicc_values <- sapply(model_list, AICc)
bic_values <- sapply(model_list, BIC)

# Create comparison table
comparison_table <- data.frame(
  Model = names(model_list),
  AIC = aic_values,
  AICc = aicc_values,
  BIC = bic_values
)

# Print comparison table
print(comparison_table)

if (!require(MuMIn)) install.packages("MuMIn")
library(MuMIn)

model_avg <- model.avg(model_list)
summary(model_avg)

model.sel(model_list)



# Average competitive models
aicc_values <- sapply(model_list, AICc)

# Identify models that perform better than the null model
better_models <- model_list[which(aicc_values < aicc_values[1])]

# Perform model averaging 
mod_avg_min <- modavg(better_models, parm = "min")
mod_avg_tempdiff <- modavg(better_models, parm = "temp_diff")
mod_avg_complexity <- modavg(better_models, parm = "complexity")
mod_avg_logmin <- modavg(better_models, parm = "log_min_value")

# Function to extract model-averaged estimates and their statistics
extract_avg_results <- function(mod_avg) {
  data.frame(
    parameter = mod_avg$Parameter,
    estimate = mod_avg$Mod.avg.beta,
    SE = mod_avg$Uncond.SE,
    Lower.CL = mod_avg$Lower.CL,
    Upper.CL = mod_avg$Upper.CL
  )
}

#create summary tables for each parameter
avg_results_min <- extract_avg_results(mod_avg_min)
avg_results_tempdiff <- extract_avg_results(mod_avg_tempdiff)
avg_results_complex <- extract_avg_results(mod_avg_complexity)
avg_results_logmin <- extract_avg_results(mod_avg_logmin)

#combine the results into a single table
avg_results <- rbind(avg_results_min, avg_results_tempdiff, avg_results_complex, avg_results_logmin)

print(avg_results)

# Compare nested models to minimum temperature lm
anova_min_td <- anova(min_, temp_diff_)
anova_min_td_c <- anova(min_, temp_diff_complexity_)
anova_min_min_max <- anova(min_, min_max_)
anova_min_min_rh <- anova(min_, min_meanrh_)
anova_min_min_c <- anova(min_, min_complex_)
anova_min_td_rh_c <- anova(min_, temp_diff_meanrh_complex_)
anova_min_logminmax <- anova(min_, log_min_max_)

anova_results <- list(
  anova_min_td_c,
  anova_min_min_max,
  anova_min_min_rh,
  anova_min_min_c,
  anova_min_td_rh_c,
  anova_min_logminmax
)

print(anova_results)

# Extract and compare adjusted R-squared values
adj_r_squared <- sapply(model_list, function(model) summary(model)$adj.r.squared)
adj_r_squared_table <- data.frame(
  Model = c("null", "mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9", 
  "mod10", "mod11", "mod12", "global"),
  Adjusted_R_Squared = adj_r_squared
)
print(adj_r_squared_table)

# Convert to normal decimal notation
df_min_value$slope <- format(df_min_value$slope, scientific = FALSE)

# Round the numbers to 3 decimal places
df_min_value$slope <- round(df_min_value$slope, 3)

# Print the result
print(df_min_value$slope)

# Check normality after transformation if p-value < 0.5 data is not normally distributed
shapiro.test(df_min_value$slope_log)
# Plot histogram and QQ-plot of transformation
hist(df_min_value$slope_log, main = "Histogram of Log-Transformed Recovery Rates", xlab = "Log-Recovery Rate (Slope)")

# cube root transformation
df_min_value$slope_cubert <- sign(df_min_value$slope) * abs(df_min_value$slope)^(1/3)
shapiro.test(df_min_value$slope_cubert)
hist(df_min_value$slope_cubert, main = "Histogram of cube root transformed recovery rates", xlab = "Recovery rates (slope) ^ (1/3)")

model <- lm(slope_cubert ~ min + complexity + mean_rh, data = df_min_value)
summary(model)


############################################################################################################
# Predicted vs residuals and visulaization
predicted_values <- predict(model_log_min_max)
residuals <- residuals(model_log_min_max)
plot_data <- data.frame(predicted = predicted_values, residuals = residuals)
# Graph predicted vs residuals
ggplot(plot_data, aes(x=predicted, y=residuals)) +
geom_point() +
geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
labs(title = "Predicted vs Residuals results not normal based on shapiro test", x = "Predicted Values", y = "Residuals") +
theme_bw()

ggsave("E:/chapter1_data/figures/min_max_predicted_residuals.png", width = 6, height=4)
# Test for normality of residuals 
qqnorm(residuals)
qqline(residuals, col = "red")

# Shapiro-Wilk test for normality
shapiro_test_result <- shapiro.test(residuals)
print(shapiro_test_result)

colnames(df_min_value)
View(df_min_value)

# Linear Models
null <- lm(slope ~ 1, data = df_min_value)
crash_ <- lm(slope ~ crash, data = df_min_value)
crash_mintemp <- lm(slope ~ crash + min, df_min_value)
crash_complex <- lm(slope ~ crash + complexity, data = df_min_value)
crash_mintemp_complex <- lm(slope ~ crash + min + complexity, df_min_value)
crash_logmin <- lm(slope ~ crash + log_min_value, df_min_value)
crash_maxcount <- lm(slope ~ crash + max_count, df_min_value)
crash_mincount <- lm(slope ~ crash + min_count, df_min_value)
crash_lastcount <- lm(slope ~ crash + last_count, df_min_value)


model_list1 <- list(null, crash_, crash_mintemp, crash_complex, crash_mintemp_complex, 
crash_logmin, crash_maxcount, crash_mincount, crash_lastcount)
aic_values <- sapply(model_list1, AIC)
bic_values <- sapply(model_list1, BIC)
comparison_table1 <- data.frame(
  Model = c("null", "crash_", "crash_mintemp", "crash_complex", 
  "crash_mintemp_complex", "crash_logmin", "crash_maxcount", "crash_mincount", "crash_lastcount"),
  AIC = aic_values,
  BIC = bic_values
)
print(comparison_table1)

# Extract and compare adjusted R-squared values
adj_r_squared <- sapply(model_list1, function(model) summary(model)$adj.r.squared)
adj_r_squared_table <- data.frame(
  Model = c("null", "crash_", "crash_mintemp", "crash_complex", 
  "crash_mintemp_complex", "crash_logmin", "crash_maxcount", "crash_mincount", "crash_lastcount"),
  Adjusted_R_Squared = adj_r_squared
)
print(adj_r_squared_table)


# Compare nested models to minimum temperature lm
anova_crash_min <- anova(crash_, crash_mintemp)
anova_crash_complex <- anova(crash_, crash_complex)
anova_crash_min_complex <- anova(crash_, crash_mintemp_complex)
anova_crash_logmin <- anova(crash_, crash_logmin)
anova_crash_maxcount <- anova(crash_, crash_maxcount)
anova_crash_mincount <- anova(crash_, crash_mincount)
anova_crash_lastcount <- anova(crash_, crash_lastcount)

anova_results <- list(
  anova_crash_min,
  anova_crash_complex,
  anova_crash_min_complex,
  anova_crash_logmin, 
  anova_crash_maxcount, 
  anova_crash_mincount,
  anova_crash_lastcount
)

print(anova_results)

