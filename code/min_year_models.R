rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
source("df_min_value.R")
#View(model_this_data1)
library(lme4)
library(car)
library(lmtest)

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

# LN of explantory variables to get Pseduothreshold
# Add 1 all min values to avoid log(0)
df_min_value <- df_min_value %>% 
mutate(min_value = min + 1, 
      max_value = max + 1) %>% 
mutate(min_value = ifelse(min_value < 0, 1, min_value))

df_min_value$log_min_value <- log(df_min_value$min_value)
df_min_value$log_max_value <- log(df_min_value$max_value)

# Check and see if log_min and log_max are colinear 
log_min_max_ <- lm(slope ~ log_min_value + log_max_value, data = df_min_value)
vif(log_min_max_) # log_min and log_max 1 < VIF < 5: moderate correlation


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
