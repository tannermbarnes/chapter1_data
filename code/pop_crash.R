rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
source("df_min_value.R")
#View(model_this_data1)
library(lme4)
library(car)
library(lmtest)

null <- lm(crash ~ 1, df_min_value)
crash_mintemp <- lm(crash ~ min, data = df_min_value)
summary(crash_mintemp)
crash_maxtemp <- lm(crash ~ max, data = df_min_value)
summary(crash_maxtemp)
crash_meantemp <- lm(crash ~ mean_temp, data = df_min_value)
summary(crash_meantemp)
crash_mediantemp <- lm(crash ~ median_temp, data = df_min_value)
summary(crash_mediantemp)
crash_complex <- lm(crash ~ complexity, df_min_value)
summary(crash_complex)
crash_maxcount <- lm(crash ~ max_count, df_min_value)
summary(crash_maxcount)
crash_mincount <- lm(crash ~ min_count, df_min_value)
summary(crash_mincount)
crash_logmin <- lm(crash ~ log_min_value, df_min_value)
summary(crash_logmin)
crash_min_complex <- lm(crash ~ min + complexity, df_min_value)
summary(crash_min_comlex)
crash_min_complex_mincount <- lm(crash ~ min + complexity + min_count, df_min_value)
crash_tempdiff <- lm(crash ~ temp_diff, df_min_value)
summary(crash_tempdiff)


anova(null, crash_mintemp)
anova(null, crash_complex)
anova(crash_mintemp, crash_min_comlex)
anova(crash_min_complex, crash_min_complex_mincount)
anova(null, crash_tempdiff)



############################################################################################################
# Predicted vs residuals and visulaization
predicted_values <- predict(crash_mintemp)
residuals <- residuals(crash_mintemp)
plot_data <- data.frame(predicted = predicted_values, residuals = residuals)
# Graph predicted vs residuals
ggplot(plot_data, aes(x=predicted, y=residuals)) +
geom_point() +
geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
labs(title = "Predicted vs Residuals results not normal based on shapiro test", x = "Predicted Values", y = "Residuals") +
theme_bw()
