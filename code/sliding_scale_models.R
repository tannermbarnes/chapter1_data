rm(list = ls())
setwd("E:/chapter1_data/code")
##################################### Models for mines on the sliding scale method ###################################
source("sliding_scale.R")
library(lme4)
library(car)
#install.packages("lmtest")
library(lmtest)

#View(df_slide_scale)

#Path anaylsis
model_df <- df_slide_scale %>% 
mutate(recovery_status = ifelse(bin == 1, "recovering", "not recovering")) %>% 
mutate(recovery_status = as.factor(recovery_status)) %>% 
select(site, bin, recovery_status, slope, standing_water, complexity, passage_length, log_passage, levels, shafts, 
crash, min, max, median_temp, mean_temp, temp_diff, ore)


# Define the path model
model_df$ore <- as.factor(model_df$ore)
model_df$levels <- as.numeric(model_df$levels)
model_df$shafts <- as.numeric(model_df$shafts)
model_df$standing_water[28] <- "yes"
model_df$standing_water[is.na(model_df$standing_water)] <- "no"
# Change row 37 to yes

# CFI = comparative fit index = models greater than 0.9, conservatively 0.95 indicate good fit
# TLI = values greater than 0.9 indicating good fit. CFI is always greater than TLI
# RMSEA <= 0.05 close-fit, >= 0.10 poor fit, between 0.5 & 0.8 reasonable approximate fit does not compare to baseline model
# The baseline model is the worst fitting model assumes no covariances between variables wanna compare our model to baseline model
# Saturated model is best model df = 0
# Your model is somewhere between baseline and saturated model

binary_model <- '
# Latent variable (measurement model)
complex =~ log_passage + levels + shafts + temp_diff
# Regressions
crash ~ complex 
bin ~ crash + mean_temp
'
binary_model2 <- '
# Latent variable (measurement model)
complex =~ log_passage + levels + shafts + temp_diff
# Regressions
crash ~ complex 
bin ~ crash + mean_temp + complex
'

fit <- sem(binary_model2, data = model_df, ordered = "bin")
summary(fit, fit.measures = TRUE)
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit, standardized = TRUE)


binary_model3 <- '
bin ~ crash + mean_temp
'

fit3 <- sem(binary_model3, data = model_df, ordered = "recovery_status")
summary(fit3, fit.measures = TRUE)
fitMeasures(fit3, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit3, standardized = TRUE)
