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
df_min_value$log_max_count <- log(df_min_value$max_count)

#Path anaylsis
df_to_model <- df_min_value %>% 
select(slope, standing_water, complexity, passage_length, levels, shafts, 
crash, min, max, median_temp, mean_temp, temp_diff, ore, log_passage, log_max_count)


# Define the path model
df_to_model$ore <- as.numeric(df_to_model$ore)
df_to_model$levels <- as.numeric(df_to_model$levels)
df_to_model$shafts <- as.numeric(df_to_model$shafts)
df_to_model$complexity <- as.numeric(df_to_model$complexity)
df_to_model$standing_water[25] <- "yes"
df_to_model$standing_water[is.na(df_to_model$standing_water)] <- "no"
df_to_model$water_numeric <- as.numeric(df_to_model$standing_water)

# Assuming df_min_value is your data frame
mean_passage_length <- mean(df_to_model$passage_length, na.rm = TRUE)
sd_passage_length <- sd(df_to_model$passage_length, na.rm = TRUE)

# Standardize the passage_length variable
df_to_model$passage_length_standardized <- (df_to_model$passage_length - mean_passage_length) / sd_passage_length

# View the standardized variable
head(df_to_model$passage_length_standardized)

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

combined.model <- '
# latent variable
complex =~ levels + shafts + log_passage + ore + temp_diff
# regresssions
crash ~ complex
slope ~ crash
'

fit.combined <- sem(combined.model, data = df_to_model)
summary(fit.combined, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit.combined, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit.combined, standardized = TRUE)



# Same model minus a variable
minus.model <- '
# latent variable
complex =~ levels + shafts + log_passage + min
# regresssions
crash ~ complex + min
slope ~ crash + min
'

fit.minus <- sem(minus.model, data = df_min_value)
summary(fit.minus, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit.minus, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit.minus, standardized = TRUE)

anova(fit.combined, fit.minus)


# Rank each model using AICc 
model1 <- '
# latent variable
complex =~ levels + shafts + temp_diff
# regressions
crash ~ complex
slope ~ crash
'
model2 <- '
# latent variable
complex =~ levels + shafts
# regressions
crash ~ complex
slope ~ crash + standing_water
'
model3 <- '
# latent variable
complex =~ levels + shafts + temp_diff
# regressions
crash ~ complex
slope ~ crash + standing_water
'
model4 <- '
# latent variable
complex =~ levels + shafts + temp_diff
# regressions
crash ~ complex + standing_water
slope ~ crash
'
model5 <- '
# latent variable
complex =~ levels + shafts + temp_diff
# regressions
crash ~ complex + standing_water
slope ~ crash + min
'
fit1 <- sem(model1, data = df_to_model)
summary(fit1, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit1, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit1, standardized = TRUE)

fit2 <- sem(model2, data = df_to_model)
summary(fit2, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit2, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit2, standardized = TRUE)

fit3<- sem(model3, data = df_to_model)
summary(fit3, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit3, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit3, standardized = TRUE)

fit4 <- sem(model4, data = df_to_model)
summary(fit4, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit4, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit4, standardized = TRUE)

fit5 <- sem(model5, data = df_to_model)
summary(fit5, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit5, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
parameterEstimates(fit5, standardized = TRUE)



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
aicc4 <- calculate_aicc(fit4)
aicc5 <- calculate_aicc(fit5)

# Print AICc values
aicc_values <- c(aicc1, aicc2, aicc3, aicc4, aicc5)
names(aicc_values) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
print(aicc_values)
