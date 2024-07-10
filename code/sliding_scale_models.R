rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")
##################################### Models for mines on the sliding scale method ###################################

source("sliding_scale.R")
library(lme4)
library(car)
#install.packages("lmtest")
library(lmtest)

# Create a variable for mine complexity
#View(model_this_data)


# Remove mines that were never hibernacula 
#View(model_this_data)
mines_to_remove <- c("Collin's Adit", "Douglas Houghton Adit #1", "Glen Adit #3", "Hilton Ohio (Hilton #5 Adit)",
"Lafayette East Adit", "Ohio Traprock #61", "Scott Falls Cave")

df_sliding_scale <- model_with_complexity %>% 
filter(!site %in% mines_to_remove)

df_slide_scale$ore <- as.factor(df_slide_scale$ore)

#Path anaylsis
model_df <- df_slide_scale %>% 
select(slope, standing_water, complexity, passage_length, levels, shafts, 
crash, min, max, median_temp, mean_temp, temp_diff, ore)


# Define the path model
model_df$ore <- as.factor(model_df$ore)
model_df$levels <- as.numeric(model_df$levels)
model_df$shafts <- as.numeric(model_df$shafts)
model_df$standing_water[37] <- "yes"
model_df$standing_water[is.na(model_df$standing_water)] <- "no"
# Change row 37 to yes


# Convert factors to dummy variables
model_df_numeric <- model.matrix(~ . - 1, data = model_df)

# Convert the matrix back to a dataframe
model_df_numeric <- as.data.frame(model_df_numeric)

# CFI = comparative fit index = models greater than 0.9, conservatively 0.95 indicate good fit
# TLI = values greater than 0.9 indicating good fit. CFI is always greater than TLI
# RMSEA <= 0.05 close-fit, >= 0.10 poor fit, between 0.5 & 0.8 reasonable approximate fit does not compare to baseline model
# The baseline model is the worst fitting model assumes no covariances between variables wanna compare our model to baseline model
# Saturated model is best model df = 0
# Your model is somewhere between baseline and saturated model



#View(model_df_numeric)

min.model <- '
crash ~ min + complexity + standing_wateryes
slope ~ crash + standing_wateryes
'
max.model <- '
crash ~ max + complexity + standing_wateryes
slope ~ crash + standing_wateryes
'

mean.model <- '
crash ~ mean_temp + complexity + standing_wateryes
slope ~ crash + standing_wateryes
'

fit.m1d <- sem(min.model, data = model_df_numeric)
fit.m2d <- sem(max.model, data = model_df_numeric)
fit.m3d <- sem(mean.model, data = model_df_numeric)
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
fit.m4d <- sem(mean.model2, data = model_df_numeric)
summary(fit.m4d, fit.measures = TRUE)
anova(fit.m3d, fit.m4d)

# Add complexity
complex.model <- '
crash ~ mean_temp + complexity + standing_wateryes
slope ~ crash + standing_wateryes + mean_temp + complexity
'
fit.m5d <- sem(complex.model, data = model_df_numeric)
summary(fit.m5d, fit.measures = TRUE)
anova(fit.m5d, fit.m3d)

temp_diff.model <- '
crash ~ temp_diff + complexity + standing_wateryes
slope ~ crash
'
fit.m6d <- sem(temp_diff.model, data = model_df_numeric)
summary(fit.m6d, fit.measures = TRUE)

test.model <- '
crash ~ min + max + complexity + standing_wateryes
slope ~ crash + min + max
'
fit.m7d <- sem(test.model, data = model_df_numeric)
summary(fit.m7d, fit.measures = TRUE)


# Cannot compare models because some mines have NA values for RH which means the model2 that only includes
# mean_rh will have a different size dataframe then the models that have all the variables
# so below I removed those mines so we can compare the models
# we can add them later once we get relative humidity data for those mines or a different way I am not sure yet

null <- lm(slope ~ 1, data = df_sliding_scale)
model1 <- lm(slope ~ temp_diff, df_sliding_scale)
model2 <- lm(slope ~ mean_rh, df_sliding_scale)
model3 <- lm(slope ~ complexity, df_sliding_scale)
model4 <- lm(slope ~ temp_diff + complexity, df_sliding_scale)
model5 <- lm(slope ~ min, df_sliding_scale) 
model6 <- lm(slope ~ max, df_sliding_scale)
model7 <- lm(slope ~ min + mean_rh, df_sliding_scale)
model8 <- lm(slope ~ max + mean_rh, df_sliding_scale)
model9 <- lm(slope ~ min + mean_rh + complexity, df_sliding_scale)
model10 <- lm(slope ~ max + mean_rh + complexity, df_sliding_scale)
model11 <- lm(slope ~ min + complexity, df_sliding_scale)
model12 <- lm(slope ~ max + complexity, df_sliding_scale)
global <- lm(slope ~ temp_diff + mean_rh + complexity, data = df_sliding_scale)

model_list <- list(null, model1, model2, model3, model4, model5, model6, model7, 
model8, model9, model10, model11, model12, global)
aic_values <- sapply(model_list, AIC)
bic_values <- sapply(model_list, BIC)
comparison_table <- data.frame(
  Model = c("null", "model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8", 
  "model9", "model10", "model11", "model12", "global"),
  AIC = aic_values,
  BIC = bic_values
)
print(comparison_table)

# Compare nested models using ANOVA
anova_null_model1 <- anova(null, model1)
anova_null_model2 <- anova(null, model2)
anova_null_model3 <- anova(null, model3)
anova_model1_model4 <- anova(model1, model4)
anova_model4_global <- anova(model4, global)
anova_results <- list(
  anova_null_model1,
  anova_null_model2,
  anova_null_model3,
  anova_model1_model4,
  anova_model4_global
)
names(anova_results) <- c("null vs model1", "null vs model2", "null vs model3", "model1 vs model4", "model4 vs global,
")
print(anova_results)

# Extract and compare adjusted R-squared values
adj_r_squared <- sapply(model_list, function(model) summary(model)$adj.r.squared)
adj_r_squared_table <- data.frame(
  Model = c("null", "model1", "model2", "model3", "model4", "global"),
  Adjusted_R_Squared = adj_r_squared
)
print(adj_r_squared_table)

############################ Visulalize the models ############################################################
df_sliding_scale %>%
ggplot(aes(x=min, y=slope)) +
geom_point(show.legend = FALSE, size = 2) +
geom_smooth(method = "lm")


df_sliding_scale %>% ggplot(aes(x=temp_diff, y=slope)) +
geom_point(show.legend = FALSE) + 
geom_smooth(method = "lm")

df_sliding_scale %>% ggplot(aes(x=complexity, y=slope)) +
geom_point(show.legend = FALSE) + 
geom_smooth(method = "lm")

df_sliding_scale %>% ggplot(aes(x=max, y=slope)) +
geom_point(show.legend = FALSE) + 
geom_smooth(method = "lm")


# Use the Negative Binomial regression for overdispersion ##########################################################
## Since slope isn't count data the negative binomial doesn't make sense to use
# if (!require(MASS)) {
#   install.packages("MASS")
# }

# library(MASS)

nulls_nb <- glm.nb(slope ~ 1, data = df_sliding_scale)
model1_nb <- glm.nb(slope ~ temp_diff, df_sliding_scale)
model2_nb <- glm.nb(slope ~ mean_rh, df_sliding_scale)
model3_nb <- glm.nb(slope ~ complexity, df_sliding_scale)
model4_nb <- glm.nb(slope ~ temp_diff + complexity, df_sliding_scale)
model5_nb <- glm.nb(slope ~ min, df_sliding_scale)
model6_nb <- glm.nb(slope ~ max, df_sliding_scale)
model7_nb <- glm.nb(slope ~ min + mean_rh, df_sliding_scale)
model8_nb <- glm.nb(slope ~ max + mean_rh, df_sliding_scale)
model9_nb <- glm.nb(slope ~ min + mean_rh + complexity, df_sliding_scale)
model10_nb <- glm.nb(slope ~ max + mean_rh + complexity, df_sliding_scale)
model11_nb <- glm.nb(slope ~ min + complexity, df_sliding_scale)
model12_nb <- glm.nb(slope ~ max + complexity, df_sliding_scale)
globals_nb <- glm.nb(slope ~ temp_diff + mean_rh + complexity, data = df_sliding_scale)

# Compare models using AIC for Negative Binomial regression
aic_nb <- AIC(nulls_nb, model1_nb, model2_nb, model3_nb, model4_nb, model5_nb, model6_nb, 
model7_nb, model8_nb, model9_nb, model10_nb, model11_nb, model12_nb, globals_nb)
print(aic_nb)

# Install and load the lavaan package
#install.packages("lavaan")
library(lavaan)

# Define the path model
model <- '
  # Recovery regression
  slope ~ standing_water + passage_length + levels + shafts
  
  # Slope regressions
  slope ~ min + max + mean_temp + temp_diff + crash
'

# Fit the model to your data
fit <- sem(model, data = df_slide_scale)

# Summarize the results
summary(fit, fit.measures = TRUE, standardized = TRUE)
