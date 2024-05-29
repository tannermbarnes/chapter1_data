rm(list = ls())
setwd("E:/chapter1_data/code")
source("subset.R")

library(lme4)
library(car)
library(lmtest)

# Change water to a factor
model_this_data1$standing_water <- as.factor(model_this_data1$water)
model_this_data1$levels <- as.factor(model_this_data1$levels)
model_this_data1$shafts <- as.factor(model_this_data1$shafts)

# Create a variable for mine complexity
#View(model_this_data)

model_with_complexity <- model_this_data1 %>% 
mutate(levels = ifelse(is.na(levels), 1, levels)) %>% 
mutate(shafts = ifelse(is.na(shafts), 1, shafts)) %>% 
mutate(complexity = case_when(
    passage_length > 200 & shafts >= 2 & levels == 1 ~ 3,
    passage_length > 200 & shafts >= 1 & levels >= 2 ~ 4,
    passage_length <= 200 & shafts >= 1 & levels >= 1 ~ 2,
    TRUE ~ 1  # Default to 1 if no other conditions are met
  )
)

# Cannot compare models because some mines have NA values for RH which means the model2 that only includes
# mean_rh will have a different size dataframe then the models that have all the variables
# so below I removed those mines so we can compare the models
# we can add them later once we get relative humidity data for those mines or a different way I am not sure yet

model_with_complexity1 <- model_with_complexity %>% 
subset(mean_rh >= 0)

null <- lm(slope ~ 1, data = model_with_complexity1)
model1 <- lm(slope ~ temp_diff, model_with_complexity1)
model2 <- lm(slope ~ mean_rh, model_with_complexity1)
model3 <- lm(slope ~ complexity, model_with_complexity1)
model4 <- lm(slope ~ temp_diff + complexity, model_with_complexity1)
global <- lm(slope ~ temp_diff + mean_rh + complexity, data = model_with_complexity1)

model_list <- list(null, model1, model2, model3, model4, global)
aic_values <- sapply(model_list, AIC)
bic_values <- sapply(model_list, BIC)
comparison_table <- data.frame(
  Model = c("null", "model1", "model2", "model3", "model4", "global"),
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
names(anova_results) <- c("null vs model1", "null vs model2", "null vs model3", "model1 vs model4", "model4 vs global")
print(anova_results)

# Extract and compare adjusted R-squared values
adj_r_squared <- sapply(model_list, function(model) summary(model)$adj.r.squared)
adj_r_squared_table <- data.frame(
  Model = c("null", "model1", "model2", "model3", "model4", "global"),
  Adjusted_R_Squared = adj_r_squared
)
print(adj_r_squared_table)

