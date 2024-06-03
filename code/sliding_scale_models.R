rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")
##################################### Models for mines on the sliding scale method ###################################

source("sliding_scale.R")
library(lme4)
library(car)
#install.packages("lmtest")
library(lmtest)

# Change water to a factor
model_this_data$standing_water <- as.factor(model_this_data$water)
model_this_data$levels <- as.factor(model_this_data$levels)
model_this_data$shafts <- as.factor(model_this_data$shafts)

# Create a variable for mine complexity
#View(model_this_data)

model_with_complexity <- model_this_data %>% 
mutate(levels = ifelse(is.na(levels), 1, levels)) %>% 
mutate(shafts = ifelse(is.na(shafts), 1, shafts)) %>% 
mutate(complexity = case_when(
    passage_length > 200 & shafts >= 2 & levels == 1 ~ 3,
    passage_length > 200 & shafts >= 1 & levels >= 2 ~ 4,
    passage_length <= 200 & shafts >= 1 & levels >= 1 ~ 2,
    TRUE ~ 1  # Default to 1 if no other conditions are met
  )
)

# Remove mines that were never hibernacula 
#View(model_this_data)
mines_to_remove <- c("Collin's Adit", "Douglas Houghton Adit #1", "Glen Adit #3", "Hilton Ohio (Hilton #5 Adit)",
"Lafayette East Adit", "Ohio Traprock #61", "Scott Falls Cave")

df_sliding_scale <- model_with_complexity %>% 
filter(!site %in% mines_to_remove)



# Cannot compare models because some mines have NA values for RH which means the model2 that only includes
# mean_rh will have a different size dataframe then the models that have all the variables
# so below I removed those mines so we can compare the models
# we can add them later once we get relative humidity data for those mines or a different way I am not sure yet

df_sliding_scale <- model_with_complexity %>% 
filter(!site %in% mines_to_remove) %>% 
subset(mean_rh >= 0)

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

