rm(list = ls())
setwd("E:/chapter1_data/code")
source("min_year.R")
#View(model_this_data1)
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


mines_to_remove <- c("Aztec East Adit", "Aztec Upper Drift", "B-95 (cave)", "Collin's Adit", 
"Copper Peak Adit", "County Line Adit", "Glen Adit #3", "Hilton Ohio (Hilton #5 Adit)", 
"Lafayette East Adit", "Ohio Traprock #61", "Rockport Quarry South Tunnel", "Scott Falls Cave",
"Silas Doty Cave", "Spider Cave", "Vivian Adit")

# Cannot compare models because some mines have NA values for RH which means the model2 that only includes
# mean_rh will have a different size dataframe then the models that have all the variables
# so below I removed those mines so we can compare the models
# we can add them later once we get relative humidity data for those mines or a different way I am not sure yet
df_min_value <- model_with_complexity %>% 
filter(!site %in% mines_to_remove) %>% 
subset(mean_rh >= 0)

null <- lm(slope ~ 1, data = df_min_value)
mod1 <- lm(slope ~ temp_diff, df_min_value)
mod2 <- lm(slope ~ mean_rh, df_min_value)
mod3 <- lm(slope ~ complexity, df_min_value)
mod4 <- lm(slope ~ temp_diff + complexity, df_min_value)
mod5 <- lm(slope ~ min, df_min_value)
mod6 <- lm(slope ~ max, df_min_value)
mod7 <- lm(slope ~ min + mean_rh, df_min_value)
mod8 <- lm(slope ~ max + mean_rh, df_min_value)
mod9 <- lm(slope ~ min + mean_rh + complexity, df_min_value)
mod10 <- lm(slope ~ max + mean_rh + complexity, df_min_value)
mod11 <- lm(slope ~ min + complexity, df_min_value)
mod12 <- lm(slope ~ max + complexity, df_min_value)
global <- lm(slope ~ temp_diff + mean_rh + complexity, data = df_min_value)

model_list <- list(null, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10,
mod11, mod12, global)
aic_values <- sapply(model_list, AIC)
bic_values <- sapply(model_list, BIC)
comparison_table <- data.frame(
  Model = c("null", "mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9", "mod10",
"mod11", "mod12","global"),
  AIC = aic_values,
  BIC = bic_values
)
print(comparison_table)

# Compare nested models using ANOVA
anova_null_mod1 <- anova(null, mod1)
anova_null_mod3 <- anova(null, mod3)
anova_null_mod4 <- anova(null, mod4)
anova_null_mod5 <- anova(null, mod5)
anova_null_mod7 <- anova(null, mod7)
anova_null_mod9 <- anova(null, mod9)
anova_null_mod11 <- anova(null, mod11)
anova_null_global <- anova(null, global)
anova_mod1_mod4 <- anova(mod1, mod4)
anova_mod1_mod7 <- anova(mod1, mod7)
anova_mod1_global <- anova(mod1, global)
anova_mod4_mod5 <- anova(mod4, mod5)
anova_mod5_mod7 <- anova(mod5, mod7)
anova_mod7_global <- anova(mod7, global)

anova_results <- list(
  anova_null_mod1,
  anova_null_mod3,
  anova_null_mod4,
  anova_null_mod5,
  anova_null_mod7,
  anova_null_mod9,
  anova_null_mod11,
  anova_null_global,
  anova_mod1_mod4,
  anova_mod1_mod7,
  anova_mod1_global,
  anova_mod4_mod5,
  anova_mod5_mod7,
  anova_mod7_global
)
names(anova_results) <- c("null vs mod1", "null vs mod3", "null vs mod4", "null vs mod5", "null vs mod7",
"null vs mod9", "null vs mod11", "null vs global", "mod1 vs mod4", "mod1 vs mod7", 
"mod1 vs global", "mod4 vs mod5", "mod5 vs mod7", "mod7 vs global")
print(anova_results)

# Extract and compare adjusted R-squared values
adj_r_squared <- sapply(model_list, function(model) summary(model)$adj.r.squared)
adj_r_squared_table <- data.frame(
  Model = c("null", "mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8", "mod9", 
  "mod10", "mod11", "mod12", "global"),
  Adjusted_R_Squared = adj_r_squared
)
print(adj_r_squared_table)


View(df_min_value)


coeff <- coefficients(mod5)
intercept <- coeff[1]
slope <- coeff[2]

df_min_value %>%
ggplot(aes(x=min, y=slope)) +
geom_point(show.legend = FALSE, size = 2) +
geom_smooth(method = "lm")

df_min_value %>% ggplot(aes(x=temp_diff, y=slope)) +
geom_point(show.legend = FALSE) + 
geom_smooth(method = "lm")

df_min_value %>% ggplot(aes(x=complexity, y=slope)) +
geom_point(show.legend = FALSE) + 
geom_smooth(method = "lm")

# Convert to normal decimal notation
df_min_value$slope <- format(df_min_value$slope, scientific = FALSE)

# Round the numbers to 3 decimal places
df_min_value$slope <- round(df_min_value$slope, 3)

# Print the result
print(df_min_value$slope)

View(df_min_value)
