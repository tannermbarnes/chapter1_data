rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
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

# Check and see if temp_diff and complexity are colinear
library(car)
mm <- lm(slope ~ temp_diff + complexity, data = df_min_value)
vif_values <- vif(mm)
print(vif_values) # 1 < VIF < 5: moderate correlation
cor_matrix <- cor(df_min_value[, c("temp_diff", "complexity")], use = "complete.obs")
print(cor_matrix)

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


#View(df_min_value)


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

df_min_value %>% ggplot(aes(x=max, y=slope)) +
geom_point(show.legend = FALSE) + 
geom_smooth(method = "lm")

# Convert to normal decimal notation
df_min_value$slope <- format(df_min_value$slope, scientific = FALSE)

# Round the numbers to 3 decimal places
df_min_value$slope <- round(df_min_value$slope, 3)

# Print the result
print(df_min_value$slope)

View(df_min_value)

# GLM Models ##########################################################################################
null_glm <- glm(slope ~ 1, data = df_min_value, family = poisson)
mod1_glm <- glm(slope ~ temp_diff, data = df_min_value, family = poisson)
mod2_glm <- glm(slope ~ mean_rh, data = df_min_value, family = poisson)
mod3_glm <- glm(slope ~ complexity, data = df_min_value, family = poisson)
mod4_glm <- glm(slope ~ temp_diff + complexity, data = df_min_value, family = poisson)
mod5_glm <- glm(slope ~ min, data = df_min_value, family = poisson)
mod6_glm <- glm(slope ~ max, data = df_min_value, family = poisson)
mod7_glm <- glm(slope ~ min + mean_rh, df_min_value, family = poisson)
mod8_glm <- glm(slope ~ max + mean_rh, df_min_value, family = poisson)
mod9_glm <- glm(slope ~ min + mean_rh + complexity, df_min_value, family = poisson)
mod10_glm <- glm(slope ~ max + mean_rh + complexity, df_min_value, family = poisson)
mod11_glm <- glm(slope ~ min + complexity, df_min_value, family = poisson)
mod12_glm <- glm(slope ~ max + complexity, df_min_value, family = poisson)
global_glm <- glm(slope ~ temp_diff + mean_rh + complexity, data = df_min_value, family = poisson)

# Compare models using AIC for Poisson regression
aic_poisson <- AIC(null_glm, mod1_glm, mod2_glm, mod3_glm, mod4_glm, mod5_glm, mod6_glm,
mod7_glm, mod8_glm, mod9_glm, mod10_glm, mod11_glm, mod12_glm, global_glm)
print(aic_poisson)

# Use the Negative Binomial regression for overdispersion ###############################################
# if (!require(MASS)) {
#   install.packages("MASS")
# }

# library(MASS)

null_nb <- glm.nb(slope ~ 1, data = df_min_value)
mod1_nb <- glm.nb(slope ~ temp_diff, df_min_value)
mod2_nb <- glm.nb(slope ~ mean_rh, df_min_value)
mod3_nb <- glm.nb(slope ~ complexity, df_min_value)
mod4_nb <- glm.nb(slope ~ temp_diff + complexity, df_min_value)
mod5_nb <- glm.nb(slope ~ min, df_min_value)
mod6_nb <- glm.nb(slope ~ max, df_min_value)
mod7_nb <- glm.nb(slope ~ min + mean_rh, df_min_value)
mod8_nb <- glm.nb(slope ~ max + mean_rh, df_min_value)
mod9_nb <- glm.nb(slope ~ min + mean_rh + complexity, df_min_value)
mod10_nb <- glm.nb(slope ~ max + mean_rh + complexity, df_min_value)
mod11_nb <- glm.nb(slope ~ min + complexity, df_min_value)
mod12_nb <- glm.nb(slope ~ max + complexity, df_min_value)
global_nb <- glm.nb(slope ~ temp_diff + mean_rh + complexity, data = df_min_value)

# Compare models using AIC for Negative Binomial regression
aic_nb <- AIC(null_nb, mod1_nb, mod2_nb, mod3_nb, mod4_nb, mod5_nb, mod6_nb, 
mod7_nb, mod8_nb, mod9_nb, mod10_nb, mod11_nb, mod12_nb, global_nb)
print(aic_nb)


# Log transform recovery rates
df_min_value$slope_log <- log(df_min_value$slope + 1) # Adding 1 to avoid log(0)

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
