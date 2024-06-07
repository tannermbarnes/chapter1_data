rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
source("min_year.R")
#View(model_this_data1)
library(lme4)
library(car)
library(lmtest)

# Change water to a factor
model_this_data2$standing_water <- as.factor(model_this_data2$water)
model_this_data2$levels <- as.factor(model_this_data2$levels)
model_this_data2$shafts <- as.factor(model_this_data2$shafts)

# Create a variable for mine complexity
#View(model_this_data)

model_with_complexity <- model_this_data2 %>% 
mutate(levels = ifelse(is.na(levels), 1, levels)) %>% 
mutate(shafts = ifelse(is.na(shafts), 1, shafts)) %>% 
mutate(complexity = case_when(
    passage_length > 200 & shafts >= 2 & levels == 1 ~ 3,
    passage_length > 200 & shafts >= 1 & levels >= 2 ~ 4,
    passage_length <= 200 & shafts >= 1 & levels >= 1 ~ 2,
    TRUE ~ 1  # Default to 1 if no other conditions are met
  )
)


# remove collin's adit because max is 1 and min is 0 not a hibernacula
mines_to_remove <- c("Collin's Adit")

df_min_value <- model_with_complexity %>% 
filter(!site %in% mines_to_remove)

# Add crash intensity
df_min_value$crash <- 1 - (df_min_value$min_count/df_min_value$max_count)
df_min_value$log_max_count <- log(df_min_value$max_count)

colors <- c("blue", "white", "red")

df_min_value %>%
ggplot(aes(x= min, y = slope)) +
geom_point(aes(color = crash), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does min temperature affect slope",
x = "Minimum temperature",
y = "Slope") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

ggsave("E:/chapter1_data/figures/min_to_slope.png", width = 6, height=4)

df_min_value %>%
ggplot(aes(x= mean_temp, y = slope)) +
geom_point(aes(color = crash), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does mean temperature affect slope",
x = "Mean temperature",
y = "Slope") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

df_min_value %>%
ggplot(aes(x= mode_temp, y = slope)) +
geom_point(aes(color = crash), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does mode temperature affect slope",
x = "Mode temperature",
y = "Slope") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

df_min_value %>%
ggplot(aes(x= max, y = slope)) +
geom_point(aes(color = crash), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does max temperature affect slope",
x = "Max temperature",
y = "Slope") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

df_min_value %>%
ggplot(aes(x= median_temp, y = slope)) +
geom_point(aes(color = crash), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does median temperature affect slope",
x = "Median temperature",
y = "Slope") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

df_min_value %>%
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(color = mean_temp), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does crash intensity and mean temperature affect slope",
x = "Crash intensity",
y = "Slope") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

df_min_value %>%
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(color = min), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does crash intensity and min temperature affect slope",
x = "Crash intensity",
y = "Slope",
color = "Minimum \nTemperature") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1),
  legend.position = c(0.95,0.95),
  legend.justification = c(1,1),
  legend.text = element_text(size = 6),
  legend.key.size = unit(0.3, "cm")
)

ggsave("E:/chapter1_data/figures/crash_intensity-color-min-temp.png", width = 6, height=4)
