rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
source("df_min_value.R")
#View(model_this_data1)
library(lme4)
library(car)
library(lmtest)

df_min_value$qualified_recovery <- df_min_value$slope * df_min_value$crash
View(df_min_value)

df_min_value$normalized_count1 <- df_min_value$last_count / df_min_value$max_count

model <- lm(normalized_count1 ~ complexity + min + crash, df_min_value)
summary(model)

# Check for Multicolinearity
lim <- lm(slope ~ crash + min, data = df_min_value)
vif_values = vif(lim)
print(vif_values)

crash_by_min <- lm(crash ~ min, data = df_min_value)
summary(crash_by_min)

colors <- c("blue", "white", "red")



complexity_colors <- c("1" = "#56B4E9", "2" = "#E69F00", "3" = "#009E73", "4" = "#CC79A7")
df_min_value$complexity <- factor(df_min_value$complexity, levels = 1:4)

df_min_value %>% filter(last_count > 0) %>% 
ggplot(aes(x= complexity, y = crash)) +
geom_point(aes(color = complexity, size = last_count)) +
geom_smooth(method = "lm") +
labs(title = "How does population crash and complexity affect slope",
x = "Population Crash",
y = "Slope of recovery") + 
scale_color_manual(values = complexity_colors) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1)
#   legend.position = c(0.95, 0.95), 
#   legend.justification = c(1,1), 
#   legend.text = element_text(size = 8),
#   legend.key.size = unit(0.5, "cm")
)



df_min_value %>% 
ggplot(aes(x=site, y=max_count)) +
geom_point() +
geom_point(aes(y= last_count))
ggsave("E:/chapter1_data/figures/max-vs-current.png", width = 6, height=4)

df_min_value %>% 
ggplot(aes(x=crash, y = normalized_count1)) +
geom_point(aes(fill = min, size = complexity), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
labs(title = "Pop crash and min temp affect the amount of bats that have recovered",
x = "Population crash",
y = "Percent recovered") + 
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)
ggsave("E:/chapter1_data/figures/normalzied_recovery.png", width = 6, height=4)

df_min_value %>% filter(passage_length < 7000) %>% 
ggplot(aes(x=passage_length, y = qualified_recovery)) +
geom_point(aes(fill = min), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
labs(title = "How does min temp, and complexity affect qualified slope",
x = "minimum temperature",
y = "Qualified recovery (slope x pop crash)") + 
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

ggsave("E:/chapter1_data/figures/q-slope_by_min_length.png", width = 6, height=4)

df_min_value %>%
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(fill = min, size = complexity), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
labs(title = "How does pop crash, min temp, and complexity affect slope",
x = "Population Crash",
y = "Recovery Rate") + 
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

ggsave("E:/chapter1_data/figures/slope_by_crash_min_complex.png", width = 6, height=4)

df_min_value %>%
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(fill = min, size = max_count), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
labs(title = "How does pop crash, min temp, and complexity affect slope",
x = "Population Crash",
y = "Recovery Rate") + 
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

ggsave("E:/chapter1_data/figures/slope_by_crash_min_maxpop.png", width = 6, height=4)

df_min_value %>% filter(last_count < 9000) %>% 
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(fill = min, size = last_count), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
labs(title = "How does pop crash, min temp, and complexity affect slope",
x = "Population Crash",
y = "Recovery Rate") + 
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

ggsave("E:/chapter1_data/figures/slope_by_crash_min_lastcount.png", width = 6, height=4)

df_min_value %>%
ggplot(aes(x= complexity, y = crash)) +
geom_point(aes(color = min), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does complexity and mintemp affect pop crash",
x = "complexity",
y = "Population Crash") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

ggsave("E:/chapter1_data/figures/popcrash_complex_min.png", width = 6, height=4)


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
geom_point(aes(color = mean_temp), size = 2) +
geom_smooth(method = "lm") +
labs(title = "How does crash intensity and mean temperature affect slope",
x = "Crash intensity",
y = "Slope",
color = "Mean \nTemperature") + 
scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1),
  legend.position = c(0.95,0.95),
  legend.justification = c(1,1),
  legend.text = element_text(size = 6),
  legend.key.size = unit(0.3, "cm")
)

ggsave("E:/chapter1_data/figures/crash_intensity-color-mean-temp.png", width = 6, height=4)


df_min_value %>% 
ggplot(aes(x = max_count, y = last_count)) +
geom_point(aes(color = site), show.legend = FALSE)
