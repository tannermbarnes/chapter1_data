# THIS CODE PRODUCES DIFFERENT FIGURES BASED ON WHAT WE ARE INTERESTED IN #
rm(list = ls())
setwd("E:/chapter1_data/code")
library(dplyr)
source("df_min_value.R")
#View(model_this_data1)
library(lme4)
library(car)
library(lmtest)

source("sliding_scale.R")
colors <- c("blue", "white", "red")

# Check for Multicolinearity
lim <- lm(slope ~ crash + min, data = df_min_value)
vif_values = vif(lim)
print(vif_values)

crash_by_min <- lm(crash ~ min, data = df_min_value)
summary(crash_by_min)

complexity_colors <- c("1" = "#56B4E9", "2" = "#E69F00", "3" = "#009E73", "4" = "#CC79A7")
df_min_value$complexity <- factor(df_min_value$complexity, levels = 1:4)

df_min_value %>%
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


model_df %>% filter(slope > 0) %>% 
ggplot(aes(x=recovery_years, y = slope)) +
geom_point(aes(fill = mean_temp), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
labs(title = "How does min temp, and complexity affect qualified slope",
x = "minimum temperature",
y = "Qualified recovery (slope x pop crash)") + 
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8)
)

ggsave("E:/chapter1_data/figures/test.png", width = 6, height=4)

df_slide_scale %>%
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(fill = min, size = complexity), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
labs(title = "Three former large hibernacula are recovery slowly",
x = "Population Crash",
y = "Recovery Rate",
fill = "log of \nminimum \ntemperature",
size = "maximum \npopulation\ncount") + 
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

ggsave("E:/chapter1_data/figures/final/test.png", width = 6, height=4)

df_min_value %>%
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(fill = log_min_value, size = complexity), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
labs(title = "How does pop crash, min temp, and complexity affect slope",
x = "Population Crash",
y = "Recovery Rate") + 
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

ggsave("E:/chapter1_data/figures/slope_by_crash_logmin_complex.png", width = 6, height=4)

df_min_value %>%
ggplot(aes(x= crash, y = slope)) +
geom_point(aes(fill = log_min_value, size = max_count), shape = 21, color = "black", stroke = 0.5) +
geom_smooth(method = "lm") +
labs(title = "How does pop crash, min temp, and complexity affect slope",
x = "Population Crash",
y = "Recovery Rate") + 
scale_fill_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1))) +
theme_bw() +
theme(
  plot.title = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 1, lineheight = 0.8),
    panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

ggsave("E:/chapter1_data/figures/slope_by_crash_logmin_maxpop.png", width = 6, height=4)

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


# Define the sites to remove
source("survey_data.R")
sites_to_remove <- c("Algonquin Adit #2 (Mark's Adit)", "Child's Adit", "Cushman Adit", "Eagle River Adit 2 (Lake Superior & Phoenix)",
"Eagle River Adit 3 (Lake Superior & Phoenix)", "Indiana Mine", "Lafayette East Adit", "Lafayette West Adit", 
"North American Adit", "North Belt Mine", "Ohio Traprock Mine #59 (Norwich Adit)", "Rockport Quarry South Tunnel", 
"Scott Falls Cave", "Washburn Mine", "Toltec Mine", "Aztec Mine", "Collin's Adit", "Douglas Houghton Adit", 
"Glen Adit #2", "Glen Adit #3", "Hilton (Shaft 1)", "Hilton Ohio (Hilton #5 Adit)", "Ohio Traprock #61", 
"Ohio Traprock Mine #2", "Ohio Traprock Mine #3", "Ridge Adit", "Randville Quarry Mine", "Merchant's Adit North", 
"Merchant's Adit South")

filtered_data <- dat1 %>%
  filter(!site %in% sites_to_remove) %>%
  group_by(site) %>% 
  mutate(year = as.numeric(year), 
         count = as.numeric(myotis_count)) %>% 
  drop_na(myotis_count)

filtered_data %>%
filter(year > 1997) %>% 
filter(n() > 2) %>% 
  ggplot(aes(x = year, y = count, color = site, group = site)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~site, scales = "free_y") +  # Facet by site with free y-axis scales
  scale_x_continuous(
    breaks = seq(min(filtered_data$year), max(filtered_data$year), by = 4),
    labels = function(x) sprintf("%02d", x %% 100)
  ) +
  geom_vline(xintercept = 2014, color = "red", linetype = "dashed", size = 0.5) +  # Adjust size here
  theme_bw() +
  labs(title = "Count Over Years by Site",
       x = "Year",
       y = "Count")

ggsave("E:/chapter1_data/figures/final/to_show/crash_mines.png", width = 12, height=12)


filtered_data %>%
group_by(site) %>% 
mutate(year = as.numeric(year)) %>% 
filter(!is.na(count)) %>% 
  ggplot(aes(x = year, y = count, color = site, group = site)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~site, scales = "free_y") +  # Facet by site with free y-axis scales
  scale_x_continuous(
    breaks = seq(min(filtered_data$year), max(filtered_data$year), by = 3),
    labels = function(x) sprintf("%02d", x %% 100)
  ) +
  geom_vline(xintercept = 2014, color = "red", linetype = "dashed", size = 0.5) +  # Adjust size here
  theme_bw() +
  labs(title = "Mines used in models because they have recovery from their minimum value",
       x = "Year",
       y = "Population Count")

ggsave("E:/chapter1_data/figures/final/mines_used.png", width = 12, height=12)

##################################################################################################################
########### NORMALIZED COUNT WITH YEAR AND TREND LINES ###############################################

your_data <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx")

# Create the trend line data using slope and intercept
your_data <- your_data %>%
  mutate(predicted_count = intercept + slope * relative_year)

colors <- c("blue", "white", "red")

your_data %>%
group_by(site) %>% 
mutate(year = as.numeric(year)) %>% 
filter(!is.na(count)) %>% 
  ggplot(aes(x = year, y = normalized_count, group = site)) +
  geom_line(show.legend = FALSE) +
  geom_line(aes(y=predicted_count, color = mean_temp), size = 1.2, show.legend = FALSE) +
  scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
  facet_wrap(~site, scales = "free_y") +  # Facet by site with free y-axis scales
  scale_x_continuous(
    breaks = seq(min(your_data$year), max(your_data$year), by = 3),
    labels = function(x) sprintf("%02d", x %% 100)
  ) +
  geom_vline(xintercept = 2014, color = "red", linetype = "dashed", size = 0.5) +  # Adjust size here
  theme_dark() +
  labs(title = "Recovering mines with recovery estimate overlayed the normalized count",
       x = "Year",
       y = "Normalized Population Count")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/trend_line_test.png", width = 12, height=12)

your_data %>%
group_by(site) %>% filter(year >= min_year) %>% 
mutate(year = as.numeric(year)) %>% 
filter(!is.na(count)) %>% 
  ggplot(aes(x = year, y = normalized_count, group = site)) +
  geom_line(show.legend = FALSE) +
  geom_line(aes(y=predicted_count, color = mean_temp), size = 1.2, show.legend = FALSE) +
  scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
  facet_wrap(~site, scales = "free_y") +  # Facet by site with free y-axis scales
  scale_x_continuous(
    breaks = seq(min(your_data$year), max(your_data$year), by = 3),
    labels = function(x) sprintf("%02d", x %% 100)
  ) +
  geom_vline(xintercept = 2014, color = "red", linetype = "dashed", size = 0.5) +  # Adjust size here
  theme_dark() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Recovering mines with recovery estimate overlayed the normalized count",
       x = "Year",
       y = "Normalized Population Count")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/trend_line_test.png", width = 12, height=12)

