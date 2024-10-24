# THIS CODE IS FOR MORE DIRECT MODELS USING COUNTS AS RESPONSE VARIABLE 
rm(list = ls())
setwd("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/code")
##################################### Models for mines on the sliding scale method ###################################
source("sliding_scale.R")

counts <- model_data1 %>% filter(count > 0) %>% filter(relative_year >= 0) %>% 
filter(site %in% c("Adventure Adit/ Rock Fall", "Adventure Mine", "Child's Adit", "Delaware Mine", "Derby Adit", 
"Flintsteel Adit", "Glen Adit #1", "Iron Mountain Iron Mine (Tourist Mine)", "Jones' Adit", "Keel Ridge Mine", 
"Keel Ridge Shaft", "Lafayette East Shaft", "Mass C Adit", "Mead Adit of Carp Lake Mine", "Merchant Mine", 
"North Cliff Mine (Shaft #3?)", "Norway Mine", "Ohio Traprock Mine #60 (Norwich Adit)", 
"Old Flintsteel River Adit B", "Quincy Mine Adit", "Quinnesec Adit", "Silver Mountain Mine", "South Bluff Adit", 
"South Bluff East Adit", "South Lake Mine", "Taylor Adit", "Windsor Shaft #3", "Young's Adit", "the belt mine"))

colnames(counts)
linear_model <- lm(count ~ relative_year * mean_temp, data = counts)
summary(linear_model)

exp_model <- lm(log(count) ~ relative_year * mean_temp, data = counts)
summary(exp_model)

poly_model <- lm(count ~ poly(relative_year, 2) * mean_temp, data = counts)
summary(poly_model)

library(ggplot2)

# Create predicted values for each model
counts$linear_pred <- predict(linear_model)
counts$exp_pred <- exp(predict(exp_model))  # For the exponential model, take the exponent of the predicted values
counts$poly_pred <- predict(poly_model)

# Plot observed data with predictions from each model
ggplot(counts, aes(x = mean_temp, y = count)) +
  geom_point(aes(color = "Observed"), size = 2, alpha = 0.6) +  # Observed data
  #geom_line(aes(y = linear_pred, color = "Linear Model"), size = 1) +  # Linear model predictions
  #geom_line(aes(y = exp_pred, color = "Exponential Model"), size = 1, linetype = "dashed") +  # Exponential model predictions
  #geom_line(aes(y = poly_pred, color = "Polynomial Model"), size = 1, linetype = "dotted") +  # Polynomial model predictions
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Observed Count and Model Predictions",
       x = "Mean Temperature (C)",
       y = "Count",
       color = "Model") +
  scale_color_manual(values = c("Observed" = "black", 
                                "Linear Model" = "blue", 
                                "Exponential Model" = "red", 
                                "Polynomial Model" = "green")) + 
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/mean_temp_count.png", width = 8, height=6)


library(readxl)
df <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx")

nest <- df %>% filter(year >= decrease_year) %>% select(site, count, relative_year) %>% group_by(site) %>% nest()

# STEP 2: Fit a linear regression model for each site
nested_data <- nest %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(relative_year))
    # Fit the linear model
    lm(count ~ year, data = df)
  }))

# Step 3: Tidy the model outputs
tidied_data <- nested_data %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# Step 4: Select the slope and intercept for each site and combine them properly
regression_results <- tidied_data %>%
  filter(term %in% c("year", "(Intercept)")) %>%  # Keep both slope and intercept terms
  pivot_wider(names_from = term, values_from = estimate) %>%  # Spread terms into columns
  select(site, intercept = `(Intercept)`, slope = year)  # Select and rename the columns

# Fix NA values by filling them appropriately
regression_results <- regression_results %>%
  group_by(site) %>%
  summarize(
    intercept = max(intercept, na.rm = TRUE),
    slope = max(slope, na.rm = TRUE)
  ) %>%
  ungroup()

regression_results <- regression_results %>% 
mutate(slope = ifelse(is.na(slope), 0, slope),
intercept = ifelse(is.na(intercept), 0, intercept))

# Step 6: Add the slope to the original data frame
final_data <- df %>%
  left_join(regression_results, by = "site")

# Create the trend line data using slope and intercept
your_data <- final_data %>%
  mutate(predicted_count = intercept.y + slope.y * relative_year)

colors <- c("blue", "white", "red")

your_data %>%
group_by(site) %>% 
mutate(year = as.numeric(year)) %>% 
filter(!is.na(count)) %>% 
  ggplot(aes(x = year, y = count, group = site)) +
  geom_line(show.legend = FALSE) +
  geom_line(aes(y=predicted_count, color = mean_temp), size = 1.2, show.legend = FALSE) +
  scale_color_gradientn(colors = colors, values = scales::rescale(c(0, 0.5, 1)), name = "Mean\nTemperature") +
  facet_wrap(~site, scales = "free_y") +  # Facet by site with free y-axis scales
  scale_x_continuous(
    breaks = seq(min(your_data$year), max(your_data$year), by = 4),
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
