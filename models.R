rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_data.R")

View(data_wide2)

model_data <- data_wide2 %>% 
pivot_longer(cols=c("1980", "1981", "1993", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Subset data from the minimum count value for each site and year combination
# Identify the minimum count value for each site
min_counts <- model_data %>% 
group_by(site) %>% 
summarize(min_year = year[which.min(count)], min_count = min(count))

# Merge the minimum counts back to the original dataframe

df_with_min <- model_data %>% 
inner_join(min_counts, by = "site") %>% 
filter(year >= min_year)

# Filter sites with at least 2 years of data after the minimum count year
sites_with_sufficient_data <- df_with_min %>% 
group_by(site) %>% 
filter(n_distinct(year) >=3) %>% 
ungroup()

# Summarize the data to see species recovery over the years
sites_with_sufficient_data %>% filter(year > 2013) %>% filter(count < 20000) %>% 
ggplot(aes(x=year, y=count, group=site, color=site)) +
geom_path(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
theme_classic()


# Try this code to summarize
species_recovery <- sites_with_sufficient_data %>% 
group_by(site, year) %>% 
summarize(total_count = sum(count), .groups = 'drop')

print(species_recovery)

#species_recovery1 <- na.omit(species_recovery)

print(species_recovery)
species_recovery <- species_recovery %>% 
mutate(year = as.numeric(year))

species_recovery %>% filter(year > 2013) %>% filter(total_count < 20000) %>% 
ggplot(aes(x=year, y=total_count, color=site)) + 
geom_line(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
theme_minimal()

#ggsave("E:/chapter1_data/species_recovery_since_min_value.png", width = 5, height=4)


# Fit a regression line for each site and extract the slope
library(broom)
beta_estimate <- species_recovery %>% 
group_by(site) %>% 
filter(n() >=2) %>%  # Ensure there are at least 2 data points
do({
    model <- lm(total_count ~ year, data = .)
    tidy(model) %>% 
    filter(term == "year") %>%
    select(estimate) %>% 
    rename(slope = estimate) 
}) %>% 
ungroup()

beta_estimate

# Merge the beta estiamtes back to the species recovery dataframe
species_recovery_with_beta <- species_recovery %>% 
left_join(beta_estimate, by = "site")

print(species_recovery_with_beta)


# Create a plot with lines for each site and add regression lines
ggplot(species_recovery_with_beta, aes(x=year, y = total_count, color = site)) +
#geom_line(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
geom_smooth(method = "lm", se = FALSE, aes(group = site)) +
labs(title = "Species Recovery Over the Years with Regression Lines",
x = "Year", 
y = "Count by Year") + 
theme_minimal() +
theme(legend.position = "none")

#ggsave("E:/chapter1_data/regression_lines_all.png", width = 10, height=4)

# Filter out the outliers for viewing purposes

species_recovery_with_beta %>% filter(total_count < 19000) %>% filter(year > 2013) %>% 
ggplot(aes(x=year, y=total_count, color=site)) +
#geom_line(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
geom_smooth(method = "lm", se = FALSE, aes(group = site)) +
labs(title = "Species Recovery Over the Years with Regression Lines",
x = "Year", 
y = "Count by Year") + 
theme_classic() +
theme(legend.position = "none")

#ggsave("E:/chapter1_data/regression_filtered.png", width = 5, height=4)

# Pull out species_recovery_with_beta to look at the beta estimates
#file_path <- "E:/chapter1_data/species_recovery_with_beta.csv"
#write.csv(species_recovery_with_beta, file_path, row.names = FALSE)

summarized_slope <- species_recovery_with_beta %>% 
group_by(site) %>% 
summarize(slope = max(slope))
summarized_slope

df_with_slopes <- left_join(data_wide2, summarized_slope %>% select(site, slope), by = "site")
#View(df_with_slopes)
# Remove sites with invalid numbers
df_with_slopes1 <- df_with_slopes[-c(15, 170), ]

#View(df_with_slopes)
#file_path <- "E:/chapter1_data/species_recovery_with_beta.csv"
#write.csv(df_with_slopes, file_path, row.names = FALSE)

# Model using slope as dependent (response) variable and the temp difference as independent (explantory) variable

# Remove any sites with NA for slope because they didn't have enough data
no_na_sites <- subset(df_with_slopes1, !is.na(slope))
#View(no_na_sites)


############################# Linear Models ######################################################################
model1 <- lm(slope ~ temp_diff, data = no_na_sites)
summary(model1)

model2 <- lm(slope ~ min, data = no_na_sites)
summary(model2)

model3 <- lm(slope ~ max, data = no_na_sites)
summary(model3)

# Sites with relative humidity data = 46 mines
df_with_rh <- subset(no_na_sites, mean_rh > 0)
model4 <- lm(slope ~ mean_rh, data = df_with_rh)
summary(model4)
# I want to see what this dataset looks like
# Years aren't in order
rh_long <- df_with_rh %>% 
pivot_longer(cols=c("1980", "1981", "1993", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count") %>% 
ggplot(aes(x = year, y = count, color = site)) +
geom_point(show.legend = FALSE) +
geom_smooth(method = "lm", se = FALSE, aes(group = site)) +
labs(title = "Species Recovery Over the Years with Regression Lines",
x = "Year", 
y = "Count by Year") + 
theme_minimal() +
theme(legend.position = "none")

rh_long

# Need to convert character variables into factor data we can model
# For example, copper needs to be 1, iron 2, concrete 3, etc. 
model5 <- lm(slope ~ ore, data = no_na_sites)
summary(model5)

model6 <- lm(slope ~ water, data = no_na_sites)
summary(model6)




########################## Exploring Multicollinearity ##############################################################
#install.packages("car")
library(car)
explantory_variables <- no_na_sites %>% 
select(min, max, temp_diff, levels, shafts, mean_rh) %>% 
mutate(levels = as.numeric(levels))

cor_matrix <- cor(explantory_variables)
print(cor_matrix)

model7 <- lm(slope ~ temp_diff + mean_rh, data = df_with_rh)
# Calculate VIF; 
# VIF = 1: No multicollinearity
# 1 < VIF < 5: Moderate multicollinearity 
# VIF >= 5: High multicollinearity
vif_values <- vif(model7)
print(vif_values)

# Scale the data and calculate condition number 
scaled_data <- scale(df_with_rh)
