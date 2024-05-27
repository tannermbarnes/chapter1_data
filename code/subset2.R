rm(list = ls())
setwd("E:/chapter1_data/code")
source("raw_last.R")

#View(data_wide2x)

model_datax <- data_wide2x %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Subset data from the minimum count value for each site and year combination
# Identify the minimum count value for each site
min_countsx <- model_datax %>% 
group_by(site) %>% 
summarize(min_year = year[which.min(count)], min_count = min(count))


# Merge the minimum counts back to the original dataframe

df_with_minx <- model_datax %>% 
inner_join(min_countsx, by = "site") %>% 
filter(year >= min_year) %>% 
filter(count >= 0)


# Filter sites with at least 2 years of data after the minimum count year
sites_with_sufficient_datax <- df_with_minx %>% 
group_by(site) %>% 
filter(n_distinct(year) >=2) %>% 
ungroup()

#View(sites_with_sufficient_data)
# Summarize the data to see species recovery over the years
sites_with_sufficient_datax %>% filter(year > 2013) %>% 
ggplot(aes(x=year, y=count, group=site, color=site)) +
geom_path(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
theme_classic()


# Try this code to summarize
species_recoveryx <- sites_with_sufficient_datax %>% 
group_by(site, year) %>% 
summarize(total_count = sum(count), .groups = 'drop')

print(species_recoveryx)
#species_recovery1 <- na.omit(species_recovery)

species_recoveryx <- species_recoveryx %>% 
mutate(year = as.numeric(year))

species_recoveryx %>% filter(year > 2013) %>% filter(total_count < 20000) %>% 
ggplot(aes(x=year, y=total_count, color=site)) + 
geom_line(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
theme_minimal()

ggsave("E:/chapter1_data/figures/species_recovery_since_min_value.jpg", width = 5, height=4)


# Fit a regression line for each site and extract the slope
library(broom)
beta_estimatex <- species_recoveryx %>% 
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

beta_estimatex
View(beta_estimatex)
# Merge the beta estiamtes back to the species recovery dataframe
species_recovery_with_betax <- species_recoveryx %>% 
left_join(beta_estimatex, by = "site")

print(species_recovery_with_betax)

# Create a plot with lines for each site and add regression lines
ggplot(species_recovery_with_betax, aes(x=year, y = total_count, color = site)) +
#geom_line(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
geom_smooth(method = "lm", se = FALSE, aes(group = site)) +
labs(title = "Species Recovery Over the Years with Regression Lines",
x = "Year", 
y = "Count by Year") + 
theme_minimal() +
theme(legend.position = "none")

#ggsave("E:/chapter1_data/figures/recovery_with_regression.jpg", width = 5, height=4)
# Need to add other variables to species_recovery_with_beta
# Only want the variables with the correct dates
#View(sites_with_sufficient_data)

testx <- species_recovery_with_betax %>% 
pivot_wider(names_from = year, 
            values_from = total_count) %>% 
select(site, slope) %>% 
left_join(data_wide2, by = "site")

View(testx)

