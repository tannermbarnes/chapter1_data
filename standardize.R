library(tidyverse)
library(readxl)
# Read in excel file - will be different for each person
dat <- read_excel("E:/chapter1_data/data.xlsx", sheet = "Sheet3")

cols(dat)
# Some years read in as character so change those to numeric values to combine them
cols.num <- c("2003", "2008", "2012", "2014", "2015", "2018")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)


# Pivot longer to get the data in a frame that we can manipulate easier
dat1 <- dat %>% pivot_longer(cols=c("1980", "1993", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")


# Change some variables from character to numeric
dat1 <- dat1 %>% mutate(entrances = as.numeric(as.character(numberofentrancesforhumanaccess)),
                year = as.numeric(as.character(year)))

#remove all NAs
dat2=dat1[complete.cases(dat1), ]


# Figure out what year the lowest count was recorded for each mine

# Summarize the minimum value for each site across all years
summarized_data <- dat1 %>%
  group_by(site) %>%
  summarize(min_value = min(count, na.rm = TRUE), .groups = 'drop') %>%
  left_join(dat1, by = c("site", "min_value" = "count")) %>%
  select(site, year, min_value)

print(summarized_data)

summarized_post_wns <- summarized_data %>% filter(year >= 2014 & year <= 2024)



# summarized_data has the site and year of the minimum count post-WNS, that year per mine starts at year 0
# Only use the mine with two data points after year 0 and create a regression per mine

#View(summarized_post_wns)

summarized_post_wns %>% ggplot(aes(x=year, y=min_value, color=site)) + 
geom_point(show.legend = FALSE) + theme_classic()

ggsave("E:/chapter1_data/min_value.png", width = 5, height=4)


# Subset data from the minimum count value for each site and year combination
# Identify the minimum count value for each site
min_counts <- dat1 %>% 
group_by(site) %>% 
summarize(min_year = year[which.min(count)], min_count = min(count))

# Merge the minimum counts back to the original dataframe

df_with_min <- dat1 %>% 
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

ggsave("E:/chapter1_data/species_recovery_since_min_value.png", width = 5, height=4)


# Try this code to summarize

species_recovery <- sites_with_sufficient_data %>% 
group_by(site, year) %>% 
summarize(total_count = sum(count), .groups = 'drop')

print(species_recovery)

species_recovery %>% filter(year > 2013) %>% filter(total_count < 20000) %>% 
ggplot(aes(x=year, y=total_count, color=site)) + 
geom_line(show.legend = FALSE) +
geom_point(show.legend = FALSE) +
theme_minimal()

ggsave("E:/chapter1_data/species_recovery_since_min_value.png", width = 5, height=4)

# Fit a regression line for each site and extract the slope
library(broom)
beta_estimate <- species_recovery %>% 
group_by(site) %>% 
filter(n() >=3) %>%  # Ensure there are at least 3 data points
do({
    model <- lm(total_count ~ year, data = .)
    tidy(model) %>% 
    filter(term == "year") %>%
    select(estimate) %>% 
    rename(slope = estimate) 
}) %>% 
ungroup()

# Merge the beta estiamtes back to the species recovery dataframe
species_recovery_with_beta <- species_recovery %>% 
left_join(beta_estimate, by = "site")

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

ggsave("E:/chapter1_data/regression_lines_all.png", width = 10, height=4)

# Filter out the outliers 

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

ggsave("E:/chapter1_data/regression_filtered.png", width = 5, height=4)

# Pull out species_recovery_with_beta to look at the beta estimates
#install.packages("xlsx")
library(xlsx)
file_path <- "E:/chapter1_data/species_recovery_with_beta.csv"
write.csv(species_recovery_with_beta, file_path, row.names = FALSE)

df_with_slopes <- left_join(dat1, species_recovery_with_beta %>% select(site, slope), by = "site")
print(df_with_slopes)

file_path <- "E:/chapter1_data/species_recovery_with_beta.csv"
write.csv(df_with_slopes, file_path, row.names = FALSE)

# Model using slope as dependent (response) variable and the temp difference as independent (explantory) variable
# Pivot wider so one row per site
df_wide <- df_with_slopes %>% 
pivot_wider(names_from = year, values_from = count)

model <- lm(slope ~ temperaturedifferencec, data = df_wide)
summary(model)

model1 <- lm(slope ~ temperaturedifferencec + lowestinternaltempc, data = df_wide)
summary(model1)
# Explantory variables not correlated


install.packages("visreg")
library(visreg)
visreg(model)
View(df_wide)

