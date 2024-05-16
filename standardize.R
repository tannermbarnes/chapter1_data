library(tidyverse)
library(readxl)
# Read in excel file - will be different for each person
dat <- read_excel("E:/chapter1_data/data.xlsx", sheet = "Sheet2")

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

dat1 %>% 
  group_by(NameofHibernaculum, year) %>% 
  summarize(min_value = min(count, na.rm = TRUE), .groups = 'drop')


library(dplyr)

# Summarize the minimum value for each site across all years
summarized_data <- dat1 %>%
  group_by(NameofHibernaculum) %>%
  summarize(min_value = min(count, na.rm = TRUE), .groups = 'drop') %>%
  left_join(dat1, by = c("NameofHibernaculum", "min_value" = "count")) %>%
  select(NameofHibernaculum, year, min_value)

print(summarized_data)

summarized_data %>% filter(year >= 2014 & year <= 2024)

# summarized_data has the site and year of the minimum count post-WNS, that year per mine starts at year 0
# Only use the mine with two data points after year 0 and create a regression per mine
