rm(list = ls())
setwd("E:/chapter1_data/code")
library(readxl)
library(tidyverse)

if (!require(dplyr)) {
  install.packages("dplyr")
}

library(dplyr)

# Because the dates are in different formats, this function will be able to get them all in the correct format
convert_excel_dates <-
function(x){
    case_when(
        str_detect(x, "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$") ~ mdy(x),
        str_detect(x, "^[0-9]{5}$") ~ x |> as.integer() |> as.Date(origin = as.Date("1899-12-30")),
        TRUE ~ NA_Date_
    )
}

dat <- read_excel("E:/chapter1_data/data.xlsx", sheet = "bat survey data") %>%
select(site = `Name of Hibernaculum...1`, ore = `Ore or Rock`, passage_length = `Length of Open Passage (feet)`,
azimuth = `Azimuth into Main Entrance`, entrance_height = `Maximum Height of Main Entrance`, entrance_width = `Maximum Width of Main Entrance`,
levels = `Number of Open Levels`, shafts = `Number of Shafts Open for Human Access`, 
water = `Standing Water at Bottom of Shaft?`,
external_ta = `External TA`, internal_ta = `Internal TA`, internal_rh = `Internal RH (%)`, date = `Date`,
disturbance = `Current Level of Disturbance`, total_bats = `Total Number of Bats`, estimate_or_count = `Estimate or Count`,
myotis_count = `Total Number of Myotis`, myotis_identified = `Sample of Myotis Identified to Species`, 
mylu = `Myotis lucifugus in Sample`, myse = `Myotis septentrionalis in Sample`, epsu = `Eptesicus fuscus`, 
pips = `Perimyotis subflavus`)

# Fix the spots that have dates but their format is messed up
dat$date[dat$date == "12/16-18/96"] <- 35415
dat$date[dat$date == "12/18/1999; 8-27-10"] <- 36512
dat$date[dat$date == "12/29-30/97; 1-16-98"] <- 35793
dat$date[dat$date == "3/3/2005; 12-18-05"] <- 38414
dat$date[dat$date == "12/28/1999; 8-29-10"] <- 36522
dat$date[dat$date == "1/15/1998; 6-26-98"] <- 35810
dat$date[dat$date == "2/14-15/1997"] <- 35475
dat$date[dat$date == "12/15/1998; 11-27-99"] <- 36144
dat$date[dat$date == "2/30/2023"] <- 44985

# Fix the spots that have dates but their format is messed up
dat$passage_length[dat$passage_length == "?"] <- NA
dat$passage_length[dat$passage_length == "75-100"] <- (75+100)/2
dat$passage_length[dat$passage_length == ">11,000"] <- 11000
dat$passage_length[dat$passage_length == ">20,000"] <- 20000
dat$passage_length[dat$passage_length == "50+"] <- 50
dat$passage_length[dat$passage_length == "400+"] <- 400
dat$passage_length[dat$passage_length == "ca. 2,500"] <- 2500
dat$passage_length[dat$passage_length == ">400"] <- 400
dat$passage_length[dat$passage_length == ">250"] <- 250
dat$passage_length[dat$passage_length == ">70"] <- 70
dat$passage_length[dat$passage_length == "> 10,000"] <- 10000
dat$passage_length[dat$passage_length == ">1,000"] <- 1000


# Remove any rows where there is no date; not useful data
cols_to_check <- "date"
datx <- dat[complete.cases(dat[, cols_to_check]), ]

dat1 <- datx %>%
mutate(date = convert_excel_dates(date)) %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date))

# Winter year is the calender year that the bats emerge from hibernation,
# so if counts were taken in the fall of 2014 the winter year will be 2015

dat1$winter_year <- ifelse(is.na(dat1$month) | is.na(dat1$year), NA,
ifelse(dat1$month >= 10, dat1$year + 1, dat1$year))

#file_path <- "E:/chapter1_data/dat1.csv"
#write.csv(dat1, file_path, row.names = FALSE)

# Split the internal_ta column into minimum_internal_ta and maximum_internal_ta
# Need to split the cells with temp data that looks like 45-48 into two columns then
# Need to convert any values from farhenheit to celsius while leaving the ones in celsius 

separate_temps <- dat1 %>% 
    separate(internal_ta, into = c("temp1", "temp2"), sep = "-", convert = TRUE) %>% 
    mutate(temp1 = as.numeric(temp1), 
            temp2 = as.numeric(temp2)) %>% 
    mutate(temp1 = ifelse(temp1 > 15,
                        ((temp1 - 32) * (5/9)),
                        temp1),
            temp2 = ifelse(temp2 > 15,
                        ((temp2 - 32) * (5/9)),
                        temp2))


# Group by site and calculate the minimum and maximum internal_ta values
result <- separate_temps %>%
  group_by(site) %>%
  mutate(minimum_internal_ta = min(temp1, na.rm = TRUE),
        maximum_internal_ta = pmax(temp1, temp2, na.rm = TRUE)) %>% 
    summarize(min = min(minimum_internal_ta, na.rm = TRUE), 
                max = max(maximum_internal_ta, na.rm = TRUE)) %>% 
    mutate(temp_diff = (max-min))

print(result)

# left_join dataframe result onto other dat1 once you pivot that dataframe wider
# pivot wider group_by(site) names_from(year) values_from(myotis_count) ???
# data_wide <- dat1 %>% 
# select(site, ore, azimuth,entrance_height, entrance_width,
# levels = levels, shafts = shafts) %>% 
# group_by(site) %>% 
# summarize(across(everything(), ~ first(na.omit(.)), .names = "first_{col}")) %>% 
# ungroup()

# names(data_wide) <- sub("^first_", "", names(data_wide))

# # Pivot and get columns by year and count as the data
# data_wide1 <- dat1 %>% 
# select(site, count = myotis_count, year) %>% 
# mutate(year = as.numeric(year)) %>% 
# group_by(site) %>% 
# pivot_wider(names_from = year, values_from = count) %>% 
# ungroup()

# # Sort columns by numerical order, keeping 'site' as the first column
# data_wide <- data_wide %>% 
# left_join(data_wide1, by = "site") %>% 
# select(site, ore, azimuth, entrance_height, entrance_width, levels,
# shafts, sort(names(.)[-1]))

# write.csv(data_wide, "E:/chapter1_data/data_wide.csv", row.names = FALSE)

#View(data_wide)

# There are mutiple counts for the same year and same site this is a problem
# To correct this we can take the first count for the winter year
# May need to come back to this 
#View(data_wide)

data_wide_test <- dat1 %>% 
select(site, ore, azimuth,entrance_height, entrance_width,
levels, shafts, water) %>% 
group_by(site) %>% 
summarize(across(everything(), ~ first(na.omit(.)), .names = "first_{col}")) %>% 
ungroup()

names(data_wide_test) <- sub("^first_", "", names(data_wide_test))

data_wide <- result %>% 
right_join(data_wide_test, by = "site")

test <- dat1 %>% 
group_by(site, winter_year) %>% 
summarize(count = first(myotis_count), .groups = 'drop')

#View(test)

data_wide_first <- test %>% 
mutate(count = as.numeric(count),
    winter_year = as.numeric(winter_year)) %>%
pivot_wider(names_from = winter_year, values_from = count) %>% 
select(site, sort(names(.)[-1]))

# data_wide1 has metadate for temperature, ore, azimuth, entrances, levels, shafts, and counts by year
data_wide1 <- data_wide %>% 
right_join(data_wide_first, by = "site")

#View(data_wide1)

#write.csv(data_wide1, "E:/chapter1_data/data_wide1.csv", row.names = FALSE)

# Next need relative humidity go back to dat1 to achieve this
# We will take the mean of the internal_rh which is internal relative humidity column 
# Some of the format is in 97-100, some have one humidity reading per site, some have multiple

# These first few lines are correcting the formatting problems in the dataset
separate_rh <- dat1 %>% 
    separate(internal_rh, into = c("rh1", "rh2"), sep = "-", convert = TRUE)

separate_rh$rh1[separate_rh$rh1 == "92 ± 3"] <- 92
separate_rh$rh1[separate_rh$rh1 == ">90"] <- 90
separate_rh$rh1[separate_rh$rh1 == "?"] <- 0

separate_rh1 <- separate_rh %>% mutate(rh1 = as.numeric(rh1))

separate_rh1$rh1[is.na(separate_rh$rh1)] <- 0
separate_rh1$rh2[is.na(separate_rh$rh2)] <- 0

# Changed the NA's to 0 so we could find the means of the numbers but have to correct for the 0s here
separate_rh1$mean_rh <- ifelse(separate_rh1$rh1 + separate_rh1$rh2 > 100, 
(separate_rh1$rh1 + separate_rh1$rh2) / 2, separate_rh1$rh1 + separate_rh1$rh2)

# Summarize so we can get the mean RH for each site
wide_rh <- separate_rh1 %>% 
group_by(site) %>% 
summarize(mean_rh = max(mean_rh))

# Change 0s in mean_rh to NAs
wide_rh$mean_rh[wide_rh$mean_rh == 0] <- NA

# Join the RH values to the wider dataset with the other variables
data_wide2 <- data_wide1 %>% 
left_join(wide_rh, by = "site")

#Need to add passage_length to data it got lost somewhere along the way
passage <- dat %>% 
mutate(passage_length = as.numeric(passage_length)) %>% 
group_by(site) %>% 
select(site, passage_length) %>% 
ungroup()

passage1 <- na.omit(passage)

data_wide2 <- data_wide2 %>% 
left_join(passage1, by = "site")


# Need to combine Belt Mine, North Belt Mine, and South Belt Mine
belt_data <- data_wide2 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")



belt_mine <- belt_data %>% filter(site %in% c("Belt Mine", "North Belt Mine", "South Belt Mine"))


combined_data <- belt_mine %>% 
mutate(site = "belt mine") %>% 
group_by(site, year) %>% 
summarize(
    min = min(min),
    max = max(max),
    temp_diff = (max - min),
    ore = first(ore),
    azimuth = 37,
    entrance_height = last(entrance_height),
    entrance_width = last(entrance_width),
    levels = 2, 
    shafts = 2, 
    water = NA,
    mean_rh = mean(mean_rh),
    passage_length = 625, 
    count = sum(count, na.rm = TRUE),
    .groups = 'drop') %>% 
mutate(count = ifelse(count == 0, NA, count)) %>% 
mutate(azimuth = as.character(azimuth),
levels = as.character(levels))

remaining_data <- belt_data %>% 
filter(!site %in% c("Belt Mine", "North Belt Mine", "South Belt Mine"))


final_data <- bind_rows(remaining_data, combined_data)

final_data <- final_data %>% 
filter(!site %in% NA)

data_wide3 <- final_data %>% 
pivot_wider(names_from = year, values_from = count) %>% 
select(site, sort(names(.)[-1])) %>% 
arrange(site)
