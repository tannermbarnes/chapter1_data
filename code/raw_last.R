rm(list = ls())
setwd("E:/chapter1_data/code")
library(readxl)
library(tidyverse)

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

data_wide_testx <- dat1 %>% 
select(site, ore, azimuth,entrance_height, entrance_width,
levels, shafts, water) %>% 
group_by(site) %>% 
summarize(across(everything(), ~ first(na.omit(.)), .names = "last_{col}")) %>% 
ungroup()

names(data_wide_testx) <- sub("^last_", "", names(data_wide_testx))

#View(data_wide_testx)

data_widex <- result %>% 
right_join(data_wide_testx, by = "site")

test1 <- dat1 %>% 
group_by(site, winter_year) %>% 
summarize(count = last(myotis_count), .groups = 'drop')

data_wide_last <- test1 %>% 
mutate(count = as.numeric(count),
    winter_year = as.numeric(winter_year)) %>%
pivot_wider(names_from = winter_year, values_from = count) %>% 
select(site, sort(names(.)[-1]))


# data_wide1 has metadate for temperature, ore, azimuth, entrances, levels, shafts, and counts by year
data_wide1 <- data_widex %>% 
right_join(data_wide_last, by = "site")

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
data_wide2x <- data_wide1 %>% 
left_join(wide_rh, by = "site")


