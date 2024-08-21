# THIS CODE WILL CREATE A DATAFRAME PER SURVEY AND PER TEMPERATURE VARIABLE FOR THAT SURVEY SOMEHOW HAHHAHAHAHAHAH #
rm(list = ls())
setwd("E:/chapter1_data/code")
library(tidyverse)
library(readxl)
# Read in excel file - will be different for each person
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
water_in = `Standing Water at Bottom of Shaft?`, water_bottom = `Standing Water in Adit or Cave Entrance?`,
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

#Fix the spots that temperature format is messed up
dat$internal_ta[dat$internal_ta == "-3.9to6.7(2nd level)"] <- -3.9
dat$internal_ta[dat$internal_ta == "<32"] <- 32
dat$internal_ta[dat$internal_ta == "≤33"] <- 33
dat$internal_ta[dat$internal_ta == "<49.2"] <- 49.2
dat$internal_ta[dat$internal_ta == "<33"] <- 33

dat$myotis_count[dat$myotis_count == "ca. 50,000"] <- 50000
dat$myotis_count[dat$myotis_count == ">24,000"] <- 24000

# Remove any rows where there is no date; not useful data
cols_to_check <- "date"
datx <- dat[complete.cases(dat[, cols_to_check]), ]

dat1 <- datx %>%
mutate(date = convert_excel_dates(date)) %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date))

# Function to convert temperatures from Fahreheit to Celsius if they are greater than 15
convert_to_celsius <- function(temp) {
    ifelse(temp > 15, (temp - 32) * (5/9), temp)
}

separate_temps <- dat1 %>% 
separate(internal_ta, into = c("temp1", "temp2"), sep = "(?<=\\d)-(?=\\d)", convert = TRUE, fill = "right") %>%
    mutate(temp1 = as.numeric(temp1), 
            temp2 = as.numeric(temp2),
            temp1 = convert_to_celsius(temp1),
            temp2 = convert_to_celsius(temp2)
    )

separate_temps$temp2[13] <- 6.7


create_mean_temp <- separate_temps %>% select(site, ore, temp1, temp2, date, year, month, day, count = myotis_count) %>% 
mutate(mean_temp = ifelse(is.na(temp2), temp1, (temp1 + temp2) / 2))

create_mean_temp1 <- create_mean_temp %>% group_by(site) %>% mutate(site_mean_temp = mean(mean_temp, na.rm = TRUE))

fin <- create_mean_temp1 %>%
  drop_na(count) %>%
  group_by(site) %>%
  mutate(mean_temp = ifelse(is.na(mean_temp), mean(mean_temp, na.rm = TRUE), mean_temp)) %>%
  mutate(count = as.numeric(count)) %>%
  arrange(site, year) %>%
  mutate(yearly_diff = count - lag(count)) %>%
  mutate(crash_year = ifelse(any(!is.na(yearly_diff)), year[which.min(yearly_diff)], NA)) %>%
  mutate(min_count = min(count, na.rm = TRUE), 
         min_year = year[which.min(count)],
         max_count = max(count, na.rm = TRUE),
         max_year = year[which.max(count)], 
         last_year = year[which.max(year)],
         last_count = count[which.max(year)]
  ) %>%
  mutate(crash_year = ifelse(is.na(crash_year), year + 1, crash_year)) %>% 
  mutate(period = ifelse(year < crash_year, "before", "after")) %>% 
    mutate(period = ifelse(year < 2013, "before", period)) %>% 
  ungroup()

fin <- fin %>% select(site, period, year, crash_year, count, yearly_diff, max_year, min_year, last_year, count, min_count, max_count, last_count, date, day, month, mean_temp, site_mean_temp)

remove_sites <- c("B-95 (cave)", "Collin's Adit", "Douglas Houghton Adit #1", "Hilton (Shaft 1)", "Jackson Mine, B Working", 
"Lafayette East Adit", "North American Adit", "Ridge Adit", "Rockport Quarry South Tunnel", "South Falls Cave",
"Spider Cave", "Vivian Adit", "White Pine Mine", "Ohio Traprock #61", "Randville Quarry Mine", "Scott Falls Cave")

fin_filter <- fin %>% group_by(site) %>% 
filter(all(c("before", "after") %in% period)) %>% 
filter(!(site == "Millie Mine" & year == 1992)) %>% 
filter(!(site %in% remove_sites)) %>% 
mutate(crash_year = ifelse(site == "Flintsteel Adit", 2019, crash_year),
crash_year = ifelse(site == "Glen Adit #1", 2020, crash_year),
crash_year = ifelse(site == "Hendrie River Water Cave", 2014, crash_year),
crash_year = ifelse(site == "Millie Mine", 2020, crash_year),
crash_year = ifelse(site == "South Bluff Adit", 2020, crash_year),
period = ifelse(site == "Flintsteel Adit" & year < 2019, "before", period),
period = ifelse(site == "Flintsteel Adit" & year >= 2019, "after", period),
period = ifelse(site == "Glen Adit #1" & year < 2020, "before", period),
period = ifelse(site == "Glen Adit #1" & year >= 2020, "after", period),
period = ifelse(site == "Hendrie River Water Cave" & year < 2014, "before", period),
period = ifelse(site == "Hendrie River Water Cave" & year >= 2014, "after", period),
period = ifelse(site == "Millie Mine" & year < 2020, "before", period),
period = ifelse(site == "Millie Mine" & year >= 2020, "after", period),
period = ifelse(site == "South Bluff Adit" & year < 2017, "before", period),
period = ifelse(site == "South Bluff Adit" & year >= 2017, "after", period),
    ) %>% 
ungroup()

View(fin_filter)
