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
pips = `Perimyotis subflavus`, utm = `UTM (NAD 1983)`)

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
levels, shafts, water, utm) %>% 
group_by(site) %>% 
summarize(across(everything(), ~ first(na.omit(.)), .names = "last_{col}")) %>% 
ungroup()

names(data_wide_testx) <- sub("^last_", "", names(data_wide_testx))

data_wide_testx <- data_wide_testx %>% 
  separate(utm, into = c("zone", "easting", "northing"), sep = " ")

write.csv(data_wide_testx, file = "E:/chapter1_data/coordinates_of_mines.csv", row.names = FALSE)
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

data <- data_wide2x %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# Subset data from the minimum count value for each site and year combination
# Identify the minimum count value for each site
# Get the minimum and maximum count data to normalize the counts
min_max_count <- data %>% 
group_by(site) %>% 
summarize(min_count = min(count, na.rm = TRUE), max_count = max((count/1.5), na.rm = TRUE),
real_max_count = max(count, na.rm = TRUE)) %>% 
ungroup()

# Merge the min and max counts back into the orginal data
data_with_min_max <- data %>% 
left_join(min_max_count, by = "site")

# Normalize the count data
normalize_count <- data_with_min_max %>% 
mutate(normalize_count = (count - min_count) / (max_count - min_count)) %>% 
mutate(normalize_to_min = count / (min_count + 1))

# To get the normalized count and relative year of max decrease for sliding scale
min_count_year1 <- normalize_count%>% 
drop_na(count) %>% 
group_by(site) %>% 
filter(normalize_count == min(normalize_count)) %>% 
slice(1) %>% # in case of ties, take the first occurence
ungroup() %>% 
select(site, min_year = year) %>% 
mutate(min_year = as.numeric(min_year))

# Merge the minimum count year back into the normalized data
data_with_decrease_year <- normalize_count %>% 
mutate(year = as.numeric(year)) %>% 
left_join(min_count_year1, by = "site") %>% 
mutate(relative_year = year - min_year)

# Filter original data to keep counts from the year of max decrease onwards
filter_data <- data_with_decrease_year %>% 
filter(relative_year >= 0) %>% 
filter(normalize_count >= 0) %>% 
select(site, relative_year, normalize_count)

#View(filter_data)

# Filter sites with at least 2 years of data after the minimum count year
# sites_with_sufficient_data <- filter_data %>% 
# group_by(site) %>% 
# filter(n() >= 2) %>% 
# ungroup()

# Nest the data by site
nested_data <- filter_data %>% 
group_by(site) %>% 
nest()

#print("Nested data:")
#print(nested_data)

# Fit a linear regression model for each site
nested_data <- nested_data %>%
  mutate(model = map(data, ~ {
    df <- .x
    # Ensure 'year' and 'count' are numeric
    df <- df %>%
      mutate(year = as.numeric(relative_year),
             count = as.numeric(normalize_count))
    # Fit the linear model
    lm(count ~ year, data = df)
  }))

# Check the models
# print("Nested data with models:")
# print(nested_data)

# Step 4: Tidy the model outputs
tidied_data <- nested_data %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

# Check the tidied data
#print("Tidied data:")
#print(tidied_data)

# Step 5: Filter and select the slope for the 'year' term
regression_results <- tidied_data %>%
  filter(term == "year") %>%
  select(site, slope = estimate) %>% 
  mutate(slope = ifelse(is.na(slope), 0, slope))



# Check the regression results
#print("Regression results:")
#print(regression_results)

# Step 6: Add the slope to the original data frame
final_data <- filter_data %>%
  left_join(regression_results, by = "site")

#View(final_data)

# Merge final slopes with the other metadata in a wide format to model
model_data1 <- final_data %>% 
pivot_wider(names_from = relative_year, 
            values_from = normalize_count) %>% 
select(site, slope) %>% 
left_join(data_wide2x, by = "site")


model_data2 <- data_with_decrease_year %>% 
group_by(site) %>% 
summarize(min_count = min(min_count), max_count = max(max_count)) %>% 
inner_join(model_data1, by = "site") %>% 
ungroup()

 sites_to_remove <- c("Adventure Shaft", "Algonquin Adit #2 (Mark's Adit)", "Collin's Adit", "Copper Falls Mine", "Douglas Houghton Adit #1", "Jackson Mine, B Working",
 "Jackson Mine, Tram Tunnel Exit", "Merchant's Adit North", "Merchant's Adit South", "National Mine #7",
 "North American Adit", "Ohio Traprock Mine #1", "Ohio Traprock Mine #2", "Ohio Traprock Mine #3",
 "Ridge Adit", "Rockland Mine, Shaft 3", "Seneca Mine #3", "Trader's Mine", "Unknown Keweenaw",
 "West Vein Adit (Robin's Ore)", "White Pine Mine", "Cushman Adit", "Jackson Mine, Tram Tunnel", "West Evergreen Bluff Mine",
 "Caledonia Mine Complex", "Michigan (A Shaft)", "Ogimaw Mine", "Owl Creek Fissure (Old Copper Falls)",
 "Pewabic Mine (Iron Mountain)", "Piscatauqau Adit", "Nassau Mine", "Goodrich Adit B", "Aztec Mine",
 "Bumblebee Mine", "Millie Mine", "Toltec Mine")

filtered_data1 <- model_data2 %>%
  filter(!site %in% sites_to_remove)


add_last_count <- filtered_data1 %>% 
pivot_longer(cols=c("1980", "1981", "1993","1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

add_last_count1 <- add_last_count %>% 
group_by(site) %>% 
arrange(site, desc(year)) %>% 
mutate(last_count = first(na.omit(count))) %>% 
ungroup()

add_last_count2 <- add_last_count1 %>% 
pivot_wider(names_from = year, values_from = count)

df_slide_scale <- add_last_count2 #%>% 
#filter(!site %in% sites_to_remove)

# Add crash intensity
df_slide_scale$crash <- 1 - (df_slide_scale$min_count/df_slide_scale$max_count)
df_slide_scale$log_max_count <- log(df_slide_scale$max_count)

# LN of explantory variables to get Pseduothreshold
# Add 1 all min values to avoid log(0)
df_slide_scale <- df_slide_scale %>% 
mutate(min_value = min + 1, 
      max_value = max + 1) %>% 
mutate(min_value = ifelse(min_value < 0, 1, min_value)) %>% 
mutate(bin = ifelse(slope > 0, 1, 0)) %>% 
subset(max_count > 6)

df_slide_scale$log_min_value <- log(df_slide_scale$min_value)
df_slide_scale$log_max_value <- log(df_slide_scale$max_value)

model_df <- df_slide_scale %>% 
mutate(recovery_status = ifelse(bin == 1, "recovering", "not recovering")) %>% 
mutate(recovery_status = as.factor(recovery_status)) %>% 
mutate(site_numeric = as.numeric(factor(site))) %>% 
select(site, site_numeric, bin, recovery_status, slope, levels, shafts, 
crash, min, max, temp_diff, ore, zone, easting, northing) %>% 
mutate(temp_diff_sqrt = sqrt(temp_diff),
temp_diff_log = log(temp_diff),
bin_numeric = as.numeric(bin),
ore = as.factor(ore),
levels = as.factor(levels),
shafts = as.factor(shafts)) %>% 
mutate(levels_numeric = as.numeric(levels), 
shafts_numeric = as.numeric(shafts))

write.csv(model_df, "E:/chapter1_data/tanner.csv", row.names = FALSE)
View(model_df)
