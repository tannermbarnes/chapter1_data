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
external_ta = `External TA`, internal_ta = `Internal TA`, internal_rh = `Internal RH (%)`, date = `Date`,
disturbance = `Current Level of Disturbance`, total_bats = `Total Number of Bats`, estimate_or_count = `Estimate or Count`,
myotis_count = `Total Number of Myotis`, myotis_identified = `Sample of Myotis Identified to Species`, 
mylu = `Myotis lucifugus in Sample`, myse = `Myotis septentrionalis in Sample`, epsu = `Eptesicus fuscus`, 
pips = `Perimyotis subflavus`)

dat1 <- dat %>%
mutate(date = convert_excel_dates(date)) %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date))

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

# left_join dataframe result onto other dataframe once you pivot that dataframe wider
# pivot wider group_by(site) names_from(year) values_from(myotis_count) ???

