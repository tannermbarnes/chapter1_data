library(readxl)
library(tidyverse)

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

file_path <- "E:/chapter1_data/dat1.csv"
write.csv(dat1, file_path, row.names = FALSE)

# Separate temps into minimum and maximum temperatures
df_separated <- dat %>%
  # Save the original values for both columns
  mutate(original_external_ta = external_ta, original_internal_ta = internal_ta) %>%
  # Separate temperature ranges into min and max columns for external_ta
  separate(external_ta, into = c("min_external_ta", "max_external_ta"), sep = "-", fill = "right", convert = TRUE) %>%
  # Separate temperature ranges into min and max columns for internal_ta
  separate(internal_ta, into = c("min_internal_ta", "max_internal_ta"), sep = "-", fill = "right", convert = TRUE) %>%
  # Handle single value cases for external_ta
  mutate(
    min_external_ta = as.numeric(min_external_ta),
    max_external_ta = as.numeric(max_external_ta),
    min_external_ta = if_else(is.na(min_external_ta) & grepl("^\\d+$", original_external_ta), as.numeric(original_external_ta), min_external_ta),
    max_external_ta = if_else(is.na(max_external_ta), min_external_ta, max_external_ta)
  ) %>%
  # Handle single value cases for internal_ta
  mutate(
    min_internal_ta = as.numeric(min_internal_ta),
    max_internal_ta = as.numeric(max_internal_ta),
    min_internal_ta = if_else(is.na(min_internal_ta) & grepl("^\\d+$", original_internal_ta), as.numeric(original_internal_ta), min_internal_ta),
    max_internal_ta = if_else(is.na(max_internal_ta), min_internal_ta, max_internal_ta)
  ) %>%
  # Optionally, remove the temporary columns if no longer needed
  select(-original_external_ta, -original_internal_ta)

print(df_separated)

# Group by site and calculate the overall min and max temperatures for both external_ta and internal_ta
df_with_extremes <- df_separated %>%
  group_by(site) %>%
  summarize(
    max_external_ta = if_else(all(is.na(max_external_ta)), NA_real_, max(max_external_ta, na.rm = TRUE)),
    min_external_ta = if_else(all(is.na(min_external_ta)), NA_real_, min(min_external_ta, na.rm = TRUE)),
    max_internal_ta = if_else(all(is.na(max_internal_ta)), NA_real_, max(max_internal_ta, na.rm = TRUE)),
    min_internal_ta = if_else(all(is.na(min_internal_ta)), NA_real_, min(min_internal_ta, na.rm = TRUE)),
    external_ta_diff = if_else(all(is.na(max_external_ta) | all(is.na(min_external_ta))), NA_real_, max_external_ta - min_external_ta),
    internal_ta_diff = if_else(all(is.na(max_internal_ta) | all(is.na(min_internal_ta))), NA_real_, max_internal_ta - min_internal_ta)
  )

# Print the resulting dataframe
print(df_with_extremes)

# Join the temperature data back with other metadata
dat <- left_join(dat, df_with_extremes, by="site")


