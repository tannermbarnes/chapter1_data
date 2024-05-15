install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
dat <- read_excel("E:/chapter1_data/data.xlsx", sheet = "Sheet2")

dat %>% names()

dat$tempdif <- round(dat$temperaturedifferencec)

dat %>% ggplot(aes(x=tempdif)) + geom_bar()

cols.num <- c("2003", "2008", "2012", "2014", "2015", "2018")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)
str(dat)

#pivot data longer so one line per year
dat1 <- dat %>% pivot_longer(cols=c("1980", "1993", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# For each mine identified I want to have a line graph with the year on the x-axis and counts of bats on the y
adventure_mine <- subset(dat1, NameofHibernaculum == "Adventure Mine")
adventure_mine %>% ggplot(aes(x=year, y=count)) + geom_point()

ggsave("E:/chapter1_data/adventure_mine.tiff", width = 5, height=4)
#%>% ggplot(aes(x=year, y=count)) + geom_line()

str(dat1)


# I want to have year on xaxis counts on y axis and a line for each mine in 1 plot
