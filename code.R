#install.packages("tidyverse")
library(tidyverse)
#install.packages("readxl")
library(readxl)
#install.packages("lubridate")
library(lubridate)
dat <- read_excel("E:/chapter1_data/data.xlsx", sheet = "Sheet2")

dat %>% names()

dat$tempdif <- round(dat$temperaturedifferencec)

#dat %>% ggplot(aes(x=tempdif)) + geom_bar()

cols.num <- c("2003", "2008", "2012", "2014", "2015", "2018")
dat[cols.num] <- sapply(dat[cols.num],as.numeric)
str(dat)

#pivot data longer so one line per year
dat1 <- dat %>% pivot_longer(cols=c("1980", "1993", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", 
"2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
"2019","2020", "2021", "2022", "2023", "2024"), names_to = "year", values_to = "count")

# For each mine identified I want to have a line graph with the year on the x-axis and counts of bats on the y
adventure_mine <- subset(dat1, NameofHibernaculum == "Adventure Mine")
adventure_mine1 <- adventure_mine %>% select(hibe = NameofHibernaculum, low = lowestinternaltempc, high = highestinternaltempc, 
diff = temperaturedifferencec, ore = ore, year = year, count = count) %>% group_by(year)

adventure_mine2=adventure_mine1[complete.cases(adventure_mine1), ]

# Proof of concept for one mine, but try and group by mine below
adventure_mine2 %>% ggplot(aes(x=year, y=count, group=1)) + geom_point()+geom_path()


ggsave("E:/chapter1_data/adventure_mine.png", width = 5, height=4)
#%>% ggplot(aes(x=year, y=count)) + geom_line()


# I want to have year on xaxis counts on y axis and a line for each mine in 1 plot

dat2 <- dat1 %>% select(hibe = NameofHibernaculum, low = lowestinternaltempc, high = highestinternaltempc, 
diff = temperaturedifferencec, ore = ore, year = year, count = count) %>% group_by(year) %>% 
mutate(year = as.numeric(as.character(year)))

#remove all NAs
dat3=dat2[complete.cases(dat2), ]

#remove Douglas Houghton Adit #1 which is only mine with a bat count in 1980
dat3 <- dat3 %>% subset(hibe != "Douglas Houghton Adit #1")

dat3%>% ggplot(aes(x=year, y=count, group=hibe, color=hibe)) + geom_point(show.legend = FALSE)+geom_path(show.legend = FALSE) +
scale_x_continuous(breaks = seq(1998, 2024, 4)) +
scale_y_continuous(limits = c(0, 25000))



ggsave("E:/chapter1_data/mines.png", width = 10, height=4)


dat4 <- dat3 %>% subset(count < 5000) %>% ggplot(aes(x=year, y=count, group=hibe, color=hibe)) + geom_point(show.legend = FALSE)+geom_path(show.legend = FALSE) +
scale_x_continuous(breaks = seq(1998, 2024, 4)) +
scale_y_continuous(limits = c(0, 5000))

dat4

ggsave("E:/chapter1_data/mines_under_5000_bats.png", width = 10, height=4)
