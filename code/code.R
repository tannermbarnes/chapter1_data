library(tidyverse)
library(readxl)
dat <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/actual_data.xlsx")

dat %>% 
  subset(site == "Mead Adit of Carp Lake Mine") %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line() +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 24),         # Increase axis tick labels size
    axis.title = element_text(size = 24),        # Increase axis title size
    plot.title = element_text(size = 24),        # Increase plot title size
    legend.text = element_text(size = 24),       # Increase legend text size
    legend.title = element_text(size = 24),      # Increase legend title size
    plot.title.position = "plot"                  # Position title at the top of the plot
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  labs(title = "Mead Mine Bat Counts Over Time")  # Add plot title


ggsave("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/Chapter1/figures/mead_mine.png", width = 12, height=12)


# For each mine identified I want to have a line graph with the year on the x-axis and counts of bats on the y
adventure_mine <- subset(dat, site == "Adventure Mine")
adventure_mine1 <- adventure_mine %>% select(hibe = NameofHibernaculum, low = lowestinternaltempc, high = highestinternaltempc, 
diff = temperaturedifferencec, ore = ore, year = year, count = count) %>% group_by(year)

adventure_mine2=adventure_mine1[complete.cases(adventure_mine1), ]

# Proof of concept for one mine, but try and group by mine below
adventure_mine2 %>% ggplot(aes(x=year, y=count, group=1)) + geom_point()+geom_path()


ggsave("E:/chapter1_data/adventure_mine.png", width = 5, height=4)
#%>% ggplot(aes(x=year, y=count)) + geom_line()


# I want to have year on xaxis counts on y axis and a line for each mine in 1 plot

dat2 <- dat1 %>% select(hibe = 'Name of Hibernaculum', low = 'Lowest internal temp c', high = 'highest internal temp c',
year, count) %>% group_by(year) %>% 
mutate(year = as.numeric(as.character(year)))

#remove all NAs
dat3=dat2[complete.cases(dat2), ]

#remove Douglas Houghton Adit #1 which is only mine with a bat count in 1980
dat3 <- dat3 %>% subset(hibe != "Douglas Houghton Adit #1")

dat3%>% ggplot(aes(x=year, y=count, group=hibe, color=hibe)) + geom_point(show.legend = FALSE)+geom_path(show.legend = FALSE) +
scale_x_continuous(breaks = seq(1998, 2024, 4)) +
scale_y_continuous(limits = c(0, 29000))



ggsave("E:/chapter1_data/mines.png", width = 10, height=4)


dat4 <- dat3 %>% subset(count < 5000) %>% ggplot(aes(x=year, y=count, group=hibe, color=hibe)) + geom_point(show.legend = FALSE)+geom_path(show.legend = FALSE) +
scale_x_continuous(breaks = seq(1998, 2024, 4)) +
scale_y_continuous(limits = c(0, 5000))

dat4

ggsave("E:/chapter1_data/mines_under_5000_bats.png", width = 10, height=4)


install.packages("ggforce")
library(ggforce)

dat3%>% ggplot(aes(x=year, y=count, group=hibe, color=hibe)) + geom_point(show.legend = FALSE)+geom_path(show.legend = TRUE) +
scale_x_continuous(breaks = seq(1998, 2024, 4)) +
facet_zoom(ylim = c(0, 5000))

dat3 %>% subset(year > 2014) %>% ggplot(aes(x=diff, y=count)) + geom_point() + geom_abline()
dat3 %>% subset(year > 2014) %>% ggplot(aes(x=low, y=count)) + geom_point() + geom_abline()
dat3 %>% subset(year > 2014) %>% ggplot(aes(x=high, y=count)) + geom_point() + geom_abline()

dat3 %>% mutate(avgtemp = ((low + high)/2)) %>% 
subset(year > 2014) %>% ggplot(aes(x=avgtemp, y=count)) + geom_point() + geom_abline()

ggsave("E:/chapter1_data/facet_zoom.png", width = 10, height=4)

# Next we want to add the names of the mines that are recovering to the plot
# Look up which mines we think they are and graph those to see
important_mines <- dat3 %>% subset(hibe %in% c("Adventure Mine", "Belt Mine", "Delaware Mine", "Iron Mountain Iron Mine (Tourist Mine)", "Keel Ridge Shaft", "Mead Adit of Carp Lake Mine", "Norway Mine", "South Lake Mine", "Windsor Shaft #3", "Lafayette East Shaft", "Glen Adit #1"))

important_mines %>% ggplot(aes(x=year, y=count, color=hibe)) + geom_line()

#facet_zoom(ylim = c(0, 5000))

ggsave("E:/chapter1_data/important_mines.png", width = 10, height=4)

important_mines %>% ggplot(aes(x=diff, y=count)) + geom_point()

important_mines %>% subset(year > 2014) %>% ggplot(aes(x=diff, y=count)) + geom_point() +geom_abline()
important_mines %>% subset(year > 2014) %>% ggplot(aes(x=low, y=count)) + geom_point() + geom_abline()
important_mines %>% subset(year > 2014) %>% ggplot(aes(x=high, y=count)) + geom_point() + geom_abline()

ggsave("E:/chapter1_data/important_mines-test.png", width = 10, height=4)
# Summary statistics mines and counts and temps

dat3 %>% subset(year > 2014)%>% group_by(hibe) %>% mutate(total_bats = sum(count))

table1 <- dat3 %>% pivot_wider(names_from = year, values_from = count)
table1

# PCA to show which mines are more similar to one another and the strength of the similarity
# Include the explantory variables 
# Change the categorical variables to numbers 
# Run a PCA

test <- important_mines[, !(names(important_mines) %in% "ore")] %>% subset()


pca_result <- prcomp(test, scale.=TRUE, na.rm=TRUE)

head(dat1)







# NMS step by step procedure - species matrix = response variable with rate of change with mines as x
# environmental matrix has mines as x and explanatory variables as y
#install.packages("vegan")
library(vegan)

# Shows which mines are in dataframe

unique(important_mines$hibe)

important_variables <- important_mines %>% mutate(orenum = as.numeric(ore))