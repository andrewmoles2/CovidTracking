# librarys
library(tidyverse)
library(data.table)
library(tibbletime)

# load data
data_url <- 'https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv'

rawdata <- data.table::fread(data_url, check.names = TRUE)

# https://coronavirus.data.gov.uk/about-data
# change weeks to week dates. change -99 to 0-2 (not sure how, average or 1). group by/aggregate by rawdata$utla19_nm 

head(rawdata)
unique(rawdata$Area.name)

# pull out cambridge
cambridge <- filter(rawdata, Area.name == "Cambridge")

# 7 day rolling mean
rolling_mean <- tibbletime::rollify(mean, window = 7)

cambridge <- cambridge %>%
  mutate(cambridge, rollMean = rolling_mean(Daily.lab.confirmed.cases))

# plot
theme_set(theme_bw())

ggplot(cambridge, aes(Specimen.date , Daily.lab.confirmed.cases)) + 
  geom_bar(stat = 'identity', fill = '#34273C', alpha = 0.9) + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  labs(title = "COVID lab cases Cambridge") + 
  geom_line(aes(Specimen.date, rollMean), colour = '#E6CF44', size = 1.1) +
  theme(axis.text.x = element_text(size = 12),
        axis.title=element_text(size = 14),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 16))

ggplot(cambridge, aes(Specimen.date , Cumulative.lab.confirmed.cases)) + 
  geom_bar(stat = 'identity') + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  labs(title = "COVID lab cases Cambridge")


# from another source - total country/region cases
theme_set(theme_bw())

url2 <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"

worldRawData <- fread(url2, check.names = T)

UkDataCovid <- filter(worldRawData, location == "United Kingdom")

ggplot(UkDataCovid, aes(date, new_cases)) + geom_bar(stat = 'identity', aes(fill = "Daily_new_cases"), alpha = 0.9) +
  geom_line(aes(date, new_deaths, colour = "Daily_new_deaths"), size = 1.25) +
  scale_x_date(date_breaks = '2 weeks') +
  labs(title = "Total daily new Covid UK cases") + 
  scale_colour_manual(name = "Deaths",
                      values = c(Daily_new_deaths="#033A22")) +
  scale_fill_manual(name = "Cases", values = c(Daily_new_cases="#6B0308")) +
  theme(axis.text.x = element_text(angle = -15,size = 12),
        axis.title=element_text(size = 16),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12))


