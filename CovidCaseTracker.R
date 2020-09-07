# librarys
library(tidyverse)
library(data.table)
library(tibbletime)

# load data
data_url <- 'https://c19downloads.azureedge.net/downloads/csv/coronavirus-cases_latest.csv'

rawdata <- data.table::fread(data_url, check.names = TRUE)

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
  geom_bar(stat = 'identity', fill = '#34273C') + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  labs(title = "COVID lab cases Cambridge") + 
  geom_line(aes(Specimen.date, rollMean), colour = '#E6CF44', size = 1.1)

ggplot(cambridge, aes(Specimen.date , Cumulative.lab.confirmed.cases)) + 
  geom_bar(stat = 'identity') + scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
  labs(title = "COVID lab cases Cambridge")


# from another source - total country/region cases

url2 <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"

worldRawData <- fread(url2, check.names = T)

head(worldRawData)

unique(worldRawData$location)

UkDataCovid <- filter(worldRawData, location == "United Kingdom")

head(UkDataCovid)
colnames(UkDataCovid)

ggplot(UkDataCovid, aes(date, new_cases)) + geom_bar(stat = 'identity', aes(fill = "Daily_new_cases")) +
  geom_line(aes(date, new_deaths, colour = "Daily_new_deaths"), size = 1.25) +
  scale_x_date(date_breaks = '2 weeks') +
  labs(title = "Total daily new Covid UK cases") + 
  scale_colour_manual(name = "Deaths",
                      values = c(Daily_new_deaths="#033A22")) +
  scale_fill_manual(name = "Cases", values = c(Daily_new_cases="#6B0308"))

