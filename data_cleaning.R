
# DESCRIPTION: script that cleans training and testing data.
# OUTPUT:
#  - data.rda: cleaned training data, using only features we're allowed
#              to use for prediction
#  - data_test.rda: cleaned testing data, with all of the features we use
#                   for prediction parsed

# Load required packages
library(tidyverse)
library(stringr)
library(lubridate)

# Data from https://www.kaggle.com/c/sf-crime
train <- read_csv("train.csv")
test <- read_csv("test.csv")

day1 <- train$Dates[nrow(train)]   # first day of data, for DayOfData variable
# Process training data
df <- train %>%
  filter(Y < 80) %>%   # remove outliers (likely data coding bug/error)
  select(-DayOfWeek, -Descript, -Resolution) %>%   # Description and Resolution can't be used for prediction
  mutate(
    Hour = hour(Dates),
    Minute = minute(Dates),
    DayOfWeek = wday(Dates),
    Month = month(Dates),
    Day = day(Dates),
    Year = year(Dates),
    DayOfYear = yday(Dates),
    DayOfData = as.numeric(difftime(Dates,day1,unit="days")),   # cumulative day of data
    MultFive = ifelse(Minute %% 5 == 0, TRUE,FALSE),   # if minute of hour X is X:00, X:05, X:10, etc.
    IsIntersection = str_detect(Address, " / ")) %>%   # if location is a street intersection (indicated by presence of " / ")
  separate(Address, sep=" / ", into=c("Street1", "Street2")) %>%   # parse both streets of intersections
  mutate(
    StreetBlock=parse_number(Street1),   # extract block number from address
    Street1=gsub(Street1, pattern="^.*\\ Block of ", replacement=""))   # clean up street name

# Process test data
test_df <- test %>%
  mutate(
    Hour = hour(Dates),
    Minute = minute(Dates),
    DayOfWeek = wday(Dates),
    Month = month(Dates),
    Day = day(Dates),
    Year = year(Dates),
    DayOfYear = yday(Dates),
    DayOfData = as.numeric(difftime(Dates,day1,unit="days")),
    MultFive = ifelse(Minute %% 5 == 0, TRUE,FALSE),
    IsIntersection = str_detect(Address, " / ")) %>%
  separate(Address, sep=" / ", into=c("Street1", "Street2")) %>%
  mutate(
    StreetBlock=parse_number(Street1),
    Street1=gsub(Street1, pattern="^.*\\ Block of ", replacement=""))

# save(df,file="data.rda")
# save(test_df,file="data_test.rda")
