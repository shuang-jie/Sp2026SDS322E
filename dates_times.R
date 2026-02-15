## Dates and Times

## Underlying representations as integers

Sys.Date()

Sys.Date() |>
  as.integer()

Sys.time()

Sys.time() |>
  as.integer()

################################################################################
## Austin 311 data from https://data.austintexas.gov


## Need to load tidyverse
library(tidyverse)

## Read the data
dat <- read_csv("https://raw.githubusercontent.com/shuang-jie/Sp2026SDS322E/main/PARD_311_Data_20250218.csv.bz2")
dat

## Quick look
glimpse(dat)

## What kinds of service requests are there?
table(dat$`SR Description`)

## Make a bar plot of the SR Description variable
dat |>
  ggplot(aes(x = `SR Description`)) +
  geom_bar() +
  coord_flip()

glimpse(dat)


## Convert date variables into R date format
dat <- dat |>
  mutate(datetime = mdy_hms(`Created Date`, tz = ""),
         close = mdy_hms(`Close Date`, tz = ""))

## Look at newly created variables
dat |>
  select(`Created Date`, datetime, close)


## Show total number of service requests by month
dat |>
  mutate(month = month(datetime)) |>
  group_by(month) |>
  summarize(n = n()) |>
  ggplot(aes(x = month, y = n)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = month.name)+
  theme_bw()

month.name
month.abb


## Show total number of service requests by month and year
dat |>
  mutate(month = month(datetime),
         year = year(datetime)) |>
  group_by(year, month) |>
  summarize(n = n()) |>
  ggplot(aes(x = month, y = n)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = 1:12)+
  theme_bw()

## Distribution of time to close a service request
dat |>
  mutate(date = as.Date(datetime),
         closedate = as.Date(close)) |>
  select(date, closedate) |>
  mutate(timetoclose = as.integer(closedate - date)) |>
  select(timetoclose) |>
  summary()

## Plot distribution of time to close a service request
dat |>
  mutate(date = as.Date(datetime),
         closedate = as.Date(close)) |>
  select(date, closedate) |>
  mutate(timetoclose = as.integer(closedate - date)) |>
  select(timetoclose) |>
  filter(!is.na(timetoclose)) |>
  ggplot(aes(x = timetoclose)) +
  geom_histogram(bins = 20) + 
  theme_bw()

## Look at times less than 60 days
dat |>
  mutate(date = as.Date(datetime),
         closedate = as.Date(close)) |>
  select(date, closedate) |>
  mutate(timetoclose = as.integer(closedate - date)) |>
  filter(!is.na(timetoclose)) |>
  filter(timetoclose < 60) |>
  ggplot(aes(x = timetoclose)) +
  geom_histogram(bins = 20)+ 
  theme_bw()

## Show daily number of service requests
dat |>
  mutate(date = as.Date(datetime)) |>
  group_by(date) |>
  summarize(n = n()) |>
  arrange(date) |>
  ggplot(aes(x = date, y = n)) +
  geom_line()+ 
  theme_bw()


## Show daily number of service requests by ZIP code
dat |>
  mutate(date = as.Date(datetime)) |>
  rename(zipcode = `Zip Code`) |>
  group_by(zipcode, date) |>
  summarize(n = n()) |>
  arrange(date) |>
  ggplot(aes(x = date, y = n)) +
  geom_point(size = 0.5) +
  facet_wrap(vars(zipcode))+ 
  theme_bw()


