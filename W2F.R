## Load the tidyverse package
library(tidyverse)

## define an object dat same as airquality data
dat = airquality

## Use filter() function to remove rows whose variable Ozone is NA values
dat |> filter(!is.na(Ozone)) 

## Use filter() function to remove rows whose variable Ozone and Solar.R are both NA values
dat |> filter(!is.na(Solar.R) & !is.na(Ozone))

## Use filter() function to get only Month 5 data
dat |> filter(Month == 5) 

## Use summarize() function to get mean of variable Ozone
dat |> summarize(ozone = mean(Ozone))


## Filter NA values (using the code above) and then compute the mean of Ozone
## using the summarize() and mean() functions
dat |> filter(!is.na(Ozone)) |> summarize(ozone = mean(Ozone))


## Compute summary statistics for more variables (Ozone, Wind, Temp)
dat |> filter(!is.na(Ozone)) |>
  summarize(ozone = mean(Ozone),
            wind = mean(Wind),
            temp = mean(Temp),
            sdozone = sd(Ozone))


## Bivariate relationships
## Look at the Temp and Wind variables only by using the select() function
dat |> select(Temp, Wind) |> summary()



## Divide the range of Temp into 2 categories using the cut_number() function
## and create a new variable 'temp_cat' with the mutate() function
dat |> select(Temp, Wind) |> mutate(temp_cat = cut_number(Temp, 2))


## Compute the mean of Wind within each of the 2 Temp categories created above
dat |>
  select(Temp, Wind) |>
  mutate(temp_cat = cut_number(Temp, 2)) |>
  group_by(temp_cat) |>
  summarize(wind = mean(Wind))


## Use 5 categories of Temp instead of 2 categories by changing the argument to
## cut_number()
dat |>
  select(Temp, Wind) |>
  mutate(temp_cat = cut_number(Temp, 5)) |>
  group_by(temp_cat) |>
  summarize(wind = mean(Wind))


## Compute the mean of Temp within each of the 5 categories of Wind
dat |>
  select(Temp, Wind) |>
  mutate(wind_cat = cut_number(Wind, 5)) |>
  group_by(wind_cat) |>
  summarize(temp = mean(Temp))


## Look at Solar.R and Ozone
dat |> select(Solar.R, Ozone)


## Filter out NA values (how many rows in the new data frame?)
dat |>
  select(Solar.R, Ozone) |>
  filter(!is.na(Solar.R) & !is.na(Ozone))


## Create 2 categories of Solar.R and save in new variable called 'solar_cat'
dat |>
  select(Solar.R, Ozone) |>
  filter(!is.na(Solar.R) & !is.na(Ozone)) |>
  mutate(solar_cat = cut_number(Solar.R, 2))


## Compute the mean of ozone within each of 2 categories of Solar.R
dat |>
  select(Solar.R, Ozone) |>
  filter(!is.na(Solar.R), !is.na(Ozone)) |>
  mutate(solar_cat = cut_number(Solar.R, 2)) |>
  group_by(solar_cat) |>
  summarize(ozone = mean(Ozone))


