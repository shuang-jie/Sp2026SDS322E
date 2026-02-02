#################################### Visualizing Multi-dimensional Relationships ####################################

## Load tidyverse as usual (which includes ggplot2 and the gss_cat dataset)
library(tidyverse)

# Use the built-in gss_cat dataset
# We perform a filter first to remove missing data, which is common in survey data
dat <- gss_cat |>
  select(year, race, age, tvhours, marital) |>
  filter(!is.na(tvhours), !is.na(age))

####

## Quick look
glimpse(dat)

## See the first few rows
dat

## Last few rows
tail(dat)

## Subset rows, colon
dat |> slice(5:7)

## Make a bar plot showing the number of observations at each year
dat |>
  ggplot(aes(x = year)) +
  geom_bar()+ 
  theme_bw()

# Make a bar plot showing the mean level of age for each race
# summarize y within each x, and draw bars at the mean of y.
dat |>
  group_by(race) |>
  summarise(mean_age = mean(age)) |>
  ggplot(aes(x = race, y = mean_age)) +
  geom_col() +
  theme_bw()


dat |>
  ggplot(aes(x = race, y = age)) +
  geom_bar(stat = "summary", fun = "mean")+ 
  theme_bw()


# Add error bars (+/- 1 SE) to the bar plot, fill color
dat |>
  ggplot(aes(x = race, y = age)) +
  geom_bar(aes(fill = race), stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se")+ 
  theme_bw()

# Change the errorbar size of the bar plot
dat |>
  ggplot(aes(x = race, y = age)) +
  geom_bar(aes(fill = race),
           stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se",
                width = 0.3) +
  labs(fill = "Race Group", x = "Race Group")+ 
  theme_bw()

# Show average tvhours by year
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_bar(stat = "summary", fun = "mean")+ 
  theme_bw()

## Facet by race group
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(vars(race))+ 
  labs(y = "TV Hours") +
  theme_bw()

## Jitter the points a little to show individual points
# (Added alpha because gss_cat has many points)
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_jitter(width = 0.3, height = 0.3, alpha = 0.1)+ 
  geom_line(stat = "summary", fun = "mean")+ 
  theme_bw()

## Color each line grouping by marital status
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_line(aes(group = marital, color = marital), stat = "summary", fun = "mean") + 
  theme_bw()

## Color each line AND point by marital status
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_point(aes(color = marital), stat = "summary", fun = "mean") +
  geom_line(aes(group = marital, color = marital), stat = "summary", fun = "mean")+ 
  theme_bw()

## Remove the legend
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_point(aes(color = marital), stat = "summary", fun = "mean") +
  geom_line(aes(group = marital, color = marital), stat = "summary", fun = "mean") + 
  theme_bw()+
  theme(legend.position = "none")

## Make a scatter plot of the mean of tvhours by year and by race group
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_point(aes(color = race), stat = "summary", fun = "mean") +
  geom_line(aes(group = race, color = race), stat = "summary", fun = "mean")+ 
  theme_bw()

## Add the points back (jittered) in to show the variation in the data and
## modify the legend title
dat |>
  ggplot(aes(x = year, y = tvhours)) +
  geom_jitter(aes(color = race), alpha = 1/10,
              width = 0.2) +
  geom_point(aes(color = race),
             stat = "summary",
             fun = "mean",
             size = 1) +
  geom_line(aes(color = race),
            stat = "summary",
            fun = "mean") +
  theme_bw()

