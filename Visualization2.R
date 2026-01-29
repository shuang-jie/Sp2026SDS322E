## Load tidyverse (as usual)
library(tidyverse)

dat = read_csv("https://raw.githubusercontent.com/rdpeng/stat322E_public/main/data/maacs.csv")

## Adjust the size of the circle by the amount of bed mouse allergen
dat |>
  ggplot(aes(x = pm25, y = eno)) +
  geom_point(aes(size = duBedMusM), alpha = 1/4) +
  facet_wrap(vars(mopos)) +
  labs(x = "PM2.5") +
  labs(y = "Exhaled nitric oxide") +
  labs(title = "Exhaled Nitric Oxide and PM2.5 by Mouse Allergic Status") + 
  theme_bw()


## Make boxplot different plots
dat |>
  ggplot(aes(x = mopos, y = duBedMusM)) +
  geom_boxplot()+ 
  theme_bw()

## Make boxplot separate plots
dat |>
  ggplot(aes(y = duBedMusM)) +
  geom_boxplot()+
  facet_wrap(vars(mopos))+ 
  theme_bw()

## Save the previous plot to a separate R object
g = dat |>
  ggplot(aes(y = duBedMusM)) +
  geom_boxplot()+
  facet_wrap(vars(mopos))+ 
  theme_bw()

## Show the previous plot
g


#################################### Visualizing Distributions and Categorical Data With ggplot2 ####################################

## New Functions covered:
# geom_bar()
# coord_flip()
# scale_y_continuous()
# facet_grid()
# geom_jitter()
# geom_smooth()
# filter() with %in% and logical operators

#### new data set gss_cat

## Quick look at the data with glimpse()
glimpse(gss_cat)

## Check the first few rows of the dataset
head(gss_cat)

## Check the *last* few rows of the dataset
tail(gss_cat)

## Make a barplot of the rincome variable
gss_cat |> 
  ggplot(aes(x = rincome)) + 
  geom_bar(fill = "sienna2") + 
  theme_bw()

## Make a barplot of the rincome variable switching axes
gss_cat |> 
  ggplot(aes(x = rincome)) + 
  geom_bar(fill = "sienna2") +
  coord_flip()+ 
  theme_bw()


## Make a barplot of the rincome variable switching axes
gss_cat |> 
  ggplot(aes(y = rincome)) + 
  geom_bar(fill = "sienna2") +
  theme_bw()

## Make categories of age then make a bar plot of categories
gss_cat |>
  mutate(agecat = cut_interval(age, 4)) |>
  ggplot(aes(x = agecat)) +
  coord_flip()+ 
  geom_bar()


## Make the same barplot but with NAs filtered out
gss_cat |>
  filter(!is.na(age)) |> 
  mutate(agecat = cut_interval(age, 4)) |>
  ggplot(aes(y = agecat)) +
  geom_bar()


## Make a barplot of marital status
gss_cat |> 
  ggplot(aes(x = marital)) + 
  geom_bar()

## Make a barplot of marital status with proportions
gss_cat |> 
  ggplot(aes(x = marital)) +
  geom_bar(aes(y = after_stat(prop), 
               group = 1))

## Make a barplot of marital status with percentages
gss_cat |> 
  ggplot(aes(x = marital)) +
  geom_bar(aes(y = after_stat(prop), 
               group = 1)) +
  scale_y_continuous(breaks = c(0, 0.4),
                     labels = c("0%", "40%")) +
  labs(y = "Percentage")


## Make a barplot of marital status with percentages using 'scales' package
gss_cat |> 
  ggplot(aes(x = marital)) +
  geom_bar(aes(y = after_stat(prop), 
               group = 1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Percentage")


## overview marital and tvhours
gss_cat |> 
  select(marital, tvhours) |> 
  glimpse()
        
## Make a scatter plot of tvhours vs age
gss_cat |> 
  ggplot(aes(x = marital, y = tvhours)) +
  geom_point()

## Make a scatterplot of tvhours vs age with jitter
gss_cat |> 
  ggplot(aes(x = marital, y = tvhours)) +
  geom_jitter()

## Make a histogram facet plot of tvhours by marital
gss_cat |> 
  ggplot(aes(x = tvhours)) + 
  geom_histogram(aes(y = after_stat(density)),
                 bins = 8) +
  facet_wrap(vars(marital))

## Make a boxplot of tvhours by marital
gss_cat |> 
  filter(marital != "No answer") |> 
  ggplot(aes(x = marital, y = tvhours)) + 
  geom_boxplot()

## overview age and tvhours
gss_cat |> 
  select(age, tvhours) |> 
  glimpse()

## Make a scatterplot of tvhours vs age point 
gss_cat |> 
  ggplot(aes(x = age, y = tvhours)) +
  geom_point() 

## Make a scatterplot of tvhours vs age with jitter
gss_cat |> 
  ggplot(aes(x = age, y = tvhours)) +
  geom_jitter() 

## Make a scatterplot of tvhours vs age with jitter and smooth
gss_cat |> 
  ggplot(aes(x = age, y = tvhours)) +
  geom_jitter() +
  geom_smooth()



## Make a smooth of tvhours vs age (no points)
gss_cat |> 
  ggplot(aes(x = age, y = tvhours)) +
  geom_smooth()


