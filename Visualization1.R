## Load the tidyverse package
library(tidyverse)

## Read in the data from the web
dat = read_csv("https://raw.githubusercontent.com/rdpeng/stat322E_public/main/data/maacs.csv")

## Summarize the dataset with glimpse()
glimpse(dat)

## Define sub data (If you use the same name, then dat will change!)
dat_sub = dat |> select(eno, id)

## Print out the first few rows of the dataset
dat

## Print out the last few rows of the dataset
dat |> tail(10)

## Sample a random 10 rows from the dataset
dat |> sample_n(10)


## Summarize variables
dat |> summary()


################################################################################
## Draw a blank canvas with ggplot()
dat |> ggplot()


## Set the frame of the plot using the eno variable but don't plot any data
## (need to add a geom)
dat |> ggplot(aes(x = eno))



## Histogram of eno variable
dat |> ggplot(aes(x = eno)) + geom_histogram(bins = 10)


## Increase the number of bins to see what difference it makes
dat |>
  ggplot(aes(x = eno)) +
  geom_histogram(bins = 15)


## Make a histogram of pm25
dat |>
  ggplot(aes(x = pm25)) +
  geom_histogram(bins = 20)


## Histogram colored purple (fill = "purple")
dat |> ggplot(aes(x = eno)) +
  geom_histogram(fill = "purple", bins = 15)


## Histogram colored (bins fill color = "white", bins boundary color = "black")
dat |> ggplot(aes(x = eno)) +
  geom_histogram(fill = "white", color="black",  bins = 15)


## Histogram colored (bins fill color = "white", bins boundary color = "black")
dat |> ggplot(aes(x = eno)) +
  geom_histogram(fill = "lightblue", color="black",  bins = 15) + theme_classic()


## Check allergic status variable
dat |> count(mopos)


## Scatterplot of pm25 and eno
dat |>
  ggplot(aes(x = pm25, y = eno)) +
  geom_point() + theme_classic()


## Scatterplot of pm25 and eno with all points colored red
dat |>
  ggplot(aes(x = pm25, y = eno)) +
  geom_point(color = "red") + 
  theme_classic()


## Scatterplot of pm25 and eno with points colored by mouse allergic status
dat |>
  ggplot(aes(x = pm25, y = eno)) +
  geom_point(aes(color = mopos)) + 
  theme_classic()


## Increase the point size, adjust alpha
dat |>
  ggplot(aes(x = pm25, y = eno)) +
  geom_point(aes(color = mopos), 
             size = 3, alpha = 1/2)+ 
  theme_classic()



## Histogram of eno colored by mouse allergic status
dat |>
  ggplot(aes(x = eno)) +
  geom_histogram(aes(fill = mopos), bins = 15)+ 
  theme_classic()

