library(tidyverse)
dat <- read_csv("https://github.com/rdpeng/SDS322E_public/raw/refs/heads/main/data/PHT_az_ca.csv.gz")
dat

## cor pm25 and rate
dat |> 
  filter(population > 0) |> 
  mutate(rate = cases / population) |> 
  filter(!is.na(pm25) & !is.na(rate)) |> 
  summarize(cor = cor(pm25, rate))


## mutate calculate rate = case / population
dat |> 
  filter(population > 0) |> 
  mutate(rate = cases / population) |> 
  filter(!is.na(pm25) & !is.na(rate)) |> 
  ggplot(aes(pm25, rate)) + 
  geom_point()


## scatter plot pm25 and rate
dat |> 
  filter(population > 0) |> 
  mutate(rate = cases / population) |> 
  filter(!is.na(pm25) & !is.na(rate)) |> 
  ggplot(aes(pm25, rate)) + 
  geom_point()

## box plot pm25 each urc
## use factor function to convert urc to a categorical variable
dat |> 
  filter(population > 0) |> 
  mutate(rate = cases / population) |> 
  filter(!is.na(pm25) & !is.na(rate)) |> 
  mutate(urc = factor(urc)) |> 
  ggplot(aes(urc, pm25)) + 
  geom_boxplot()


## box plot rate each urc 
dat |> 
  filter(population > 0) |> 
  mutate(rate = cases / population) |> 
  filter(!is.na(pm25) & !is.na(rate)) |> 
  mutate(urc = factor(urc)) |> 
  ggplot(aes(urc, rate)) + 
  geom_boxplot() 


