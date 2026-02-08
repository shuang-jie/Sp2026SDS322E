library(tidyverse)

######## US rent and income data (ACS 2017) Long Data
us_rent_income
help(us_rent_income)
View(us_rent_income)


######## select variable
us_rent_income |> 
  select(NAME, variable, estimate)


######## Pivot data from long to wide
us_rent_income |> 
  select(NAME, variable, estimate) |> 
  pivot_wider(names_from = "variable",
              values_from = "estimate")


######## String added to the start of every variable name
us_rent_income |> 
  select(NAME, variable, estimate) |> 
  pivot_wider(names_from = "variable",
              names_prefix = "median_",
              values_from = "estimate")


######## scatter plot of income and rent
us_rent_income |> 
  select(NAME, variable, estimate) |> 
  pivot_wider(names_from = "variable",
              names_prefix = "median_",
              values_from = "estimate") |> 
  ggplot(aes(x = median_income, 
             y = median_rent)) + 
  geom_point() +
  labs(title = "Median annual income and monthly rent")


######## US rent and income data (ACS 2017) Long Data
billboard
help(billboard)
View(billboard)


######## Pivot data from wide to long
billboard |> 
  pivot_longer(cols = wk1:wk76,
               names_to = "week",
               values_to = "rank") 

######## remove matching text from the start of each variable name.
billboard |> 
  pivot_longer(cols = wk1:wk76,
               names_to = "week",
               names_prefix = "wk",
               values_to = "rank")

######## convert an variable to be numbers.
billboard |> 
  pivot_longer(cols = wk1:wk76,
               names_to = "week",
               names_prefix = "wk",
               values_to = "rank") |> 
  mutate(week = as.numeric(week))


######## scatter plot of rank and week
billboard |> 
  slice(1:5) |> 
  pivot_longer(cols = wk1:wk76,
               names_to = "week",
               names_prefix = "wk",
               values_to = "rank") |> 
  mutate(week = as.numeric(week)) |> 
  ggplot(aes(x = week,
             y = rank)) + 
  geom_point()


######## change color
billboard |> 
  slice(1:5) |> 
  pivot_longer(cols = wk1:wk76,
               names_to = "week",
               names_prefix = "wk",
               values_to = "rank") |> 
  mutate(week = as.numeric(week)) |> 
  ggplot(aes(x = week,
             y = rank)) + 
  geom_point(aes(color = track)) + 
  geom_line(aes(color = track)) +
  theme_bw()


######## speciation data
dat <- read_csv("https://github.com/rdpeng/stat322E_public/raw/main/data/speciation_2021.csv")


######## Unite
dat |> 
  unite(ID, 
        state, county, site, 
        sep = ".")


######## Long data
dat |> 
  unite(ID, 
        state, county, site,
        sep = ".") |> 
  pivot_longer(cols = starts_with("OC"),
               names_to = "pollutant_method",
               values_to = "value") 


######## separate method into a new variable
dat |> 
  unite(ID, 
        state, county, site,
        sep = ".") |> 
  pivot_longer(cols = starts_with("OC"),
               names_to = "pollutant_method",
               values_to = "value") |> 
  separate(col = pollutant_method, 
           into = c("pollutant", "method"),
           sep = 12)


######## separate date into year, month, day. remove = FALSE
dat |> 
  unite(ID, 
        state, county, site,
        sep = ".") |> 
  pivot_longer(cols = starts_with("OC"),
               names_to = "pollutant_method",
               values_to = "value") |> 
  separate(col = pollutant_method, 
           into = c("pollutant", "method"),
           sep = 12) |> 
  separate(col = date,
           into = c("year", "month", "day"),
           sep = "-", 
           remove = FALSE)




dat |> 
  filter(state == "48") |> 
  unite(ID, 
        state, county, site,
        sep = ".") |> 
  pivot_longer(cols = starts_with("OC"),
               names_to = "pollutant_method",
               values_to = "value") |> 
  separate(col = pollutant_method, 
           into = c("pollutant", "method"),
           sep = 12) |> 
  separate(col = date,
           into = c("year", "month", "day"),
           sep = "-", 
           remove = FALSE) |> 
  ggplot(aes(x = date, y = value)) + 
  geom_point(aes(color = method)) +
  geom_line(aes(color = method)) +
  facet_wrap(vars(ID)) +
  theme_bw()+
  labs(x = "Date", 
       y = "OC Value",
       title = "Organic Carbon Pollution")
