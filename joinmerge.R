## Joining and Merging

library(tidyverse)

base_url <- "https://raw.githubusercontent.com/shuang-jie/Sp2026SDS322E/main/"

subject  <- read_csv(paste0(base_url, "subject.csv"))
symptoms <- read_csv(paste0(base_url, "symptoms.csv"))
ige      <- read_csv(paste0(base_url, "ige.csv"))
housing  <- read_csv(paste0(base_url, "housing.csv"))


subject
symptoms
ige
housing

################################################################################

## Bar plot of different housing types
subject |> 
  ggplot(aes(x = hometype)) + 
  geom_bar()

## Bar plot of proportion of cat allergic by housing type
subject |> 
  ggplot(aes(x = hometype, y = catpos)) + 
  geom_bar(stat = "summary", fun = "mean")


## left_join
subject |> 
  left_join(housing, by = "hometype")
  

## Bar plot of proportion of cat allergic by housing type (labelled)
subject |> 
  left_join(housing, by = "hometype") |> 
  ggplot(aes(x = label, y = catpos)) + 
  geom_bar(stat = "summary", fun = "mean") + 
  labs(x = NULL, 
       y = "Proportion Cat Allergic")

## Merge IgE with housing types
ige

## left_join ige and subject
ige |> 
  filter(visit == 0) |> 
  select(-visit) |> 
  left_join(subject, by = "ID")


## further left_join housing
ige |> 
  filter(visit == 0) |> 
  select(-visit) |> 
  left_join(subject, by = "ID") |> 
  left_join(housing, by = "hometype") 


## Plot IgE by housing type
ige |> 
  filter(visit == 0) |> 
  select(-visit) |> 
  left_join(subject, by = "ID") |> 
  left_join(housing, by = "hometype") |> 
  ggplot(aes(x = label, y = IgE)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.3)

## Demonstrate a left join
symptoms |> 
  left_join(ige, by = c("ID", "visit"))

## Use inner join
symptoms |> 
  inner_join(ige, by = c("ID", "visit"))

## Plot symptoms vs. IgE
symptoms |> 
  inner_join(ige, by = c("ID", "visit")) |> 
  ggplot(aes(x = IgE, y = symptoms)) + 
  geom_point(size = 3)


## Full join
set.seed(1)
x <- tibble(a = 4:6,
            b = rnorm(3))
x
y <- tibble(a = 7:9,
            c = runif(3))
y

x |> 
  left_join(y, by = "a")

x |> 
  inner_join(y, by = "a")

x |> 
  right_join(y, by = "a")

x |> 
  full_join(y, by = "a")


## Anti Join
symptoms |> 
  anti_join(ige, by = c("ID", "visit"))

