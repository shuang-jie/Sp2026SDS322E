## String processing

library(tidyverse)

## In what US states are WCA competitions held?

## Read in WCA competitions data
competitions <- read_tsv("https://raw.githubusercontent.com/shuang-jie/Sp2026SDS322E/main/WCA_export_Competitions.tsv.bz2")

## Look at top of data frame
competitions

## Look at all data
competitions |>
    glimpse()

## In what US states are competitions held?
competitions |>
    filter(countryId == "USA") |>
    select(starts_with("venue"))

## Restrict columns
competitions |>
    filter(countryId == "USA") |>
    select(matches("venue[a-zA-Z]+"))

## Take a random sample of competitions
set.seed(2026-02-21)
competitions |>
    filter(countryId == "USA") |>
    select(matches("venue[a-zA-Z]+")) |>
    sample_n(20)

## Find competitions in Austin, TX
competitions |>
    filter(countryId == "USA") |>
    select(name, venueAddress) |>
    filter(str_detect(venueAddress, "Austin"))

## Find competitions in Austin, TX
competitions |>
    filter(countryId == "USA") |>
    filter(str_detect(venueAddress,
                      "Austin, TX|Austin, Texas|Austin TX|Austin Texas")) |>
    select(name, venueAddress, year)

## Find competitions in Houston, TX
competitions |>
    filter(countryId == "USA") |>
    filter(str_detect(venueAddress,
                      "Houston, TX|Houston, Texas|Houston TX|Houston Texas")) |>
    select(name, venueAddress, year)

## How many competitions are there in each U.S. state?

## Select some random rows to get the pattern
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    sample_n(20)

## Remove the ZIP code
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |>
    select(state) |>
    sample_n(20)


## Remove trailing 'USA's
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |>
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |>
    select(state) |>
    sample_n(20)

## Remove the street address
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |>
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |>
    select(state) |>
    mutate(state = str_replace(state, "^.*, ", ""))

## Clean up
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |>
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |>
    mutate(state = str_replace(state, "^.*, ", "")) |>
    mutate(state = str_replace(state, ",$", "")) |>
    select(state) |>
    sample_n(20)

## Count the number of competitions in each state
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |>
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |>
    mutate(state = str_replace(state, "^.*, ", "")) |>
    mutate(state = str_replace(state, ",$", "")) |>
    filter(state %in% state.name
           | state %in% state.abb) |>
    select(state) |>
    group_by(state) |>
    summarize(n = n()) |>
    arrange(state)

## Bar plot of number of competitions in each state
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |>
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |>
    mutate(state = str_replace(state, "^.*, ", "")) |>
    mutate(state = str_replace(state, ",$", "")) |>
    filter(state %in% state.name
           | state %in% state.abb) |>
    group_by(state) |>
    summarize(n = n()) |>
    ggplot(aes(x = n, y = state)) +
    geom_col()



## Bar plot of number of competitions in each state w/states ordered
competitions |>
    filter(countryId == "USA") |>
    filter(!is.na(venueAddress)) |>
    select(venueAddress) |>
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |>
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |>
    mutate(state = str_replace(state, "^.*, ", "")) |>
    mutate(state = str_replace(state, ",$", "")) |>
    filter(state %in% state.name
           | state %in% state.abb) |>
    group_by(state) |>
    summarize(n = n()) |>
    mutate(state = fct_reorder(state, n)) |>
    ggplot(aes(y = state, x = n)) +
    geom_col()



