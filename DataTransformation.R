## Data Transformation Exercise

## Amazon Digital Software Reviews Data

library(tidyverse)
## download the file into local device
## https://drive.google.com/file/d/1NEWZZBXL2dLhe-6u6gOtiuMd1_0cn2NS/view?usp=drive_link
## download it and save it to downloads

## If you are using mac OS and save downloaded data file under Downloads folder
amazon = read_tsv("~/Downloads/amazon.tsv", col_types = "cccccccdddccccD")

## If you are using windows and save downloaded data file under Downloads folder
# amazon_reviews_us_Pet_Products_v1_00 = read_tsv("C:/Users/YourWindowsUserName/Downloads/amazon_reviews_us_Pet_Products_v1_00.tsv")
amazon = read_tsv("C:/Users/YourWindowsUserName/Downloads/amazon.tsv", col_types = "cccccccdddccccD")

## glimpse
glimpse(amazon)

## View
View(amazon)

## subset data
dat <- amazon |> 
  select(customer_id, 
         product_id:product_title,
         star_rating,
         starts_with("review")) |> 
  rename(rating = star_rating)

## remove review columns
dat <- dat |> 
  select(-review_body, -review_id)


## Look at the distribution of star ratings
dat |> 
  group_by(rating) |> 
  summarize(n = n())

## Create a new variable in the summarized data frame that is the percentage of all ratings with that number of stars
dat |> 
  group_by(rating) |> 
  summarize(n = n()) |> 
  mutate(pct = 100 * n / sum(n))

## Sort rows in increasing order as determined by the 'pct' column
dat |> 
  filter(!is.na(rating)) |> 
  group_by(rating) |> 
  summarize(n = n()) |> 
  mutate(pct = 100 * n / sum(n)) |> 
  arrange(pct)

## Sort rows in descending order as determined by the 'pct' column
dat |> 
  filter(!is.na(rating)) |> 
  group_by(rating) |> 
  summarize(n = n()) |> 
  mutate(pct = 100 * n / sum(n)) |> 
  arrange(desc(pct))

## Sample one row at random
dat |> sample_n(1)

## Filter to rows that just review this product
dat |> 
  filter(product_id == "0763004227")

## Count the number of reviews for each product
dat |> 
  group_by(product_id) |> 
  summarize(n = n())

## Summarize the distribution of the number of reviews per product
dat |> 
  group_by(product_id) |> 
  summarize(n = n()) |> 
  select(n) |> 
  summary()
## Half of all products have 2 or fewer reviews


## Identify Products That Have More Than 2 Reviews
prodlist <- dat |> 
  group_by(product_id) |> 
  summarize(n = n()) |> 
  filter(n > 2) |> 
  pull(product_id)

## Identify Users Who Have Reviewed More Than 1 Product
dat |> 
  group_by(customer_id) |> 
  summarize(n = n()) 

## Summarize the distribution of the number of reviews per customer
dat |> 
  group_by(customer_id) |> 
  summarize(n = n()) |> 
  select(n) |> 
  summary()

## Identify Users Who Have Reviewed More Than 1 Product
userlist <- dat |> 
  group_by(customer_id) |> 
  summarize(n = n()) |> 
  filter(n > 1) |> 
  pull(customer_id)

## Subset the Data!
## Keep rows where the value of 'customer_id' is contained in the vector 'userlist'
## AND
## Keep rows where the value of 'product_id' is contained in the vector 'prodlist' 
ratings <- dat |> 
  filter(customer_id %in% userlist 
         & product_id %in% prodlist)

## Set random number generator seed with an integer to make sampling reproducible
set.seed(2023)
ratings |>
  sample_n(1) |>
  select(customer_id, product_id,
         rating, product_title)

## Which of the other people are most similar?
ratings |>
  filter(product_id == "B001U4M9I6") |>
  select(customer_id, rating,
         review_headline, review_date)

## Characterize Other Customers
ratings |>
  filter(customer_id %in% c("45464833", "50994974",
                            "41861197", "12485248")) |>
  group_by(customer_id) |>
  summarize(mean_rating = mean(rating),
            num_rating = n())

## Find Other Products
ratings |> 
  filter(customer_id %in% c("50994974", "41861197", "12485248")) |> 
  filter(rating >= 4) |> 
  arrange(customer_id) |> 
  select(customer_id, product_id, 
         rating, product_title)

## Just return rows 15--17 from the previous table
ratings |>
  filter(customer_id %in% c("50994974", "41861197", "12485248")) |>
  filter(rating >= 4) |>
  arrange(customer_id) |>
  select(customer_id, product_id,
         rating, product_title) |>
  slice(15:17)