# Load tidyverse packages
library(tidyverse)

# Read raw commit log text file from GitHub (one line per row)
raw_data <- read_lines("https://raw.githubusercontent.com/shuang-jie/Sp2026SDS322E/main/commit_logs.txt")

# Convert character vector into a tibble with one column called 'text'
commits <- tibble(text = raw_data)

# Print the commits tibble
commits


# -------------------------
# Extract commit hashes
# -------------------------

# Filter lines that start with "commit"
commits |> 
  filter(str_detect(text, "^commit .*"))

# Count total number of commits
commits |> 
  filter(str_detect(text, "^commit .*")) |> 
  nrow()


# -------------------------
# Extract author information
# -------------------------

# Filter lines that contain author information
commits |> 
  filter(str_detect(text, "^Author:"))

# Extract email address inside angle brackets <...>
commits |> 
  filter(str_detect(text, "^Author:")) |> 
  mutate(email = str_extract(text, "<.*>"))

# Get distinct (unique) author email addresses
commits |> 
  filter(str_detect(text, "^Author:")) |> 
  mutate(email = str_extract(text, "<.*>")) |> 
  select(email) |> 
  distinct() 


# -------------------------
# Extract author names
# -------------------------

# Use regex to capture author's first name (letters only)
# str_match() returns a matrix: column 1 = full match, column 2 = captured group
commits |> 
  mutate(author = str_match(text, "^Author: ([a-zA-Z]+) <.*>"))

# Keep only the captured name (second column of matrix)
commits |> 
  mutate(author = str_match(text, "^Author: ([a-zA-Z]+) <.*>")) |> 
  mutate(author = author[,2])

# Remove rows where author is NA (non-author lines)
commits |> 
  mutate(author = str_match(text, "^Author: ([a-zA-Z]+) <.*>")) |> 
  mutate(author = author[,2]) |> 
  filter(!is.na(author))


# -------------------------
# Extract and parse dates
# -------------------------

# Filter date lines and remove "Date:" prefix
commits |> 
  filter(str_detect(text, "^Date:")) |> 
  mutate(datetime = str_replace(text, "^Date: +", ""))


# Parse datetime and visualize distribution
commits |> 
  filter(str_detect(text, "^Date:")) |> 
  mutate(datetime = str_replace(text, "^Date: +", "")) |> 
  select(datetime) |> 
  mutate(datetime = parse_date_time(datetime, 
                                    orders = "a b d H:M:S Y z",
                                    tz = "America/New_York")) |> 
  ggplot(aes(x = datetime)) + 
  geom_histogram(bins = 20, fill = "white", color = "black") +
  labs(x = "Commit date/time", 
       y = "Count", 
       title = "Number of Commits Over Time") + 
  theme_bw()
