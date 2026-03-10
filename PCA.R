# Load the tidyverse package (includes readr, dplyr, ggplot2, tibble, etc.)
library(tidyverse)

# Read compressed dataset from GitHub
# Keep only columns that start with "GROUP"
# Then keep the first 20 of those columns
dat <- read_csv("https://raw.githubusercontent.com/shuang-jie/Sp2026SDS322E/main/raw_anonymized_data.csv.gz") |> 
  select(starts_with("GROUP")) |> 
  select(1:20)

# Show the structure of the dataset
dat |> glimpse()

# Display columns 11 to 13 of the dataset
dat |> select(11:13)


# Perform Principal Component Analysis (PCA)
# Convert tibble to numeric matrix since prcomp() expects numeric input
pca_result <- dat |> 
  data.matrix() |> 
  prcomp()

# Print the PCA object (contains rotation, scores, etc.)
pca_result

# Show importance of each principal component
summary(pca_result)


# Basic PCA diagnostic plots
plot(pca_result)


# Convert PCA scores (coordinates of observations in PC space) to tibble
# Plot PC1 vs PC2 to visualize the main variation in the data
as_tibble(pca_result$x) |> 
  ggplot(aes(PC1, PC2)) + 
  geom_point() + 
  theme_bw()


# Plot PC2 vs PC3 to explore additional variation patterns
as_tibble(pca_result$x) |> 
  ggplot(aes(PC2, PC3)) + 
  geom_point() + 
  theme_bw()


# Extract PCA loadings (contribution of each variable to components)
as_tibble(pca_result$rotation, 
          rownames = "variable") |> 
  ggplot(aes(variable, PC1)) +
  geom_point() +
  coord_flip() +      
  theme_bw()


# Plot variable loadings for PC2
as_tibble(pca_result$rotation, 
          rownames = "variable") |> 
  ggplot(aes(variable, PC2)) +
  geom_point() +
  coord_flip() + 
  theme_bw()


# Plot variable loadings for PC3
as_tibble(pca_result$rotation, 
          rownames = "variable") |> 
  ggplot(aes(variable, PC3)) +
  geom_point() +
  coord_flip() + 
  theme_bw()

