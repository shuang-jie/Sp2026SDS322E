# Load the tidyverse package
library(tidyverse)

# Read the asthma symptoms dataset
dat <- read_csv("https://raw.githubusercontent.com/shuang-jie/Sp2026SDS322E/main/asthma_symptoms.csv")
dat

# Run K-means clustering with 3 clusters
kresult <- kmeans(dat, 3)

# installed tidymodels package
if (!require("tidymodels")) {
  install.packages("tidymodels")
  library(tidymodels)
}
# shows cluster centers and cluster assignment
tidy(kresult)


# Add cluster assignments to each observation in the dataset
augment(kresult, dat)


augment(kresult, dat) |> 
  ggplot(aes(sxsgeneral, sxsslowed)) +      
  geom_jitter(aes(color = .cluster),        
              width = 0.2, height = 0.2,    
              size = 3) + 
  theme_bw()                                

tidy(kresult) |> select(-size, -withinss)


# Convert cluster centers from wide format to long format
tidy(kresult) |>
  select(-size, -withinss) |> 
  pivot_longer(-cluster,                    
               names_to = "symptom",        
               values_to = "value")         



# Plot cluster centers for each symptom
tidy(kresult) |>
  select(-size, -withinss) |> 
  pivot_longer(-cluster,
               names_to = "symptom", 
               values_to = "value") |> 
  ggplot(aes(symptom, value)) + 
  geom_col() +                              
  facet_wrap(vars(cluster)) +               
  coord_flip() +                            
  theme_bw()



### Hierarchical Clustering Example

set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
dat <- tibble(x = x, y = y)


# Install and load ggrepel package
if (!require("ggrepel")) {
  install.packages("ggrepel")
  library(ggrepel)
}

# Plot the points and label them
dat |> 
  ggplot(aes(x, y)) + 
  geom_point(size = 4) + 
  geom_text_repel(aes(label = 1:12)) +      
  coord_fixed() +                           
  theme_bw()

dist(dat)


if (!require("factoextra")) {
  install.packages("factoextra")
  library(factoextra)
}


# Determine optimal number of clusters using WSS (Elbow method)
dat |> fviz_nbclust(kmeans, method = "wss", k.max = 11)

# Determine optimal clusters using Silhouette method
dat |> fviz_nbclust(kmeans, method = "silhouette", k.max = 11)

# Determine optimal clusters using Gap statistic
dat |> fviz_nbclust(kmeans, method = "gap", k.max = 11)



# Reload the asthma dataset
dat <- read_csv("https://raw.githubusercontent.com/shuang-jie/Sp2026SDS322E/main/asthma_symptoms.csv")

# Apply cluster selection methods to the asthma data
dat |> fviz_nbclust(kmeans, method = "wss")
dat |> fviz_nbclust(kmeans, method = "gap")
dat |> fviz_nbclust(kmeans, method = "silhouette")



# Run k-means clustering with 2 clusters
k <- kmeans(dat, 2)

# Visualize symptom distributions for each cluster
k |> 
  augment(dat) |>                           
  pivot_longer(starts_with("sxs")) |>       
  ggplot(aes(x = name, y = value)) + 
  geom_jitter(width = 0.1, alpha = 1/4) +   
  facet_wrap(vars(.cluster)) +              
  coord_flip() + 
  theme_bw()



# Run k-means clustering with 6 clusters
k <- kmeans(dat, 6)

# Visualize symptom distributions again for 6 clusters
k |> 
  augment(dat) |> 
  pivot_longer(starts_with("sxs")) |> 
  ggplot(aes(x = name, y = value)) + 
  geom_jitter(width = 0.1, alpha = 1/4) +
  facet_wrap(vars(.cluster)) +
  coord_flip() +
  theme_bw()


# Plot the cluster centers (mean symptom values) for each cluster
k |> 
  tidy() |> 
  pivot_longer(starts_with("sxs")) |> 
  ggplot(aes(name, value)) + 
  geom_col() +               
  facet_wrap(vars(cluster)) + 
  coord_flip() + 
  theme_bw()
