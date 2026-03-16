## Prediction metrics

library(tidyverse)
library(broom)
library(plotROC)
library(caret)

## Fashion MNIST
dat <- read_csv("https://github.com/shuang-jie/Sp2026SDS322E/raw/main/fashion-mnist_sample.csv.gz")
dat

## Show one image
dat |> 
  filter(label == 5) |> 
  slice(2) |>   ## Show the 1st image (change this to see others)
  select(-label) |>  ## Remove the label column (don't need it for now)
  pivot_longer(everything()) |> ## Pivot to long form
  bind_cols(expand_grid(x = 1:28, y = 1:28)) |>  ## Combined row/column numbers
  ggplot(aes(x = y, y = x)) +   ## Plot it so that it's right side up
  geom_raster(aes(fill = value)) +
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(low = "white", high = "black") +
  coord_fixed() +
  theme_bw() +
  scale_y_reverse() +
  theme(legend.position = "none")

## Subset data and feature extraction
dat <- dat |> 
  filter(label %in% c(5, 6)) |> ## Restrict to sandals and shirts
  select(label, where(~ mean(.x) > 100))  ## Keep columns with larger numbers
dat           

## Proportion of outcomes
dat |> 
  summarize(proportion = mean(label == 5))

## Recode outcomes
dat <- dat |> 
  mutate(label = ifelse(label == 5, 0, 1)) # 0 = sandal; 1 = shirt
dat

## Logistic regression model
pm25_log <- glm(label ~ ., data = dat, 
                family = binomial)
summary(pm25_log)

## See tidy data frame of coefficients
pm25_log |> 
  tidy()

## Sort coefficients by size (descending)
pm25_log |> 
  tidy() |> 
  arrange(desc(estimate))

## Visualize one pixel
dat |> 
  ggplot(aes(pixel273, label)) + 
  geom_point(alpha = 1/15) + 
  geom_smooth() +
  scale_y_continuous(breaks = c(0, 1))

## Store predictions
pred_log <- dat |> 
  select(label) |> 
  mutate(predictions = predict(pm25_log, dat, type = "response")) |> 
  mutate(predicted = ifelse(predictions > 0.5, 1, 0))
pred_log

## Confusion matrix
table(predicted = pred_log$predicted, 
      truth = pred_log$label)

## Success rate for shirt (Positive predicted value)?
928 / (928 + 92)



## Scale data
dat_scaled <- dat |> 
  mutate(across(-label, scale))
dat_scaled

## Try knn
pm25_kNN <- knn3(label ~ ., data = dat_scaled, k = 5)

predict(pm25_kNN, dat_scaled) |> 
  head()

## Predictions
pred_kNN <- dat |> 
  select(label) |> 
  mutate(predictions = predict(pm25_kNN, dat_scaled)[, 2]) |> 
  mutate(predicted = ifelse(predictions > 0.5, 1, 0))
pred_kNN

## Confusion matrix
table(predicted = pred_kNN$predicted, 
      truth = pred_kNN$label)

## PPV for shirt
998 / (998 + 273)


## ROC curves
bind_rows(pred_log, 
          pred_kNN, 
          .id = "model") |> 
  mutate(model = ifelse(model == "1", "Logistic", "k-NN")) |> 
  ggplot(aes(d = label, 
             m = predictions, 
             color = model)) + 
  geom_roc(n.cuts = 10) 


## Predictions vs. truth for logistic
pred_log |> 
  ggplot(aes(predictions, label)) + 
  geom_jitter(height = 0.1, alpha = 1/5) +
  labs(x = "Model predictions",
       y = "Truth (0/1)") +
  scale_y_continuous(breaks = c(0, 1))


## Plot predictions vs truth for k-NN
pred_kNN |> 
  ggplot(aes(predictions, label)) + 
  geom_jitter(height = 0.1, alpha = 1/5) +
  labs(x = "Model predictions",
       y = "Truth (0/1)") +
  scale_y_continuous(breaks = c(0, 1))


## Get fresh test dataset
test <- read_csv("https://github.com/rdpeng/stat322E_public/raw/main/data/fashion_test2.csv.gz")
test <- test |> 
  mutate(label = ifelse(label == 5, 0, 1))

## Logistic predictions
pred_log <- test |> 
  select(label) |> 
  mutate(predictions = predict(pm25_log, test, type = "response")) |> 
  mutate(predicted = ifelse(predictions > 0.5, 1, 0))
pred_log

## Confusion matrix
table(predicted = pred_log$predicted, 
      truth = pred_log$label)

## Scale the data for k-NN model
test_scaled <- test |> 
  mutate(across(-label, scale))

## k-NN predictions
pred_kNN <- test |> 
  select(label) |> 
  mutate(predictions = predict(pm25_kNN, test_scaled)[, 2]) |> 
  mutate(predicted = ifelse(predictions > 0.5, 1, 0))
pred_kNN

## Confusion matrix
table(predicted = pred_kNN$predicted, 
      truth = pred_kNN$label)

## ROC curves on new test data
bind_rows(pred_log, 
          pred_kNN, 
          .id = "model") |> 
  mutate(model = ifelse(model == "1", "Logistic", "k-NN")) |> 
  ggplot(aes(d = label, m = predictions, color = model)) + 
  geom_roc(n.cuts = 10) 

## Logistic predictions vs. truth
pred_log |> 
  ggplot(aes(predictions, label)) + 
  geom_jitter(height = 0.1, alpha = 1/2) +
  labs(x = "Model predictions",
       y = "Truth (0/1)") +
  scale_y_continuous(breaks = c(0, 1))

## k-NN predictions vs. truth
pred_kNN |> 
  ggplot(aes(predictions, label)) + 
  geom_jitter(height = 0.1, alpha = 1/2) +
  labs(x = "Model predictions",
       y = "Truth (0/1)") +
  scale_y_continuous(breaks = c(0, 1))

