library(tidyverse)
library(caret)
library(plotROC)

dat <- read_csv("https://github.com/rdpeng/stat322E_public/raw/main/data/pm25_data.csv.gz")

# Create binary outcome
dat <- dat |>
  mutate(high_pm25 = ifelse(value > 12, 1, 0))

# Remove non-predictor variables
dat_model <- dat |>
  select(-id, -fips, -state, -county, -city, -value)

# Train/test split
set.seed(1)
train_index <- createDataPartition(dat_model$high_pm25, p = 0.8, list = FALSE)

train <- dat_model[train_index, ]
test  <- dat_model[-train_index, ]

# -------------------------
# Scale predictors ONLY
# -------------------------
pre <- preProcess(train |> select(-high_pm25),
                  method = c("center","scale"))

train_scaled <- train
test_scaled  <- test

train_scaled[, colnames(train_scaled) != "high_pm25"] <-
  predict(pre, train |> select(-high_pm25))

test_scaled[, colnames(test_scaled) != "high_pm25"] <-
  predict(pre, test |> select(-high_pm25))

# -------------------------
# kNN model
# -------------------------
knn_model <- knn3(
  as.factor(high_pm25) ~ .,
  data = train_scaled,
  k = 10
)

knn_prob <- predict(knn_model, test_scaled)[,2]

pred_kNN <- test_scaled |>
  select(high_pm25) |>
  mutate(
    predictions = knn_prob,
    predicted = ifelse(predictions > 0.5, 1, 0)
  )

table(pred_kNN$predicted, pred_kNN$high_pm25)

# -------------------------
# Logistic regression
# -------------------------
log_model <- glm(
  high_pm25 ~ .,
  data = train_scaled,
  family = "binomial"
)

log_prob <- predict(log_model, test_scaled, type = "response")

pred_log <- test_scaled |>
  select(high_pm25) |>
  mutate(
    predictions = log_prob,
    predicted = ifelse(predictions > 0.5, 1, 0)
  )

table(pred_log$predicted, pred_log$high_pm25)

# -------------------------
# ROC curve
# -------------------------
pred_log |>
  bind_rows(pred_kNN, .id = "model") |>
  mutate(model = ifelse(model == "1","Logistic","k-NN")) |>
  ggplot(aes(d = high_pm25,
             m = predictions,
             color = model)) +
  geom_roc() +
  coord_equal() + theme_bw()