library(tidymodels)
library(broom)
library(ggplot2)
library(dplyr)

# -----------------------------
# Basic Linear Regression
# -----------------------------
fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
tidy(fit)

# Natural spline example
fit <- lm(Ozone ~ splines::ns(Wind, 2) + Temp + Solar.R, data = airquality)
tidy(fit)

# -----------------------------
# Tidymodels Linear Regression
# -----------------------------
rec <- airquality |>
  recipe(Ozone ~ Wind + Temp + Solar.R)

model <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

model_fit <- fit(wf, data = airquality)
tidy(model_fit)

# -----------------------------
# Add preprocessing
# -----------------------------
rec <- airquality |>
  recipe(Ozone ~ Wind + Temp + Solar.R) |>
  step_normalize(Wind, Temp, Solar.R) |>
  step_ns(Wind, deg_free = 2)

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

model_fit <- fit(wf, data = airquality)
tidy(model_fit)

# -----------------------------
# Check processed data
# -----------------------------
dat_model <- rec |>
  prep(airquality) |>
  bake(new_data = NULL)
dat_model

# Predictions
model_fit |>
  extract_fit_parsnip() |>
  augment(new_data = dat_model)

# Prediction plot
model_fit |>
  extract_fit_parsnip() |>
  augment(new_data = dat_model) |>
  ggplot(aes(.pred, Ozone)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# -----------------------------
# Logistic Regression Example
# -----------------------------
rec <- airquality |>
  recipe(Ozone ~ Wind + Temp + Solar.R) |>
  step_naomit(Ozone) |>
  step_cut(Ozone, breaks = 70) |>
  step_normalize(Wind, Temp, Solar.R) |>
  step_ns(Wind, deg_free = 2)

model <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

model_fit <- fit(wf, data = airquality)
tidy(model_fit)

# -----------------------------
# Cross Validation (Regression)
# -----------------------------
rec <- airquality |>
  recipe(Ozone ~ Wind + Temp + Solar.R)

model <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

folds <- vfold_cv(airquality, v = 5)
folds
model_fit <- fit_resamples(wf, resamples = folds)
model_fit |> collect_metrics()

# -----------------------------
# Logistic CV
# -----------------------------
rec <- airquality |>
  recipe(Ozone ~ Wind + Temp + Solar.R) |>
  step_naomit(Ozone) |>
  step_cut(Ozone, breaks = 70)

model <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

folds <- vfold_cv(airquality, v = 5)

model_fit <- fit_resamples(wf, resamples = folds)

model_fit |> collect_metrics()

# -----------------------------
# Decision Tree Model
# -----------------------------
model <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

model_fit <- fit_resamples(wf, resamples = folds)

model_fit |> collect_metrics()

# -----------------------------
# Hyperparameter Tuning (Single Parameter)
# -----------------------------
rec <- airquality |>
  recipe(Ozone ~ Wind + Temp + Solar.R) |>
  step_ns(Wind, deg_free = tune("df"))

model <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

res <- tune_grid(
  wf,
  resamples = folds,
  grid = tibble(df = c(2,4,6,8))
)

res |> collect_metrics()

# -----------------------------
# Multiple Parameter Tuning (FIXED)
# -----------------------------
rec <- airquality |>
  recipe(Ozone ~ Wind + Temp + Solar.R) |>
  step_normalize(Wind, Temp, Solar.R) |>
  step_ns(Wind, deg_free = tune("df_wind")) |>
  step_ns(Temp, deg_free = tune("df_temp"))

model <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# IMPORTANT: rebuild workflow with new recipe
wf <- workflow() |>
  add_recipe(rec) |>
  add_model(model)

res <- tune_grid(
  wf,
  resamples = folds,
  grid = expand.grid(
    df_wind = c(2,4,6,8),
    df_temp = c(2,4,6,8)
  ),
  control = control_grid(save_pred = TRUE)
)

# View tuning results
res |> collect_metrics()

# Best results
res |> show_best(metric = "rmse")

# Select best parameters
res |> select_best(metric = "rmse")

