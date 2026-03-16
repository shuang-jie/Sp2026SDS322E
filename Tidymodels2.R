## Case studies

library(tidyverse)
library(tidymodels)

## Simple example on mtcars data
dat_split <- initial_split(mtcars)
dat_split

## Training dataset
dat_train <- training(dat_split)
dat_train

## Testing dataset (save for later)
dat_test <- testing(dat_split)
dat_test


dat_train |>
    ggplot(aes(hp, mpg)) +
    geom_point()


## Start with linear regression
rec <- dat_train |>
    recipe(mpg ~ cyl + disp + hp + wt)
rec

model <- linear_reg() |>
    set_engine("lm") |>
    set_mode("regression")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)
wf

## Check performance using cross-validation
folds <- vfold_cv(dat_train, v = 10)
folds
res <- wf |>
    fit_resamples(resamples = folds)
res |>
    collect_metrics()

fit <- wf |>
    fit(data = dat_train)

fit |>
    extract_fit_parsnip() |>
    augment(dat_train) |>
    ggplot(aes(.pred, mpg)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

## At this point we could try different models (i.e. k-NN, random forest)

## Settled on a model; do final estimate of performance
final <- wf |>
    last_fit(split = dat_split)
final |>
    collect_metrics()

## Plot observed vs. predicted
final |>
    extract_fit_parsnip() |>
    augment(dat_test) |>
    ggplot(aes(.pred, mpg)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

################################################################################

dat0 <- read_csv("https://github.com/rdpeng/stat322E_public/raw/main/data/planet.csv.gz")
dat0

## Show one image
dat0 |>
    slice(500) |>   ## Show the 1st image (change this to see others)
    select(-pmFRM) |>  ## Remove the label column (don't need it for now)
    pivot_longer(everything()) |> ## Pivot to long form
    bind_cols(expand_grid(x = 1:64, y = 1:64)) |>  ## Combined row/column numbers
    ggplot(aes(x = y, y = x)) +   ## Plot it so that it's right side up
    geom_raster(aes(fill = value)) +
    labs(x = NULL, y = NULL) +
    scale_fill_gradient(low = "white", high = "black") +
    coord_fixed() +
    theme_bw() +
    scale_y_reverse() +
    theme(legend.position = "none")

## Subset the variables to save time
set.seed(2023-04-10)
var_use <- sample(4096, 64)
dat <- dat0 |>
    select(pmFRM, num_range(prefix = "pixel",
                            range = var_use))
dat

## Show which pixels were selected
expand_grid(x = 1:64, y = 1:64) |>
    mutate(z = NA) |>
    mutate(z = replace(z, var_use, 1)) |>
    ggplot(aes(y, x)) +
    geom_raster(aes(fill = z)) +
    scale_y_reverse() +
    coord_fixed()



## Immediately split the data into training and test sets
dat_split <- initial_split(dat)
dat_split

dat_train <- training(dat_split)

dat_train


dat_train |>
    ggplot(aes(pixel2976, pmFRM)) +
    geom_point() +
    geom_smooth()

## Show pmFRM with all pixels
dat_train |>
    pivot_longer(-pmFRM) |>
    ggplot(aes(value, pmFRM)) +
    geom_point(alpha = 1/10) +
    geom_smooth() +
    facet_wrap(vars(name))


## Start with linear regression
rec <- dat_train |>
    recipe(pmFRM ~ .) |>
    step_normalize(starts_with("pixel"))
rec

model <- linear_reg() |>
    set_engine("lm") |>
    set_mode("regression")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)
wf
res <- fit(wf, data = dat_train)

## Check performance on the complete training data
res |>
    extract_fit_engine() |>
    summary()

## Check performance using cross-validation
folds <- vfold_cv(dat_train, v = 10)
folds
res <- fit_resamples(wf, resamples = folds)
res |>
    collect_metrics()



## Try k-NN model
rec <- dat_train |>
    recipe(pmFRM ~ .) |>
    step_normalize(starts_with("pixel"))

model <- nearest_neighbor(neighbors = 10) |>
    set_engine("kknn") |>
    set_mode("regression")

wf <- workflow() |>
    add_model(model) |>
    add_recipe(rec)
wf

folds <- vfold_cv(dat_train, v = 10)

res <- fit_resamples(wf, resamples = folds)

res |>
    collect_metrics()

## Tune for the optimal number of neighbors
model <- nearest_neighbor(neighbors = tune("k")) |>
    set_engine("kknn") |>
    set_mode("regression")
wf <- workflow() |>
    add_model(model) |>
    add_recipe(rec)
wf

folds <- vfold_cv(dat_train, v = 10)

res <- tune_grid(wf, resamples = folds,
                 grid = tibble(k = c(10, 12, 13, 15, 17, 18, 20, 25)))

res |>
    collect_metrics()

res |>
    collect_metrics() |>
    filter(.metric == "rmse") |>
    ggplot(aes(k, mean)) +
    geom_point() +
    geom_line()

res |>
    show_best(metric = "rmse")

res |>
    show_best(metric = "rsq")

res |>
    collect_metrics() |>
    filter(.metric == "rsq") |>
    ggplot(aes(k, mean)) +
    geom_point() +
    geom_line()

## Try k-NN with PCA on predictors
rec <- dat_train |>
    recipe(pmFRM ~ .) |>
    step_normalize(starts_with("pixel")) |>
    step_pca(starts_with("pixel"), num_comp = 3)


model <- nearest_neighbor(neighbors = 17) |>
    set_engine("kknn") |>
    set_mode("regression")

wf <- workflow() |>
    add_model(model) |>
    add_recipe(rec)

folds <- vfold_cv(dat_train, v = 10)

res <- fit_resamples(wf, resamples = folds)
res |>
    collect_metrics()

## Tune PCA Components
rec <- dat_train |>
    recipe(pmFRM ~ .) |>
    step_normalize(starts_with("pixel")) |>
    step_pca(starts_with("pixel"), num_comp = tune("pca"))

model <- nearest_neighbor(neighbors = 15) |>
    set_engine("kknn") |>
    set_mode("regression")
wf <- workflow() |>
    add_model(model) |>
    add_recipe(rec)


res <- tune_grid(wf, resamples = folds,
                 grid = tibble(pca = c(1, 5, 10, 15, 20)))

res |>
    show_best(metric = "rmse")

res |>
    show_best(metric = "rsq")


## Try random forest instead
rec <- dat_train |>
    recipe(pmFRM ~ .) |>
    step_normalize(starts_with("pixel"))

model <- rand_forest(mtry = 5) |>
    set_engine("ranger") |>
    set_mode("regression")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)
wf

folds <- vfold_cv(dat_train, v = 10)

res <- fit_resamples(wf, resamples = folds)

res |>
    collect_metrics()

## Try a grid of tuning parameters
model <- rand_forest(mtry = tune("mtry"),
                     min_n = tune("min_n")) |>
    set_engine("ranger") |>
    set_mode("regression")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

## Fit model over grid of tuning parameters
res <- tune_grid(wf, resamples = folds,
                 grid = expand.grid(mtry = c(1, 2, 5),
                                    min_n = c(3, 5)))
res |>
    collect_metrics()

res |>
    show_best(metric = "rmse")

res |>
    show_best(metric = "rsq")

res |>
    select_best(metric = "rmse")

## Fit the best model obtained from tuning
model <- rand_forest(mtry = 2,
                     min_n = 3) |>
    set_engine("ranger") |>
    set_mode("regression")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

## Fit final model to entire training set; evaluate on test set
final <- wf |>
    last_fit(split = dat_split)

final |>
    collect_metrics()

## Plot the observed PM2.5 values vs. model predictions
final |>
    collect_predictions() |>
    ggplot(aes(.pred, pmFRM)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)


################################################################################

library(tidyverse)
library(tidymodels)
library(lubridate)

## https://www.kaggle.com/datasets/salvatorerastelli/spotify-and-youtube

dat0 <- read_csv("https://github.com/rdpeng/stat322E_public/raw/main/data/Spotify_Youtube.csv.gz")
dat0
dat0 |>
    glimpse()

dat <- dat0 |>
    select(-1) |>
    select(where(is.numeric)) |>
    filter(if_all(everything(), ~ !is.na(.x))) |> ## Remove rows w/NA values
    sample_frac(0.1)

dat |>
    glimpse()

## Histogram of streams
dat |>
    ggplot(aes(Stream)) +
    geom_histogram(bins = 10) +
    labs(x = "Streams")

## Summarize the distribution of the 'Streams' variable
summary(dat$Stream)

## Dichotomize Stream variable
dat <- dat |>
    mutate(success = ifelse(Stream > 50000000, 1, 0),
           success_f = ifelse(success == 1, "hit", "dud"))

dat |>
    glimpse()

dat_split <- initial_split(dat)
dat_split
dat_train <- training(dat_split)

## Look at Liveness
dat_train |>
    ggplot(aes(Liveness, success)) +
    geom_point(alpha = 1/20) +
    geom_smooth()

## Look at Danceability
dat_train |>
    ggplot(aes(Danceability, success)) +
    geom_point(alpha = 1/20) +
    geom_smooth()

dat |>
    glimpse()

## Plot a bunch of variables
dat_train |>
    select(success, Danceability:Duration_ms) |>
    pivot_longer(-success, names_to = "name", values_to = "metric") |>
    ggplot(aes(metric, success)) +
    geom_point(alpha = 1/2) +
    geom_smooth(se = FALSE) +
    facet_wrap(vars(name), scales = "free_x")

summary(dat_train$Duration_ms)

rec <- dat_train |>
    recipe(success_f ~ Danceability)

rec |>
    prep() |>
    bake(new_data = NULL)

model <- logistic_reg() |>
    set_mode("classification") |>
    set_engine("glm")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

res <- fit(wf, data = dat_train)

res |>
    extract_fit_parsnip() |>
    augment(new_data = dat_train)

tidy(res)

## Get regular glm() summary
res |>
    extract_fit_engine() |>
    summary()

## Create 10-folds for cross validation
folds <- vfold_cv(dat_train, v = 10)
folds

res <- fit_resamples(wf, resamples = folds)
res |>
    collect_metrics()

## Try adding more variables
rec <- dat_train |>
    recipe(success_f ~ Danceability + Acousticness + Energy + Loudness
           + Instrumentalness + Speechiness)

rec |>
    prep() |>
    bake(new_data = NULL)

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

folds <- vfold_cv(dat_train, v = 10)

res <- fit_resamples(wf, resamples = folds)
res |>
    collect_metrics()

## Try some other metrics of performance
res <- fit_resamples(wf, resamples = folds,
                     metrics = metric_set(accuracy, roc_auc,
                                          ppv, npv, sens, spec))
res |>
    collect_metrics()



## Try k-NN instead
rec <- dat_train |>
    recipe(success_f ~ Danceability + Acousticness + Energy + Loudness
           + Instrumentalness + Speechiness) |>
    step_normalize(-success_f)

model <- nearest_neighbor(neighbors = 2) |>
    set_engine("kknn") |>
    set_mode("classification")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

folds <- vfold_cv(dat_train, v = 10)

res <- fit_resamples(wf, resamples = folds,
                     metrics = metric_set(accuracy, roc_auc,
                                          ppv, npv, sens, spec))
res |>
    collect_metrics()

## Tune the 'neighbors' parameter
model <- nearest_neighbor(neighbors = tune("k")) |>
    set_engine("kknn") |>
    set_mode("classification")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

folds <- vfold_cv(dat_train, v = 10)

res <- tune_grid(wf, resamples = folds,
                 grid = tibble(k = seq(2, 30, 2)))
res |>
    collect_metrics()

## Plot performance metrics
res |>
    collect_metrics() |>
    filter(.metric == "roc_auc") |>
    ggplot(aes(k, mean)) +
    geom_point() +
    geom_line()

res |>
    show_best(metric = "roc_auc")

## Try random forest
rec <- dat_train |>
    recipe(success_f ~ Danceability + Acousticness + Energy + Loudness
           + Instrumentalness + Speechiness) |>
    step_normalize(-success_f)

model <- rand_forest() |>
    set_engine("ranger") |>
    set_mode("classification")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

folds <- vfold_cv(dat_train, v = 10)

res <- fit_resamples(wf, resamples = folds,
                     metrics = metric_set(accuracy, roc_auc,
                                          ppv, npv, sens, spec))
res |>
    collect_metrics()

## More variables
rec <- dat_train |>
    recipe(success_f ~ Danceability + Acousticness + Energy + Loudness
           + Instrumentalness + Speechiness + Liveness + Duration_ms
           + Valence + Tempo + Key) |>
    step_normalize(-success_f)

model <- rand_forest() |>
    set_engine("ranger") |>
    set_mode("classification")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

folds <- vfold_cv(dat_train, v = 10)

res <- fit_resamples(wf, resamples = folds,
                     metrics = metric_set(accuracy, roc_auc,
                                          ppv, npv, sens, spec))
res |>
    collect_metrics()

## Tuning for min_n
model <- rand_forest(min_n = tune("min_n")) |>
    set_engine("ranger") |>
    set_mode("classification")
wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)
wf
folds <- vfold_cv(dat_train, v = 5)
res <- tune_grid(wf, resamples = folds,
                 grid = tibble(min_n = c(2, 3, 5, 10)))
res |>
    collect_metrics()

res |>
    show_best(metric = "roc_auc")

## Remove a variable (Tempo)
rec <- dat_train |>
    recipe(success_f ~ Liveness + Loudness + Speechiness +
               Duration_ms + Valence + Instrumentalness + Acousticness +
               Energy) |>
    step_normalize(-success_f)
model <- rand_forest(min_n = 10) |>
    set_engine("ranger") |>
    set_mode("classification")
wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)
folds <- vfold_cv(dat_train, v = 5)
res <- fit_resamples(wf, resamples = folds)
res |>
    collect_metrics()



## Final model
rec <- dat_train |>
    recipe(success_f ~ Liveness + Tempo + Loudness + Speechiness +
               Duration_ms + Valence + Instrumentalness + Acousticness +
               Energy) |>
    step_normalize(-success_f)
model <- rand_forest(min_n = 10) |>
    set_engine("ranger") |>
    set_mode("classification")
wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)


## Plot the observed PM2.5 values vs. model predictions
res <- last_fit(wf, split = dat_split)
res |>
    collect_metrics()

dat_test <- rec |>
    prep() |>
    bake(new_data = testing(dat_split))

res |>
    extract_fit_parsnip() |>
    augment(new_data = dat_test) |>
    select(success_f, .pred_hit) |>
    ggplot(aes(.pred_hit, success_f)) +
    geom_point(alpha = 1/10)

## ROC plot
library(plotROC)

res |>
    extract_fit_parsnip() |>
    augment(new_data = dat_test) |>
    select(success_f, .pred_hit) |>
    ggplot(aes(d = success_f,
               m = .pred_hit)) +
    geom_roc()








