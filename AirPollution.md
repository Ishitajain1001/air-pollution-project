---
title: "<div style='text-align: center;'>Project 2</div>"
author: "<div style='text-align: center;'>Ishita Jain</div>"
date: "<div style='text-align: center;'>2024-08-20</div>"
output: 
  html_document:
    css: style.css
    keep_md: true
  pdf_document: default
---



### Project 2

#### Author: Ishita Jain

Loading the data and packages needed.


``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(tidymodels)
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
## ✔ broom        1.0.6     ✔ rsample      1.2.1
## ✔ dials        1.3.0     ✔ tune         1.2.1
## ✔ infer        1.0.7     ✔ workflows    1.1.4
## ✔ modeldata    1.4.0     ✔ workflowsets 1.1.0
## ✔ parsnip      1.2.1     ✔ yardstick    1.3.1
## ✔ recipes      1.1.0     
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks stats::step()
## • Use tidymodels_prefer() to resolve common conflicts.
```

``` r
tidymodels_prefer()
dat <- read_csv("pm25_data.csv.gz")
```

```
## Rows: 876 Columns: 50
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (3): state, county, city
## dbl (47): id, value, fips, lat, lon, CMAQ, zcta, zcta_area, zcta_pop, imp_a5...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

### Title and Introduction

**For this data set, I looked at three different modeling approaches including Linear Regression, K-Nearest Neighbors, and Random Forest. In terms of the predictors chosen I looked at the different variables presented and found ones I thought might help predict the average PM2.5 concentration at the monitors. I then looked at which variables would appear to have a significant linear relationship by running different summaries on different recipes and used those for the rest of the models. I also ran a histogram on the nohs variable and saw that it was not normal so I applied a logarithmic transformation which helped significantly. I expect the RMSE of the model to be between 2 and 5.**

### Wrangling


``` r
dat <- dat |>
separate(col = id,
into = c(NA, "monitor"))
hist(dat$nohs)
```

![](AirPollution_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


``` r
dat <- dat |>
mutate(nohs = scale(nohs))
```

**One of the columns in the data, id, appeared to hold extraneous information so I separated the data and kept the monitor number for later application. The data set appeared to be in a tidy format so not much else was needed.**

### **Results**


``` r
# Split into training and testing data sets
dat_split <- initial_split(dat)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)
```

#### Linear Regression


``` r
rec <- dat_train |>
recipe(value~CMAQ + aod + log_dist_to_prisec + log_nei_2008_pm25_sum_10000 + nohs) |>
step_normalize(all_predictors()) |>
step_scale(all_predictors())
lm_model <- linear_reg() |>
set_engine("lm") |>
set_mode("regression")
lm_wf <- workflow() |>
add_recipe(rec) |>
add_model(lm_model)
folds <- vfold_cv(dat_train, v = 10)
lm_res <- fit_resamples(lm_wf, resamples = folds, metrics = metric_set(rmse))
lm_metrics <- lm_res %>%
collect_metrics() |>
mutate(model = "linear regression")
lin_rmse = 2.1243151
```

#### K-Nearest Neighbors


``` r
knn_model <- nearest_neighbor(neighbors = tune("k")) %>%
set_engine("kknn") %>%
set_mode("regression")
knn_wf <- workflow() %>%
add_model(knn_model) %>%
add_recipe(rec)
knn_res <- tune_grid(knn_wf, resamples = folds,
grid = tibble(k = c(3, 5, 10, 15, 20, 25)),
metrics = metric_set(rmse))
knn_res %>%
collect_metrics()
```

```
## # A tibble: 6 × 7
##       k .metric .estimator  mean     n std_err .config             
##   <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
## 1     3 rmse    standard    2.44    10   0.108 Preprocessor1_Model1
## 2     5 rmse    standard    2.29    10   0.110 Preprocessor1_Model2
## 3    10 rmse    standard    2.17    10   0.118 Preprocessor1_Model3
## 4    15 rmse    standard    2.13    10   0.117 Preprocessor1_Model4
## 5    20 rmse    standard    2.11    10   0.115 Preprocessor1_Model5
## 6    25 rmse    standard    2.10    10   0.113 Preprocessor1_Model6
```


``` r
knn_res %>%
collect_metrics() %>%
filter(.metric == "rmse") %>%
ggplot(aes(k, mean)) +
geom_point() +
geom_line()
```

![](AirPollution_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


``` r
#Thus k neighbors of 20 seem to have the best model since it has the lowest rmse
knn_model <- nearest_neighbor(neighbors = 20) %>%
set_engine("kknn") %>%
set_mode("regression")
knn_wf <- workflow() %>%
add_model(knn_model) %>%
add_recipe(rec)
knn_res <- fit_resamples(knn_wf, resamples = folds, metrics = metric_set(rmse))
k_metrics <- knn_res %>%
collect_metrics() |>
filter(.metric == "rmse") |>
mutate(model = "k_NN")
k_20_rmse = 2.008735
```

#### Random Forest Model


``` r
rf_model <- rand_forest(mtry = tune("mtry"),
min_n = tune("min_n")) %>%
set_engine("ranger") %>%
set_mode("regression")
rf_wf <- workflow() %>%
add_recipe(rec) %>%
add_model(rf_model)
## Fit model over grid of tuning parameters
rf_res <- tune_grid(rf_wf, resamples = folds,
grid = expand.grid(mtry = c(1, 2, 5),
min_n = c(3, 5)))
rf_res %>%
collect_metrics()
```

```
## # A tibble: 12 × 8
##     mtry min_n .metric .estimator  mean     n std_err .config             
##    <dbl> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
##  1     1     3 rmse    standard   1.98     10  0.112  Preprocessor1_Model1
##  2     1     3 rsq     standard   0.392    10  0.0463 Preprocessor1_Model1
##  3     2     3 rmse    standard   1.98     10  0.123  Preprocessor1_Model2
##  4     2     3 rsq     standard   0.395    10  0.0514 Preprocessor1_Model2
##  5     5     3 rmse    standard   2.01     10  0.121  Preprocessor1_Model3
##  6     5     3 rsq     standard   0.377    10  0.0506 Preprocessor1_Model3
##  7     1     5 rmse    standard   1.99     10  0.114  Preprocessor1_Model4
##  8     1     5 rsq     standard   0.388    10  0.0484 Preprocessor1_Model4
##  9     2     5 rmse    standard   1.98     10  0.120  Preprocessor1_Model5
## 10     2     5 rsq     standard   0.392    10  0.0499 Preprocessor1_Model5
## 11     5     5 rmse    standard   2.02     10  0.118  Preprocessor1_Model6
## 12     5     5 rsq     standard   0.370    10  0.0498 Preprocessor1_Model6
```


``` r
rf_res %>%
show_best(metric = "rmse")
```

```
## # A tibble: 5 × 8
##    mtry min_n .metric .estimator  mean     n std_err .config             
##   <dbl> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
## 1     2     3 rmse    standard    1.98    10   0.123 Preprocessor1_Model2
## 2     2     5 rmse    standard    1.98    10   0.120 Preprocessor1_Model5
## 3     1     3 rmse    standard    1.98    10   0.112 Preprocessor1_Model1
## 4     1     5 rmse    standard    1.99    10   0.114 Preprocessor1_Model4
## 5     5     3 rmse    standard    2.01    10   0.121 Preprocessor1_Model3
```


``` r
rf_metrics <- rf_res |>
collect_metrics() |>
filter(.metric == "rmse") |>
filter(mtry == 2 & min_n == 3) |>
mutate(model = "random forest") |>
arrange(mean)
randFor_rmse = 1.854144
```

#### Summarizing Results


``` r
combined <- bind_rows(lm_metrics, k_metrics, rf_metrics)
combined |>
group_by(model) |>
summarize(mean) |>
rename("RMSE" = mean) |>
arrange(RMSE)
```

```
## # A tibble: 3 × 2
##   model              RMSE
##   <chr>             <dbl>
## 1 random forest      1.98
## 2 k_NN               2.11
## 3 linear regression  2.14
```


``` r
# Gather RMSE values into a data frame
rmse_df <- bind_rows(lm_res, knn_res, rf_res)
# Create a plot of the RMSE values for each model
ggplot(combined, aes(x = model, y = mean)) +
geom_boxplot() +
scale_y_continuous(limits = c(0, 5), expand = expansion(mult = c(0, 0.05))) +
labs(x = "Model", y = "RMSE", title = "Comparison of Model Performance") +
theme_bw()
```

![](AirPollution_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


``` r
rf_model <- rand_forest(mtry = 5, min_n = 5) %>%
set_engine("ranger") %>%
set_mode("regression")
rf_wf <- workflow() %>%
add_recipe(rec) %>%
add_model(rf_model)
rf_fit <- rf_wf %>%
fit(dat_train)
rf_pred <- predict(rf_fit, dat_test) %>%
bind_cols(dat_test)
dat_test$differences <- rf_pred$.pred - dat_test$value
rf_rmse <- sqrt(mean(dat_test$differences^2, na.rm = TRUE))
rf_rmse
```

```
## [1] 1.896279
```

**Primarily I split the data into training and testing data sets with a 75-25 split. I created 2 different models all using the same recipe to test which approach works the best. They included linear regression, k-nearest neighbors and random forest. The model with the lowest RMSE appears to be the Random Forest model showing it is the best approach.**

### Discussion


``` r
dat_test |>
group_by(state) |>
summarize(avg = mean(differences), n = n()) |>
arrange(abs(avg))
```

```
## # A tibble: 44 × 3
##    state           avg     n
##    <chr>         <dbl> <int>
##  1 Alabama     -0.0187     3
##  2 Iowa         0.0271     6
##  3 Kentucky    -0.0924     6
##  4 Oregon      -0.112      6
##  5 Montana     -0.127      3
##  6 Missouri    -0.164      5
##  7 Maryland     0.241      3
##  8 Wisconsin    0.246      2
##  9 Mississippi  0.250      3
## 10 Ohio        -0.345     15
## # ℹ 34 more rows
```

1.  For the most part, the locations that are closest from observed values appear to be near a water source and opposite for the far ones. This might be due to the clarity of the air when it is near the coast.

2.  I do not think there are exact regions for the model that I have
    where they perform better or worse since they appear mainly scattered except for the coasts. I think information about the locations car pollution might be a strong predictor that could have helped.

3.  The model seems to be weaker when CMAQ and AOD are not included in
    the model with higher residuals.

4.  I do not think the model will perform quite well for them since the weather conditions are drastically different there compared to other US locations. It was very challenging for me to come up with a vizualization for the rmse but I have learned to keep trying and exprimenting to reach a conclusion. It performed as I expected but on the lower spectrum of what I thought for my models. I did this project on my own.
