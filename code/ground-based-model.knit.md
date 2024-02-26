---
title: "ground-based-model"
output: html_document
date: "2023-12-24"
---




## Load all packages and remove all objects from memory


::: {.cell}

```{.r .cell-code}
rm(list = ls())
gc()
```

::: {.cell-output .cell-output-stdout}

```
          used (Mb) gc trigger (Mb) max used (Mb)
Ncells  800070 42.8    1290524   69  1290524 69.0
Vcells 1577773 12.1    8388608   64  2712444 20.7
```


:::
:::

::: {.cell warn='false'}

```{.r .cell-code}
library(rpart.plot)
library(tidyverse)
library(tidymodels)
library(vip)
library(glue)
library(tictoc)
library(xgboost)
source('code/src/spatial-utils.R')
```
:::


## Set the seed for reproducibility


::: {.cell}

```{.r .cell-code}
set.seed(8675309)
```
:::


## Extract all pixels

If the data is not generated run 'src/sample-all-pixels.R' to extract pixel data from all 50 photographs using the labeled polygons

You can use the function `extract_polygon_pixels` to extract the pixels from polygons drawn in `label-me`. The function takes four arguments:

1.  the path to the image
2.  a polygon sf object or the path to the polygons json file
3.  a boolean to include the polygon data (IDs of polygons etc)
4.  the extent of the image

The function returns a list of data frames with the pixels from the polygons.


::: {.cell}

```{.r .cell-code}
extract_polygon_pixels
```

::: {.cell-output .cell-output-stdout}

```
function (image_path, polygon, include_polygon_info = TRUE, extent = c(0, 
    4032, 0, 3024)) 
{
    image <- terra::rast(image_path)
    quadrat_number <- gsub("\\D", "", image_path)
    if (is.character(polygon)) {
        polygon <- label_me_json_to_sf(polygon_path)
    }
    poly_info <- mutate(mutate(st_drop_geometry(polygon), ID = row_number()), 
        key = paste0(quadrat_number, "_", ID))
    pixels <- terra::extract(image, polygon, cells = TRUE, extent = extent)
    if (include_polygon_info) {
        pixels <- as_tibble((function(x) x[, !colnames(x) %in% 
            c("ID", "imagePath"), drop = FALSE])(left_join(pixels, 
            poly_info, by = "ID")))
    }
    else {
        pixels <- (function(x) x[, !colnames(x) %in% c("ID", 
            "cell"), drop = FALSE])(pixels)
    }
    return(pixels)
}
```


:::
:::

::: {.cell}

```{.r .cell-code}
pixels_path <- 'clean_data/labeled_pixels.rds'

clean_pixels <- readRDS(pixels_path) |>
  group_by(key) |>
  slice_sample(n = 1000) |>
  ungroup()

clean_pixels
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 544,852 × 164
   label key      cell   area hsl_1 hsl_2 hsl_3 lab_1 lab_2 lab_3
   <chr> <chr>   <dbl>  <dbl> <int> <int> <int> <int> <int> <int>
 1 sand  34_1  9250893 29235.    25    45   113   183    71    77
 2 sand  34_1  9448515 29235.    25    56    95   169    72    86
 3 sand  34_1  9460486 29235.    25    48   105   177    71    80
 4 sand  34_1  9456528 29235.    24    53    97   170    74    82
 5 sand  34_1  9279079 29235.    25    50   149   205    70    69
 6 sand  34_1  9589581 29235.    25    51   147   204    71    71
 7 sand  34_1  9605749 29235.    23    66   167   215    74    68
 8 sand  34_1  9831567 29235.    24    48   120   187    74    80
 9 sand  34_1  9577434 29235.    25    45   112   182    71    77
10 sand  34_1  9089581 29235.    28    42   105   178    67    76
# ℹ 544,842 more rows
# ℹ 154 more variables: lab_contrast_L3_W11 <dbl>, lab_contrast_L3_W5 <dbl>,
#   rgb_1 <int>, rgb_2 <int>, rgb_3 <int>, rgb_seg_SR3_RR4.5_MD30_1 <int>,
#   rgb_seg_SR3_RR4.5_MD30_2 <int>, rgb_seg_SR3_RR4.5_MD30_3 <int>,
#   rgb_seg_SR3_RR4.5_MD30_hsl_1 <int>, rgb_seg_SR3_RR4.5_MD30_hsl_2 <int>,
#   rgb_seg_SR3_RR4.5_MD30_hsl_3 <int>, rgb_seg_SR3_RR4.5_MD30_lab_1 <int>,
#   rgb_seg_SR3_RR4.5_MD30_lab_2 <int>, rgb_seg_SR3_RR4.5_MD30_lab_3 <int>, …
```


:::
:::


## Create training and testing set


::: {.cell}

```{.r .cell-code}
pixel_split <- clean_pixels |>
  dplyr::select(-c(cell))|>
  mutate(label = as.factor(label)) |>
  group_initial_split(prop = 0.75, group = key)

pixels_train <- pixel_split |>
  training()

pixels_test <- pixel_split |>
  testing()
```
:::

::: {.cell}

```{.r .cell-code}
pixels_train |>
  head()
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 163
  label key     area hsl_1 hsl_2 hsl_3 lab_1 lab_2 lab_3 lab_contrast_L3_W11
  <fct> <chr>  <dbl> <int> <int> <int> <int> <int> <int>               <dbl>
1 sand  34_1  29235.    25    45   113   183    71    77               0.448
2 sand  34_1  29235.    25    56    95   169    72    86               0.806
3 sand  34_1  29235.    25    48   105   177    71    80               0.409
4 sand  34_1  29235.    24    53    97   170    74    82               0.552
5 sand  34_1  29235.    25    50   149   205    70    69               0.791
6 sand  34_1  29235.    25    51   147   204    71    71               0.638
# ℹ 153 more variables: lab_contrast_L3_W5 <dbl>, rgb_1 <int>, rgb_2 <int>,
#   rgb_3 <int>, rgb_seg_SR3_RR4.5_MD30_1 <int>,
#   rgb_seg_SR3_RR4.5_MD30_2 <int>, rgb_seg_SR3_RR4.5_MD30_3 <int>,
#   rgb_seg_SR3_RR4.5_MD30_hsl_1 <int>, rgb_seg_SR3_RR4.5_MD30_hsl_2 <int>,
#   rgb_seg_SR3_RR4.5_MD30_hsl_3 <int>, rgb_seg_SR3_RR4.5_MD30_lab_1 <int>,
#   rgb_seg_SR3_RR4.5_MD30_lab_2 <int>, rgb_seg_SR3_RR4.5_MD30_lab_3 <int>,
#   rgb_seg_SR3_RR4.5_MD30_yuv_1 <int>, rgb_seg_SR3_RR4.5_MD30_yuv_2 <int>, …
```


:::

```{.r .cell-code}
pixels_test |> 
  head()
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 163
  label key    area hsl_1 hsl_2 hsl_3 lab_1 lab_2 lab_3 lab_contrast_L3_W11
  <fct> <chr> <dbl> <int> <int> <int> <int> <int> <int>               <dbl>
1 sand  34_10 8355.    26    51   108   180    70    84               0.574
2 sand  34_10 8355.    27    54   108   180    69    88               0.669
3 sand  34_10 8355.    23    50   112   181    75    79               1.05 
4 sand  34_10 8355.    23    46   128   192    75    75               1.16 
5 sand  34_10 8355.    23    46   121   187    75    77               0.736
6 sand  34_10 8355.    25    52    93   167    73    80               0.581
# ℹ 153 more variables: lab_contrast_L3_W5 <dbl>, rgb_1 <int>, rgb_2 <int>,
#   rgb_3 <int>, rgb_seg_SR3_RR4.5_MD30_1 <int>,
#   rgb_seg_SR3_RR4.5_MD30_2 <int>, rgb_seg_SR3_RR4.5_MD30_3 <int>,
#   rgb_seg_SR3_RR4.5_MD30_hsl_1 <int>, rgb_seg_SR3_RR4.5_MD30_hsl_2 <int>,
#   rgb_seg_SR3_RR4.5_MD30_hsl_3 <int>, rgb_seg_SR3_RR4.5_MD30_lab_1 <int>,
#   rgb_seg_SR3_RR4.5_MD30_lab_2 <int>, rgb_seg_SR3_RR4.5_MD30_lab_3 <int>,
#   rgb_seg_SR3_RR4.5_MD30_yuv_1 <int>, rgb_seg_SR3_RR4.5_MD30_yuv_2 <int>, …
```


:::
:::


## XGBoost model

First we build the formula with all the predictors


::: {.cell}

```{.r .cell-code}
all_columns <- names(clean_pixels)# |>
  #sample(size =  110)

non_predictors <- c("cell", "train_val", "label", "directory", "poly_num", "key", "label", "area")
predictors <- all_columns[!all_columns %in% non_predictors]

formula <- as.formula(paste("label ~", paste(predictors, collapse = " + ")))
#formula <- as.formula(paste("label ~ hsv_1"))
```
:::


We have hundreds of predictors so we fit a single decision tree to get the top 20 layers.


::: {.cell}

```{.r .cell-code}
tree_spec <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification") |>
  set_args(cost_complexity = tune())

data <- pixels_train |>
  dplyr::select(all_of(c(predictors, 'label') ))

# We use update_role because formula eats up ram usage  if there are many predictors(will throw an error)
# https://github.com/tidymodels/recipes/issues/548

tree_rec <- recipe(data) |> 
  update_role(everything()) |>
  update_role(label, new_role = "outcome") |> 
  step_naomit(all_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_predictors())

tree_wf <- workflow() |>
  add_model(tree_spec) |>
  add_recipe(tree_rec)

tree_grid <- grid_regular(cost_complexity(range = c(-10, -1)), levels = 20)
```
:::


Tune the decision tree


::: {.cell}

```{.r .cell-code}
pixel_folds <- pixels_train |>
  group_vfold_cv(v = 10, group = key, balance = "observations")

tic()
doParallel::registerDoParallel(15)
tune_res <- tune_grid(
  tree_wf,
  resamples = pixel_folds,
  grid = tree_grid,
  control = control_grid(verbose = TRUE)
)
toc()
```

::: {.cell-output .cell-output-stdout}

```
6431.436 sec elapsed
```


:::
:::


Visualize tuning results


::: {.cell}

```{.r .cell-code}
tune_res |>
  autoplot(metric = "accuracy") +
  theme_minimal()
```

::: {.cell-output-display}
![](ground-based-model_files/figure-html/unnamed-chunk-8-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
tree_best <- tune_res |>
  select_by_one_std_err(desc(cost_complexity), metric = "accuracy")

tree_final <- tree_wf |>
  finalize_workflow(tree_best)

tree_fit <- tree_final |>
  fit(data = pixels_train)

# with usual 0.5 threshold
tree_fit |>
  augment(new_data = pixels_test) |>
  conf_mat(truth = label, estimate = .pred_class)
```

::: {.cell-output .cell-output-stdout}

```
                 Truth
Prediction        dead vegetation live vegetation  sand
  dead vegetation           25264             718  6823
  live vegetation             527           59681   287
  sand                       2932            1183 38650
```


:::

```{.r .cell-code}
importance <- tree_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 15)
importance
```

::: {.cell-output-display}
![](ground-based-model_files/figure-html/unnamed-chunk-9-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
tree_fit |>
  extract_fit_engine() |>
  rpart.plot(box.palette = list("gray80", "darkolivegreen3", "gold2"), tweak = 0.5)
```

::: {.cell-output .cell-output-stderr}

```
Warning: Cannot retrieve the data used to build the model (model.frame: object '..y' not found).
To silence this warning:
    Call rpart.plot with roundint=FALSE,
    or rebuild the rpart model with model=TRUE.
```


:::

::: {.cell-output-display}
![](ground-based-model_files/figure-html/unnamed-chunk-10-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
top_features <- importance$data$Variable
formula_top_features <- as.formula(paste("label ~", paste(top_features, collapse = " + ")))
formula_top_features
```

::: {.cell-output .cell-output-stdout}

```
label ~ hsl_1 + rgb_seg_SR6_RR4.5_MD30_hsl_1 + rgb_seg_SR6_RR4.5_MD70_hsl_1 + 
    rgb_seg_SR3_RR4.5_MD50_hsl_1 + rgb_seg_SR6_RR4.5_MD50_hsl_1 + 
    rgb_seg_SR3_RR4.5_MD30_hsl_1 + rgb_seg_SR6_RR7.5_MD70_3 + 
    rgb_seg_SR6_RR7.5_MD50_3 + rgb_seg_SR6_RR7.5_MD30_3 + rgb_seg_SR6_RR7.5_MD50_hsl_3 + 
    rgb_seg_SR6_RR7.5_MD30_hsl_3 + rgb_seg_SR6_RR7.5_MD50_2 + 
    rgb_seg_SR3_RR4.5_MD50_hsl_2 + rgb_seg_SR3_RR4.5_MD70_hsl_2 + 
    rgb_seg_SR3_RR4.5_MD30_hsl_2
```


:::
:::

::: {.cell}

```{.r .cell-code}
xgb_specification <- boost_tree(
  trees = 1000,
  min_n = tune(),
  mtry = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) |>
  set_engine("xgboost", nthread = 5) |>
  set_mode("classification")

data <- pixels_train[c(predictors, "label")]

# We use update_role because formula eats up ram usage if there are too many predictors(will throw an error)
# https://github.com/tidymodels/recipes/issues/548

xgb_recipe  <- recipe(data) |> 
  update_role(everything()) |>
  update_role(label, new_role = "outcome") |> 
  step_naomit(all_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_predictors())
  
xgb_workflow <- workflow() |>
  add_model(xgb_specification) |>
  add_recipe(xgb_recipe)

prepped_recipe <- prep(xgb_recipe)
baked_recipe <- bake(prepped_recipe, new_data = NULL)

xgb_grid <- grid_latin_hypercube(
  finalize(mtry(), baked_recipe),
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 12
)
```
:::


## Create folds for K fold cross validation


::: {.cell}

```{.r .cell-code}
pixel_folds <- pixels_train |>
  group_vfold_cv(v = 10, group = key, balance = "observations")
```
:::


Try nthread = 3 or 4 as an option in set_engine. Then try registerDoParallel(8) or 6. If runs out of memeory use more threads and fewer forks.

Can also try racing methods to really speed things up.


::: {.cell}

```{.r .cell-code}
tic()
doParallel::registerDoParallel(15)
tune_results <- tune_grid(
  xgb_workflow,
  resamples = pixel_folds,
  grid = xgb_grid,
  control = control_grid(verbose = TRUE)
  )
```

::: {.cell-output .cell-output-stderr}

```
Warning in mclapply(argsList, FUN, mc.preschedule = preschedule, mc.set.seed =
set.seed, : scheduled cores 5, 6 did not deliver results, all values of the
jobs will be affected
```


:::

::: {.cell-output .cell-output-stderr}

```
Warning: More than one set of outcomes were used when tuning. This should never
happen. Review how the outcome is specified in your model.
```


:::

```{.r .cell-code}
toc()
```

::: {.cell-output .cell-output-stdout}

```
46224.337 sec elapsed
```


:::
:::


Tuning results


::: {.cell}

```{.r .cell-code}
tune_results |>
  collect_metrics() |>
  filter(.metric == "accuracy") |>
  pivot_longer(mtry:sample_size, names_to = "parameter", values_to = "value") |>
  rename(accuracy = mean) |>
  ggplot(aes(value, accuracy, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ parameter, scales = "free_x") +
  theme_minimal()
```

::: {.cell-output-display}
![](ground-based-model_files/figure-html/Tuning results-1.png){width=672}
:::
:::


Train the model with best hyper-parameters based on accuracy and then calculate testing error.


::: {.cell}

```{.r .cell-code}
xgb_best <- tune_results |>
  select_best(metric = "accuracy")

xgb_final <- xgb_workflow |>
  finalize_workflow(xgb_best)

xgb_fit <- xgb_final |>
  fit(data = pixels_train)
```
:::

::: {.cell}

```{.r .cell-code}
today <- Sys.Date()
saveRDS(xgb_fit, glue('clean_data/xgb_fit_{today}.rds'))
```
:::


Confusion matrix on the test set


::: {.cell}

```{.r .cell-code}
# 0.5 threshold
confusion_matrix <- xgb_fit |>
  augment(new_data = pixels_test) |>
  conf_mat(truth = label, estimate = .pred_class)

confusion_matrix
```

::: {.cell-output .cell-output-stdout}

```
                 Truth
Prediction        dead vegetation live vegetation  sand
  dead vegetation           26005             597  1945
  live vegetation             148           60354    27
  sand                       2570             631 43788
```


:::
:::


Accuracy on the test set


::: {.cell}

```{.r .cell-code}
accuracy <- sum(diag(confusion_matrix$table)) / sum(confusion_matrix$table)
accuracy
```

::: {.cell-output .cell-output-stdout}

```
[1] 0.9565061
```


:::
:::


Find the top features


::: {.cell}

```{.r .cell-code}
xgb_fit |>
  extract_fit_engine() |>
  vip(num_features = 25) +
  theme_minimal()
```

::: {.cell-output-display}
![](ground-based-model_files/figure-html/unnamed-chunk-16-1.png){width=672}
:::
:::

