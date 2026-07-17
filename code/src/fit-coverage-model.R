# Fit the paper's large-scale coverage model (multinomial lasso, MultinomialLogistic.qmd) and
# save it as a self-contained artifact. This is THE coverage model reported in the paper — the
# previously-saved clean_data/mutlinomial_fit.rds is a cv.glmnet from the older drone-based-model.qmd
# with a different (larger) feature set, NOT this model.
#
# Reproduces exactly: selected lambda = 0.002512, test RMSE = 0.184 (verified 2026-07-17).
# Run from the project root:  R -f code/src/fit-coverage-model.R
library(tidyverse)
library(tidymodels)
library(glmnet)

# --- data: 200 quarter-quadrats, keep the 20 NDVI + HSV summary predictors ---
quad <- readRDS("clean_data/quad-data-xgb_model_final.rds") %>%
  select(-starts_with("ortho_6")) %>%   # thermal
  select(-ends_with("_n")) %>%          # pixel counts
  select(-matches("_p\\d$")) %>%        # quartile-bin proportions
  select(-starts_with("ortho")) %>%     # raw ortho bands
  select(-ends_with("_min")) %>%
  select(-ends_with("_max"))

set.seed(8675309)
quad_split <- quad %>% initial_split(strata = grass)
quad_train <- training(quad_split)
quad_test  <- testing(quad_split)
quad_boot  <- quad_train %>% bootstraps(times = 30, strata = grass)

multi_rec <- recipe(dead + grass + sand ~ ., data = quad_train) %>%
  step_rm(quadrant_key) %>%
  step_normalize(all_predictors())

lambda_grid <- 10 ^ (seq(0, -20, by = -20/100))

# --- bootstrap tuning (per-split RMSE across the lambda grid) ---
tune_split <- function(split, rec, grid) {
  an <- analysis(split); as <- assessment(split)
  prep_an <- rec %>% prep(training = an) %>% bake(new_data = NULL)
  prep_as <- rec %>% prep(training = an) %>% bake(new_data = as)
  y <- prep_an %>% select(dead, grass, sand) %>% as.matrix()
  x <- prep_an %>% select(-c(dead, grass, sand)) %>% as.matrix()
  glm1 <- glmnet(x, y, family = "multinomial", alpha = 1, lambda = grid)
  x <- prep_as %>% select(-c(dead, grass, sand)) %>% as.matrix()
  nob <- nrow(x)
  predict(glm1, newx = x, type = "response") %>% as_tibble() %>%
    bind_cols(prep_as %>% select(dead, grass, sand)) %>%
    mutate(total = dead + grass + sand, dead = dead/total, grass = grass/total, sand = sand/total) %>%
    select(-total) %>% rename(dead.obs = dead, grass.obs = grass, sand.obs = sand) %>%
    pivot_longer(everything(), names_to = c("type", ".value"), names_sep = "\\.") %>%
    transmute(across(starts_with("s"), ~ (obs - .x)^2)) %>%
    summarize(across(everything(), ~ sqrt(sum(.x)/nob))) %>%
    pivot_longer(everything(), names_to = "grid_ind", values_to = "value") %>%
    mutate(grid_ind = grid_ind %>% str_remove("s") %>% as.numeric())
}

tune_res <- quad_boot %>%
  mutate(split_res = map(splits, tune_split, rec = multi_rec, grid = lambda_grid)) %>%
  select(id, split_res) %>% unnest(split_res)
tune_sum <- tune_res %>% group_by(grid_ind) %>%
  summarize(mean = mean(value), sd = sd(value)) %>%
  mutate(lambda = lambda_grid[grid_ind + 1]) %>% ungroup()

# one-SE-style rule: largest lambda whose mean RMSE is within sd/nboot of the minimum
nboot  <- nrow(quad_boot)
thresh <- tune_sum %>% slice(which.min(mean)) %>% mutate(t = mean + sd/nboot) %>% pull(t)
best   <- tune_sum %>% filter(mean < thresh) %>% arrange(lambda) %>% tail(1)
best_lambda <- best %>% pull(lambda)

# --- final fit on the full training set + prepped recipe for baking new data ---
train_prep  <- multi_rec %>% prep(training = quad_train)
y <- bake(train_prep, new_data = NULL) %>% select(dead, grass, sand) %>% as.matrix()
x <- bake(train_prep, new_data = NULL) %>% select(-c(dead, grass, sand)) %>% as.matrix()
glm_trained <- glmnet(x, y, family = "multinomial", alpha = 1, lambda = lambda_grid)

# --- test-set RMSE (coverage proportions) ---
test_prep <- bake(train_prep, new_data = quad_test)
xt  <- test_prep %>% select(-c(dead, grass, sand)) %>% as.matrix()
nob <- nrow(xt)
test_rmse <- predict(glm_trained, newx = xt, type = "response", s = best_lambda) %>%
  as_tibble() %>% rename(dead.pred = dead.1, grass.pred = grass.1, sand.pred = sand.1) %>%
  bind_cols(test_prep %>% select(dead, grass, sand)) %>%
  mutate(total = dead + grass + sand, dead.obs = dead/total, grass.obs = grass/total, sand.obs = sand/total) %>%
  select(ends_with(".obs"), ends_with(".pred")) %>%
  pivot_longer(everything(), names_to = c("type", ".value"), names_sep = "\\.") %>%
  mutate(dif2 = (obs - pred)^2) %>% summarise(rmse = sqrt(sum(dif2)/nob)) %>% pull(rmse)

cat(sprintf("selected lambda = %.6f  (paper 0.002512)\n", best_lambda))
cat(sprintf("test RMSE       = %.4f  (paper 0.184)\n", test_rmse))

# --- self-contained artifact ---
coverage_model_final <- list(
  glmnet      = glm_trained,          # multinomial lasso across the lambda grid
  recipe      = train_prep,           # prepped recipe: use bake(recipe, new_data) then predict(glmnet, s = best_lambda)
  best_lambda = best_lambda,
  test_rmse   = test_rmse,
  predictors  = setdiff(colnames(quad), c("quadrant_key", "dead", "grass", "sand")),
  seed        = 8675309,
  source      = "code/src/fit-coverage-model.R (MultinomialLogistic.qmd logic)",
  note        = "THE paper's coverage model. clean_data/mutlinomial_fit.rds is the older drone-based-model.qmd cv.glmnet, not this."
)
saveRDS(coverage_model_final, "outputs/coverage_model_final.rds")
cat("saved outputs/coverage_model_final.rds\n")
