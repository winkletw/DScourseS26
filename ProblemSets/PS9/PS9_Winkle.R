# ==============================================================================
# run under R 4.3.2 on OSCER
# module load R/4.3.2-gfbf-2023a
# ==============================================================================

library(tidyverse)
library(tidymodels)
library(glmnet)

# ==============================================================================
# set seed
# ==============================================================================

set.seed(123456)

# ==============================================================================
# load data
# ==============================================================================

# load Boston housing data from UCI
housing <- read_table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",
  col_names = FALSE
)

# assign variable names
names(housing) <- c(
  "crim", "zn", "indus", "chas", "nox", "rm", "age",
  "dis", "rad", "tax", "ptratio", "b", "lstat", "medv"
)

# check dimensions
dim(housing)

# ============================================================================
# split sample into training and test sets
# ==============================================================================

# 80/20 train-test split
housing_split <- initial_split(housing, prop = 0.8)

# create training and test data
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# check dimensions
dim(housing_train)
dim(housing_test)

# ==============================================================================
# recipe and preprocessing
# ==============================================================================

# recipe:
# - log outcome
# - convert chas to factor
# - add interaction terms among continuous predictors
# - add 6th-degree polynomial terms for continuous predictors
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  step_log(all_outcomes()) %>%
  step_bin2factor(chas) %>%
  step_interact(
    terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox
  ) %>%
  step_poly(
    crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox,
    degree = 6
  )

# estimate preprocessing steps using the training ta only
housing_prep <- housing_recipe %>%
  prep(training = housing_train, retain = TRUE)

# apply recipe to training and test data
housing_train_prepped <- housing_prep %>% juice()
housing_test_prepped  <- housing_prep %>% bake(new_data = housing_test)

# create x and y training and test objects
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped %>% select(-medv)

housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y  <- housing_test_prepped %>% select(medv)

# check dimensions after preprocessing
dim(housing_train_prepped)
dim(housing_test_prepped)

# number of x variables after preprocessing
ncol(housing_train_x)

# number of additional x variables relative to the original data
ncol(housing_train_x) - (ncol(housing_train) - 1)

# ==============================================================================
# LASSO with 6-fold cross-validation
# ==============================================================================

# LASSO specification:
# penalty is tuned, mixture = 1 implies LASSO
lasso_spec <- linear_reg(
  penalty = tune(),
  mixture = 1
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# grid of lambda values to search over
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation on the training data
lasso_folds <- vfold_cv(housing_train_prepped, v = 6)

# workflow for LASSO
lasso_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(lasso_spec)

# tune lambda using cross-validation
lasso_res <- lasso_wf %>%
  tune_grid(
    resamples = lasso_folds,
    grid = lambda_grid,
    metrics = metric_set(rmse)
  )

# show best lambda values by RMSE
show_best(lasso_res, metric = "rmse")

# extract best lambda
best_lasso <- select_best(lasso_res, metric = "rmse")
best_lasso

# finalize workflow using the best lambda
final_lasso_wf <- finalize_workflow(lasso_wf, best_lasso)

# fit final LASSO model on the training data
final_lasso_fit <- final_lasso_wf %>%
  fit(data = housing_train_prepped)

# in-sample RMSE for LASSO
final_lasso_fit %>%
  predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth, .pred)

# out-of-sample RMSE for LASSO
final_lasso_fit %>%
  predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth, .pred)

# ==============================================================================
# ridge regression with 6-fold cross-validation
# ==============================================================================

# ridge specification:
# penalty is tuned, mixture = 0 implies ridge
ridge_spec <- linear_reg(
  penalty = tune(),
  mixture = 0
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# workflow for ridge
ridge_wf <- workflow() %>%
  add_formula(medv ~ .) %>%
  add_model(ridge_spec)

# tune lambda using cross-validation
ridge_res <- ridge_wf %>%
  tune_grid(
    resamples = lasso_folds,
    grid = lambda_grid,
    metrics = metric_set(rmse)
  )

# show best lambda values by RMSE
show_best(ridge_res, metric = "rmse")

# extract best lambda
best_ridge <- select_best(ridge_res, metric = "rmse")
best_ridge

# finalize workflow using the best lambda
final_ridge_wf <- finalize_workflow(ridge_wf, best_ridge)

# fit final ridge model on the training data
final_ridge_fit <- final_ridge_wf %>%
  fit(data = housing_train_prepped)

# in-sample RMSE for ridge
final_ridge_fit %>%
  predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth, .pred)

# out-of-sample RMSE for ridge
final_ridge_fit %>%
  predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth, .pred)

