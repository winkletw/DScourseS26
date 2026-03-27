library(mice)
library(modelsummary) 
library(tidyverse)

# ==============================================================================
# load data 
# ==============================================================================
df_raw <- read_csv("wages.csv")

# check N
df_raw %>% 
  nrow()

# ==============================================================================
# clean data
# ==============================================================================

df_clean <- df_raw %>% 
  drop_na(hgc, tenure)

# new N
df_clean %>% 
  nrow()

# ==============================================================================
# sum stats
# ==============================================================================

# pct_missing function
pct_missing <- function(x) {
  (sum(is.na(x)) / length(x)) * 100
}

# sum stats in dataframe (hate the default tex output it is ugly) 
sumtable <- datasummary(
  logwage + hgc + tenure + age ~ N + pct_missing + Mean + SD + Min + Median + Max,
  data = df_clean,
  output = "dataframe"
)

# view table
sumtable 

# check pct_missing to make sure function is working correctly
df_clean %>%
  summarize(
    missing_count = sum(is.na(logwage)),
    total_obs = n(),
    pct_missing = (missing_count / total_obs) * 100
  )

# ==============================================================================
# complete case (cc) model
# ==============================================================================

# gen tenure2
df_clean <- df_clean %>%
  mutate(
    tenure2 = tenure^2
  )

# drop missing log wage cases
df_cc <- df_clean %>%
  drop_na(logwage)

# check cc N
df_cc %>%
  nrow()

# model of cc
mod_cc <- lm(
  logwage ~ hgc + college + tenure + tenure2 + age + married,
  data = df_cc
)

# results
summary(mod_cc)

# ==============================================================================
# mean logwage model
# ==============================================================================

# mean logwage
mean_logwage <- mean(df_clean$logwage, na.rm = TRUE)

# impute mean logwage
df_meanimp <- df_clean %>%
  mutate(
    logwage = ifelse(is.na(logwage), mean_logwage, logwage)
  )

# check if imputation worked
df_meanimp %>%
  summarize(
    missing_logwage = sum(is.na(logwage))
  )

# model with mean imputation
mod_meanimp <- lm(
  logwage ~ hgc + college + tenure + tenure2 + age + married,
  data = df_meanimp
)

# results
summary(mod_meanimp)

# ==============================================================================
# reg imputation model
# ==============================================================================

# create predicted log wages from the complete-case regression
df_regimp <- df_clean %>%
  mutate(
    pred_logwage = predict(mod_cc, newdata = df_clean)
  )

# replace missing logwage with predicted values
df_regimp <- df_regimp %>%
  mutate(
    logwage = if_else(is.na(logwage), pred_logwage, logwage)
  )

# verify no missing logwage remains
df_regimp %>%
  summarize(
    missing_logwage = sum(is.na(logwage))
  )

# check N
df_regimp %>%
  nrow()

# estimate model on regression-imputed data
mod_regimp <- lm(
  logwage ~ hgc + college + tenure + tenure2 + age + married,
  data = df_regimp
)

# results
summary(mod_regimp)


# ==============================================================================
# mice model
# ==============================================================================

# tell mice to impute logwage with pmm
meth <- make.method(df_clean)
meth[] <- ""
meth["logwage"] <- "pmm"

# run multiple imputation
imp <- mice(
  df_clean,
  m = 5,
  method = meth,
  maxit = 50,
  seed = 42, # answer to the ultimate question of life, the universe, and everything
  print = FALSE
)

# model using mice imputations
mod_mi <- with(
  imp,
  lm(logwage ~ hgc + college + tenure + tenure2 + age + married)
)

# pool model results
pool_mi <- pool(mod_mi)

# results
summary(pool_mi)

# ==============================================================================
# model summary table
# ==============================================================================

models <- list(
  "Complete Cases" = mod_cc,
  "Mean Imputation" = mod_meanimp,
  "Regression Imputation" = mod_regimp,
  "Multiple Imputation" = pool_mi
)

modelsummary(
  models,
  coef_rename = c(
    "hgc" = "hgc ($\\beta_1$)",
    "college" = "college",
    "tenure2" = "tenure_sq",
    "married" = "married"
  ),
  title = "Comparison of Imputation Methods for Missing Logwage Data",
  stars = TRUE,
  output = "latex"
)








