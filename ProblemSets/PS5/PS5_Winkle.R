library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)

# ==============================================================================
# scrape data from tx space commission
# ==============================================================================

txsc <- "https://space.texas.gov/grants/awards"
txsc_webpage <- read_html(txsc)

space_grants_raw <- txsc_webpage %>%
  html_element("table") %>%
  html_table() %>%
  clean_names()

space_grants_clean <- space_grants_raw %>%
  rename(amountawarded = amount_up_to) %>%
  mutate(
    award_date = mdy(award_date),
    year = year(award_date),
    amountawarded = str_extract(amountawarded, "\\$[0-9,]+"),
    amountawarded = str_remove_all(amountawarded, "[$,]"),
    amountawarded = as.numeric(amountawarded)
  ) %>%
  separate(
    location,
    into = c("city", "state"),
    sep = ",\\s*",
    remove = TRUE
  )

space_grants

# ==============================================================================
# NASA black marble API - DO NOT RUN; TAKES FOREVER
# ==============================================================================
# 
# library(blackmarbler)
# library(geodata)
# library(sf)
# library(terra)
# library(ggplot2)
# library(tidyterra)
# library(lubridate)
# library(tidyverse)
# library(purrr)
# 
# # ==============================================================================
# # set log file
# # ==============================================================================
# 
# log_file <- paste0(
#   "blackmarble_pull_2021_",
#   format(Sys.time(), "%Y%m%d_%H%M%S"),
#   ".log"
# )
# 
# log_con <- file(log_file, open = "wt")
# sink(log_con, split = TRUE)
# sink(log_con, type = "message")
# 
# cat("Black Marble 2021 pull started:", as.character(Sys.time()), "\n")
# 
# # ==============================================================================
# # authentication
# # ==============================================================================
# 
# bearer <- Sys.getenv("NASA_BEARER")
# 
# # ==============================================================================
# # get South Africa admin-2 boundaries
# # ==============================================================================
# 
# sa_adm2 <- gadm(
#   country = "ZAF",
#   level = 2,
#   path = tempdir()
# )
# 
# sa_adm2 <- sa_adm2 %>% 
#   st_as_sf()
# 
# # ==============================================================================
# # set dates for daily extraction
# # ==============================================================================
# 
# # jan 2021
# dates_daily_01 <- seq.Date(
#   from = as.Date("2021-01-01"),
#   to   = as.Date("2021-01-31"),
#   by   = "day"
# )
# 
# # feb 2021
# dates_daily_02 <- seq.Date(
#   from = as.Date("2021-02-01"),
#   to   = as.Date("2021-02-28"),
#   by   = "day"
# )
# 
# # mar 2021
# dates_daily_03 <- seq.Date(
#   from = as.Date("2021-03-01"),
#   to   = as.Date("2021-03-31"),
#   by   = "day"
# )
# 
# # apr 2021
# dates_daily_04 <- seq.Date(
#   from = as.Date("2021-04-01"),
#   to   = as.Date("2021-04-30"),
#   by   = "day"
# )
# 
# # may 2021
# dates_daily_05 <- seq.Date(
#   from = as.Date("2021-05-01"),
#   to   = as.Date("2021-05-31"),
#   by   = "day"
# )
# 
# # jun 2021
# dates_daily_06 <- seq.Date(
#   from = as.Date("2021-06-01"),
#   to   = as.Date("2021-06-30"),
#   by   = "day"
# )
# 
# # jul 2021
# dates_daily_07 <- seq.Date(
#   from = as.Date("2021-07-01"),
#   to   = as.Date("2021-07-31"),
#   by   = "day"
# )
# 
# # aug 2021
# dates_daily_08 <- seq.Date(
#   from = as.Date("2021-08-01"),
#   to   = as.Date("2021-08-31"),
#   by   = "day"
# )
# 
# # sep 2021
# dates_daily_09 <- seq.Date(
#   from = as.Date("2021-09-01"),
#   to   = as.Date("2021-09-30"),
#   by   = "day"
# )
# 
# # oct 2021
# dates_daily_10 <- seq.Date(
#   from = as.Date("2021-10-01"),
#   to   = as.Date("2021-10-31"),
#   by   = "day"
# )
# 
# # nov 2021
# dates_daily_11 <- seq.Date(
#   from = as.Date("2021-11-01"),
#   to   = as.Date("2021-11-30"),
#   by   = "day"
# )
# 
# # dec 2021
# dates_daily_12 <- seq.Date(
#   from = as.Date("2021-12-01"),
#   to   = as.Date("2021-12-31"),
#   by   = "day"
# )
# 
# # ==============================================================================
# # extract daily data for South Africa admin-2 boundaries
# # ==============================================================================
# 
# # jan 2021
# ntl_sa_01_21_list <- vector("list", length(dates_daily_01))
# 
# for (i in seq_along(dates_daily_01)) {
#   
#   this_date <- dates_daily_01[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_01_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_01_21 <- ntl_sa_01_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_01_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_01_21.rds"
# )
# 
# # feb 2021
# ntl_sa_02_21_list <- vector("list", length(dates_daily_02))
# 
# for (i in seq_along(dates_daily_02)) {
#   
#   this_date <- dates_daily_02[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_02_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_02_21 <- ntl_sa_02_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_02_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_02_21.rds"
# )
# 
# # mar 2021
# ntl_sa_03_21_list <- vector("list", length(dates_daily_03))
# 
# for (i in seq_along(dates_daily_03)) {
#   
#   this_date <- dates_daily_03[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_03_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_03_21 <- ntl_sa_03_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_03_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_03_21.rds"
# )
# 
# # apr 2021
# ntl_sa_04_21_list <- vector("list", length(dates_daily_04))
# 
# for (i in seq_along(dates_daily_04)) {
#   
#   this_date <- dates_daily_04[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_04_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_04_21 <- ntl_sa_04_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_04_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_04_21.rds"
# )
# 
# # may 2021
# ntl_sa_05_21_list <- vector("list", length(dates_daily_05))
# 
# for (i in seq_along(dates_daily_05)) {
#   
#   this_date <- dates_daily_05[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_05_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_05_21 <- ntl_sa_05_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_05_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_05_21.rds"
# )
# 
# # jun 2021
# ntl_sa_06_21_list <- vector("list", length(dates_daily_06))
# 
# for (i in seq_along(dates_daily_06)) {
#   
#   this_date <- dates_daily_06[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_06_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_06_21 <- ntl_sa_06_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_06_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_06_21.rds"
# )
# 
# # jul 2021
# ntl_sa_07_21_list <- vector("list", length(dates_daily_07))
# 
# for (i in seq_along(dates_daily_07)) {
#   
#   this_date <- dates_daily_07[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_07_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_07_21 <- ntl_sa_07_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_07_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_07_21.rds"
# )
# 
# # aug 2021
# ntl_sa_08_21_list <- vector("list", length(dates_daily_08))
# 
# for (i in seq_along(dates_daily_08)) {
#   
#   this_date <- dates_daily_08[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_08_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_08_21 <- ntl_sa_08_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_08_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_08_21.rds"
# )
# 
# # sep 2021
# ntl_sa_09_21_list <- vector("list", length(dates_daily_09))
# 
# for (i in seq_along(dates_daily_09)) {
#   
#   this_date <- dates_daily_09[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_09_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_09_21 <- ntl_sa_09_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_09_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_09_21.rds"
# )
# 
# # oct 2021
# ntl_sa_10_21_list <- vector("list", length(dates_daily_10))
# 
# for (i in seq_along(dates_daily_10)) {
#   
#   this_date <- dates_daily_10[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_10_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_10_21 <- ntl_sa_10_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_10_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_10_21.rds"
# )
# 
# # nov 2021
# ntl_sa_11_21_list <- vector("list", length(dates_daily_11))
# 
# for (i in seq_along(dates_daily_11)) {
#   
#   this_date <- dates_daily_11[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_11_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_11_21 <- ntl_sa_11_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_11_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_11_21.rds"
# )
# 
# # dec 2021
# ntl_sa_12_21_list <- vector("list", length(dates_daily_12))
# 
# for (i in seq_along(dates_daily_12)) {
#   
#   this_date <- dates_daily_12[i]
#   
#   message("Pulling: ", this_date)
#   
#   ntl_sa_12_21_list[[i]] <- tryCatch(
#     {
#       bm_extract(
#         roi_sf = sa_adm2,
#         product_id = "VNP46A2",
#         date = this_date,
#         bearer = bearer
#       )
#     },
#     error = function(e) {
#       message("Failed on ", this_date, ": ", e$message)
#       NULL
#     }
#   )
# }
# 
# ntl_sa_12_21 <- ntl_sa_12_21_list %>% 
#   compact() %>% 
#   bind_rows()
# 
# saveRDS(
#   ntl_sa_12_21,
#   file = "data/raw/blackmarble/ZAF/2021/ntl_sa_12_21.rds"
# )
# 
# # ==============================================================================
# # close up shop
# # ==============================================================================
# 
# cat("Black Marble 2021 pull finished:", as.character(Sys.time()), "\n")
# 
# sink(type = "message")
# sink()
# 
# close(log_con)
# 
# # ==============================================================================
# # sample table 
# # ==============================================================================
# 
# ntl_sa_q1_21 <- bind_rows(
#   ntl_sa_01_21,
#   ntl_sa_02_21,
#   ntl_sa_03_21
# )
# 
# ntl_summary_q1_21 <- ntl_sa_q1_21 %>% 
#   mutate(
#     month = floor_date(date, unit = "month")
#   ) %>% 
#   group_by(month) %>% 
#   summarise(
#     n_obs = n(),
#     n_admin2 = n_distinct(GID_2),
#     n_days = n_distinct(date),
#     mean_ntl = mean(ntl_mean, na.rm = TRUE),
#     sd_ntl = sd(ntl_mean, na.rm = TRUE),
#     min_ntl = min(ntl_mean, na.rm = TRUE),
#     max_ntl = max(ntl_mean, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# ntl_summary_q1_21

