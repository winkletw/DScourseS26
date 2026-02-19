library(fredr)
library(jsonlite)
library(tidyverse)
library(httr)

# fred practice-----------------------------------------------------------------
endpoint <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=",Sys.getenv("FRED_KEY"),"&file_type=json")

df <- fromJSON(endpoint) %>%
  as_tibble()

head(df$observations)

df2 <- fredr(
  series_id = "GNPCA",
  observation_start = as.Date("1948-01-01"),
  observation_end = as.Date("2020-01-01")
)

# college scorecard practice----------------------------------------------------
library(rscorecard)

sc_key(Sys.getenv("USGOV_API_KEY"))

# download some data
df3 <- sc_init() %>% 
  sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
  sc_select(unitid, instnm, stabbr, ugds) %>% 
  sc_year("latest") %>% 
  sc_get()

