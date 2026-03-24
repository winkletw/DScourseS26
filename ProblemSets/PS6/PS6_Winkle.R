library(tidyverse)
library(lubridate)

# 1. Ingest only the necessary columns from the local directory
df_raw <- read_csv("acled_raw.csv")
