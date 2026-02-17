library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"
webpage <- read_html(url)

# unofficial progression before the iaaf
pre_iaaf <- webpage %>%
  html_node(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11)") %>%
  html_table()

# records 1912–1976
iaaf_76 <- webpage %>%
  html_node(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(17) > tbody") %>%
  html_table()

# records 1977–present
iaaf_77 <- webpage %>%
  html_node(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(23) > tbody") %>%
  html_table()

# combine 1912–1976 and 1977–present records
records <- bind_rows(iaaf_76, iaaf_77)

# make a graph of the world record progression over time 
# x axis is calendar time (raw from is january 5, 1912 for example)
# y axis is time in seconds


# having manners
library(polite)

session <- bow("https://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression")
please <- scrape(session, query=list(t="semi-soft", per_page = 100)) %>%
  html_node(css = "#mw-content-text > div.mw-content-ltr.mw-parser-output > table:nth-child(11)") %>%
  html_table()

# tx space grants
txsc <- "https://space.texas.gov/grants/awards"
txsc_webpage <- read_html(txsc)

space_grants <- txsc_webpage %>%
  html_node(css = "#main > div:nth-child(5) > div > div > div > div > table") %>%
  html_table()



