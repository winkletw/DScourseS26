# PS4a_Winkle.R
# Load libraries
library(jsonlite)
library(tidyverse)

# 5(a): Download JSON
system('wget -O dates.json "https://www.vizgr.org/historical-event$')

# 5(b): Print raw JSON
system('cat dates.json')

# 5(c): Convert to dataframe
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# 5(d): Print classes
print("--- Class of mydf ---")
print(class(mydf))
print("--- Class of mydf$date ---")
print(class(mydf$date))

# 5(e): Print head
print("--- Head of mydf ---")
print(head(mydf))

