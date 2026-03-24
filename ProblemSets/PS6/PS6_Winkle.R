library(tidyverse)
library(lubridate)

# ==============================================================================
# load data 
# ==============================================================================
df_raw <- read_csv("acled_raw.csv")

# ==============================================================================
# clean data
# ==============================================================================

# create clean df 
df_clean <- df_raw 

# filter to include only protests and riots
df_clean <- df_clean %>% filter(event_type %in% c("Protests", "Riots"))

# keep events only 2012 onward
df_clean <- df_clean %>%
  filter(year >= 2012)

# create month-level date to count events by month.
df_clean <- df_clean %>% 
  mutate(month_year = floor_date(event_date, unit = "month"))

# ==============================================================================
# time series plot of protests and riots in Africa
# ==============================================================================

# count number of events by month and event type
monthly_counts <- df_clean %>%
  count(month_year, event_type)

# time series of protests and riots by month
p1 <- ggplot(monthly_counts, aes(x = month_year, y = n, color = event_type)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Monthly Protest and Riot Events in Africa",
    x = "Month-Year",
    y = "Number of Events",
    color = "Event Type"
  ) +
  theme_classic()

ggsave("PS6a_Winkle.png", plot = p1, width = 10, height = 6, dpi = 300, device = "png", type = "cairo")

# ==============================================================================
# table of protests and riots by country 2012-2025
# ==============================================================================

# count number of events by country and event type
country_counts <- df_clean %>%
  count(country, event_type)

# pivot   
country_table <- country_counts %>%
  pivot_wider(
    names_from = event_type,
    values_from = n, 
    values_fill = list(n = 0)
 )

# create total col and sort by total
country_table <- country_table %>%
  mutate(Total = Protests + Riots) %>%
  arrange(desc(Total))

# table
top15_table <- country_table %>%
  select(country, Protests, Riots, Total) %>%
  slice(1:15)

# print table
top15_table 

# ==============================================================================
# total protests/riots by countries common to infrastructure literature
# ==============================================================================

# filter to include only countries common in infrastructure literature
litreview_df <- df_clean %>%
  filter(country %in% c("Ghana", "Nigeria", "South Africa", "Kenya"))

# count number of events by country and year
litreview_yearly <- litreview_df %>%
  count(country, year)

# facet wrap 
p2 <- ggplot(litreview_yearly, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~ country) +
  labs(
    title = "Yearly Total of Protest and Riot Events",
    x = "Year",
    y = "Number of Events"
  ) +
  theme_classic()

ggsave("PS6b_Winkle.png", plot = p2, width = 10, height = 6, dpi = 300, device = "png", type = "cairo")




