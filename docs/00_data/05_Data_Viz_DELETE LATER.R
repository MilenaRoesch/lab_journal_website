library(tidyverse)
library(lubridate)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# Format data ----
cumulative_cases_by_day_tbl <- covid_data_tbl %>%
  select(dateRep, cases, countriesAndTerritories) %>%
  filter(countriesAndTerritories %in% c("Germany","United_Kingdom", "France", "Spain", "United_States_of_America")) %>% 
  
  # Grouping by country & date, and cumulatively summarize number of cases
  group_by(countriesAndTerritories) %>%
  mutate(dateRep = as.Date(dateRep, "%d/%m/%Y")) %>%
  arrange(dateRep) %>%
  mutate(cumulative_cases = cumsum(cases)) %>%
  ungroup()

#cumulative_cases_by_day_tbl

# Plot ----
cumulative_cases_by_day_tbl %>%
  ggplot(aes(x = dateRep, y = cumulative_cases, color = countriesAndTerritories)) +
  geom_line(size = 1) + 
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/2020",
    x = "Date",
    y = "Confirmed CoVID-19 cases",
    color = "Country"
    ) +
  theme_minimal() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45)) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%b",
    date_minor_breaks = "1 month"
    ) 