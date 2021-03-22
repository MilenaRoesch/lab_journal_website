library(tidyverse)
library(lubridate)
library(maps)
library(scales)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# Challenge 1 -----------------
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
  scale_color_brewer(palette = "Accent") +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/2020",
    x = "Year 2020",
    y = "Confirmed CoVID-19 cases",
    color = "Country"
  ) +
  theme_gray() +
  theme(legend.position  = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45)) +
  scale_x_date(
    date_breaks = "1 month", date_labels = "%B",
    date_minor_breaks = "1 month"
  ) +
  scale_y_continuous(
    breaks = seq(0, 15000000, by = 2500000),
    labels = scales::label_number_si(accuracy = 0.1),
    minor_breaks = seq(0, 10000000, by = 1250000)
  )

# Challenge 2 ----------
world <- map_data("world")
cumulative_deaths_per_country_tbl <- covid_data_tbl %>%
  select(popData2019, deaths, countriesAndTerritories) %>%
  group_by(countriesAndTerritories) %>% 
  summarise(total_deaths = sum(deaths), population_2019 = median(popData2019)) %>%
  ungroup() %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  )) %>%
  mutate (mortality_rate = total_deaths/population_2019) %>%
  rename("region" = "countriesAndTerritories")

world_mortality_rate_covid <- left_join(world, cumulative_deaths_per_country_tbl)
# 
ggplot(world_mortality_rate_covid) +
  geom_map(aes(map_id = region, fill = mortality_rate), map = world_mortality_rate_covid, data = world_mortality_rate_covid) +
  expand_limits(x = world_mortality_rate_covid$long, y = world_mortality_rate_covid$lat) +
  labs(
        title = "Mortality rate of COVID-19 worldwide",
        subtitle = "As of 11/2020",
        x = NULL,
        y = NULL,
        fill = "Mortality Rate"
      ) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  ) +
  scale_fill_gradient(
    low = "#D80000",
    high = "#3F0000",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill",
    breaks = seq(0.0000, 0.0012, by=0.0003), 
    labels = scales::percent_format(accuracy = 0.001)
    )

