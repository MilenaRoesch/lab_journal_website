# Libraries

library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(plotly)
library(ggrepel)
library(ggthemes)

# Load information ----
# STOCK PRICES
sp_500_prices_tbl <- read_rds("00_data/06_challenge/sp_500_prices_tbl.rds") %>%
  select("symbol", "date", "adjusted")
# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("00_data/06_challenge/sp_500_index_tbl.rds") %>%
  select("company", "symbol", "sector")
# DAILY RETURNS
sp_500_daily_returns_tbl <- sp_500_prices_tbl %>%
  mutate(date = as.Date(date, "%Y-%M-%D")) %>%
  filter(date >= as.Date("2017-12-31")) %>%
  group_by(symbol) %>%
  mutate(previous_adjusted = lag(adjusted, n=1)) %>%
  filter(!is.na(previous_adjusted)) %>%
  mutate(pct_return = (adjusted - previous_adjusted)/previous_adjusted) %>%
  ungroup() %>%
  select("symbol", "date", "pct_return")

stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>%
  pivot_wider(
    names_from = "date",
    values_from = "pct_return",
    values_fill = 0
  )

 
kmeans_mapper <- function(center) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}


k_means_mapped_tbl <- tibble(centers = 1:30) %>%
  mutate(k_means = map(centers, kmeans_mapper)) %>%
  mutate(glance = map(k_means, glance))


# Visualize Scree Plot ----
k_means_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") +
  labs(
    title = "Scree Plot"
  )
# Visualization: UMAP ------
umap_results <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  umap()

# Convert umap results to tibble with symbols
umap_results_tbl <- umap_results$layout %>%
  as_tibble() %>%
  bind_cols(
    stock_date_matrix_tbl %>% select(symbol)
  )

# Visualize UMAP results
#k_means_mapped_official_tbl <- read_rds("00_data/06_challenge/k_means_mapped_tbl.rds")
#umap_results_official_tbl   <- read_rds("00_data/06_challenge/umap_results_tbl.rds")

umap_results_official_tbl %>%
  ggplot(aes(V1, V2)) + 
  geom_point(alpha=0.5) + 
  theme_tq() +
  labs(
    title = "UMAP Projection"
  )

# Combine K-means and UMAP ----
k_means_obj <- k_means_mapped_official_tbl %>%
  filter(centers == 10) %>%
  pull(k_means) %>%
  pluck(1)

umap_kmeans_results_tbl <- k_means_obj %>% 
  augment(stock_date_matrix_tbl) %>%
  select(symbol, .cluster) %>%
  left_join(umap_results_official_tbl, by = "symbol") %>%
  left_join((sp_500_index_tbl %>% select(symbol, company, sector)), by = "symbol")

# visualize combined k-means and umap results ----
ggplot(umap_kmeans_results_tbl, aes(V1, V2, color = .cluster)) +
  geom_point(
    na.rm = TRUE,
    alpha = 1/2
    ) +
  labs(
    title = "Visualization of k_means and UMAP results combined"
  ) +
  theme(legend.position = "none", 
  )