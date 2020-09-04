# datascience at NIT ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library("tidyverse")
library(readr)
# 2.0 Importing Files ----
order_items_tbl <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_order_items_dataset.csv") 
products_tbl    <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_products_dataset.csv")
orders_tbl      <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_orders_dataset.csv")

# 3.0 Examining Data ----
glimpse(order_items_tbl)
products_tbl
glimpse(orders_tbl)

# 4.0 Joining Data ----
order_items_joined_tbl <- order_items_tbl %>%
  left_join(orders_tbl) %>%
  left_join(products_tbl)

order_items_joined_tbl$product.category.name

order_items_joined_tbl %>% 
  select(product.category.name) %>%
  filter(str_detect(product.category.name, "moveis")) %>% 
  unique()

# 5.0 Wrangling Data ----
order_items_wrangled_tbl <- order_items_joined_tbl %>%
  separate(col    = product.category.name,
           into   = c("main.category.name", "sub.category.name"),
           sep    = " - ",
           remove = FALSE) %>%
  mutate(total.price = price + freight.value) %>%
  select(-shipping.limit.date, order.approved.at) %>%
  select(-starts_with("product.")) %>%
  select(-ends_with(".date")) %>%
  bind_cols(order_items_joined_tbl %>% select(product.id)) %>%
  select(contains("timestamp"), contains(".id"),
         main.category.name, sub.category.name, price, freight.value, total.price,
         everything()) %>% 
  rename(order_date = order.purchase.timestamp) %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))
  
# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

library(lubridate)

revenue_by_year_tbl <- order_items_wrangled_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>% 
  summarize(revenue = sum(total_price)) %>%
  mutate(revenue_text = scales::dollar(revenue))


# Step 2 - Visualize
revenue_by_year_tbl %>%
  ggplot(aes(x = year, y = revenue)) +
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = revenue_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  scale_y_continuous(labels = scales::dollar) + # Change the y-axis
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
revenue_by_year_cat_main_tbl <- order_items_wrangled_tbl %>%
  select(order_date, total_price, main_category_name) %>% 
  mutate(year = year(order_date)) %>%
  group_by(main_category_name) %>% 
  filter(sum(total_price) > 1000000) %>% # If you run the code up here, R will tell you that we have 6 groups
  ungroup() %>%
  group_by(year, main_category_name) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>%
  mutate(revenue_text = scales::dollar(revenue))
# Step 2 - Visualize
revenue_by_year_cat_main_tbl %>%
  ggplot(aes(x = year, y = revenue, fill = main_category_name)) +
  geom_col() + # Run up to here to get a stacked bar plot
  facet_wrap(~ main_category_name) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" 
  )

# 7.0 Writing Files ----
#install.packages("fs")
library(fs)
fs::dir_create("00_data/01_e-commerce/04_wrangled_data_student")

# 7.1 Excel ----
#install.packages("writexl")
library("writexl")
write_xlsx(order_items_wrangled_tbl, "00_data/01_e-commerce/04_wrangled_data_student/order_items.xlsx")

# 7.2 CSV ----
write.csv(order_items_wrangled_tbl, "00_data/01_e-commerce/04_wrangled_data_student/order_items.csv")
# 7.3 RDS ----
saveRDS(order_items_wrangled_tbl, "00_data/01_e-commerce/04_wrangled_data_student/order_items.rds")
