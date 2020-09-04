#Challenge 1 ----

# 1.0 Load libraries ---
library("tidyverse")
library(readr)

# 2.0 Importing files ---
sellers_tbl <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_sellers_dataset.csv")
order_items_tbl <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_order_items_dataset.csv")
orders_tbl      <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_orders_dataset.csv")
# 3.0 Examining Data ---
glimpse(sellers_tbl)

# 4.0 Wrangling data
orders_sellers_joined_tbl <- sellers_tbl %>% 
                              left_join(order_items_tbl) %>%
                              left_join(orders_tbl)

orders_sellers_wrangled_tbl <- orders_sellers_joined_tbl %>%
  separate(col    = seller.location,
           into   = c("city.seller.location", "state.seller.location"),
           sep    = ", ",
           convert = T) %>%
  mutate(total.price = freight.value + price) %>%
  
  # Reorganize
  select(-shipping.limit.date, -order.approved.at) %>%
  select(-starts_with("product.")) %>%   #?ends_with --> select helpers
  select(-ends_with(".date")) %>%
  
  # Rename
  rename(order_date = order.purchase.timestamp) %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))

#Sales by location and year
library(lubridate)

# Step 1 - Manipulate

revenue_by_loc_year_tbl <- orders_sellers_wrangled_tbl %>% 
  
  # Select columns
  select(order_date, total_price, state_seller_location) %>% 
  mutate(year = year(order_date)) %>% 
  
  # Filter  > 1.000.000
  group_by(state_seller_location) %>% 
  filter(sum(total_price) > 1000000) %>% 
  ungroup() %>% 
  
  # Group by and summarize year and main catgegory
  group_by(year, state_seller_location) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  
  # Format $ Text
  mutate(revenue_text = scales::dollar(revenue))


# Step 2 - Visualize
revenue_by_loc_year_tbl %>% 
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = revenue, fill = state_seller_location)) +
  
  # Geometries
  geom_col() +
  facet_wrap(~ state_seller_location) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Revenue by year and location",
    subtitle = "Each location shows an upward trend",
    fill = "Location"
  )

# Challenge 1 part 2 - Initial Analysis using English category translations --
# 1.0 Load libraries ----
#install.packages("readxl")
library("tidyverse")
library(readr)
library(readxl)
# 2.0 Importing Files ----
order_items_tbl <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_order_items_dataset.csv") 
products_tbl    <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_products_dataset.csv")
orders_tbl      <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_orders_dataset.csv")
translation_prod_cat_tbl      <- read_excel("00_data/01_e-commerce/01_raw_data/product_category_name_translation.xlsx")

# 3.0 Examining Data ----
glimpse(translation_prod_cat_tbl)
products_tbl
glimpse(orders_tbl)

#Excurse: Rename
translation_prod_cat_changed_tbl <- translation_prod_cat_tbl %>%
  set_names(names(.) %>% str_replace_all("\\_", "."))

# 4.0 Joining Data ----
order_items_joined_tbl <- order_items_tbl %>%
  left_join(orders_tbl) %>%
  left_join(products_tbl)

order_items_engl_joined_tbl <- order_items_joined_tbl %>%
  left_join(translation_prod_cat_changed_tbl)

# 5.0 Wrangling Data ----
order_items_engl_wrangled_tbl <- order_items_engl_joined_tbl %>%
  separate(col    = product.category.name.english,
           into   = c("main.category.name", "sub.category.name"),
           sep    = " - ",
           remove = FALSE) %>%
  mutate(total.price = price + freight.value) %>%
  select(-product.category.name, -shipping.limit.date, order.approved.at) %>%
  select(-starts_with("product.")) %>%
  select(-ends_with(".date")) %>%
  bind_cols(order_items_engl_joined_tbl %>% select(product.id)) %>%
  select(contains("timestamp"), contains(".id"),
         main.category.name, sub.category.name, price, freight.value, total.price,
         everything()) %>% 
  rename(order_date = order.purchase.timestamp) %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

library(lubridate)

revenue_by_year_engl_tbl <- order_items_engl_wrangled_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>% 
  summarize(revenue = sum(total_price)) %>%
  mutate(revenue_text = scales::dollar(revenue))


# Step 2 - Visualize
revenue_by_year_engl_tbl %>%
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
revenue_by_year_engl_cat_main_tbl <- order_items_engl_wrangled_tbl %>%
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
revenue_by_year_engl_cat_main_tbl %>%
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
fs::dir_create("00_data/01_e-commerce/04_wrangled_data_student/challenge1")

# 7.1 Excel ----
#install.packages("writexl")
library("writexl")
write_xlsx(order_items_wrangled_tbl, "00_data/01_e-commerce/04_wrangled_data_student/challenge1/order_items.xlsx")

# 7.2 CSV ----
write.csv(order_items_wrangled_tbl, "00_data/01_e-commerce/04_wrangled_data_student/challenge1/order_items.csv")
# 7.3 RDS ----
saveRDS(order_items_wrangled_tbl, "00_data/01_e-commerce/04_wrangled_data_student/challenge1/order_items.rds")
