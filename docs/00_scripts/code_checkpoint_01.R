# datascience at NIT ------------------------------------------------------

# 1.0 Load libraries ----

library(tidyverse)
# library(tibble)    --> is a modern re-imagining of the data frame
# library(readr)     --> provides a fast and friendly way to read rectangular data like csv
# library(dplyr)     --> provides a grammar of data manipulation
# library(magrittr)  --> offers a set of operators which make your code more readable (pipe operator)
# library(tidyr)     --> provides a set of functions that help you get to tidy data
# library(stringr)   --> provides a cohesive set of functions designed to make working with strings as easy as possible
# library(purrr)     --> enhances R’s functional programming (FP) toolkit
# library(lubridate) --> makes working with dates & times easier  
# library(ggplot2)   --> graphics

# 2.0 Importing Files ----
?read_csv()

order_items_tbl <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_order_items_dataset.csv")
products_tbl    <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_products_dataset.csv")
orders_tbl      <- read_csv(file = "00_data/01_e-commerce/01_raw_data/olist_orders_dataset.csv")

# 3.0 Examining Data ----
order_items_tbl
glimpse(order_items_tbl)

# 4.0 Joining Data ----
?left_join()

# by automatically detecting common column
left_join(order_items_tbl, products_tbl)

# if no common col name
left_join(order_items_tbl, products_tbl, by = c("product.id" = "product.id"))

# Chaining commands with the pipe
order_items_joined_tbl  <- order_items_tbl %>% 
                            left_join(orders_tbl) %>% 
                            left_join(products_tbl) 

# Examine
order_items_joined_tbl %>% glimpse()


# 5.0 Wrangling Data ----
order_items_wrangled_tbl <- order_items_joined_tbl %>%
  
  # Separate product category name in main and sub
  separate(col    = product.category.name,
           into   = c("main.category.name", "sub.category.name"),
           sep    = " - ",
           remove = FALSE) %>%
  
  # total price (price + freight value) <-- mutate
  # glimpse()
  mutate(total.price = price + freight.value) %>% 
  
  # Reorginize
  select(-shipping.limit.date, -order.approved.at) %>%
  select(-starts_with("product.")) %>%   #?ends_with --> select helpers
  select(-ends_with(".date")) %>%
  
  # Add back
  bind_cols(order_items_joined_tbl %>% select(product.id)) %>% 
  
  # Reorder
  select(contains("timestamp"), contains(".id"),
         main.category.name, sub.category.name, price, freight.value, total.price,
         everything()) %>% 
  
  # Rename
  rename(order_date = order.purchase.timestamp) %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))


# 6.0 Business Insights ----
# 6.1 Sales by Year ----
library(lubridate)
# Step 1 - Manipulate

revenue_by_year_tbl <- order_items_wrangled_tbl %>% 
  
  # Select columns
  select(order_date, total_price) %>% 
  # Add year column
  mutate(year = year(order_date)) %>% 
  
  #  Grouping by year and summarizing sales
  group_by(year) %>% 
  summarize(revenue = sum(total_price)) %>% 
  
  # ungroup() %>% 
  
  # Optional: Add a column that turns the numbers into a currency format (makes it in the plot optically more appealing)
  # You need to install the scales package
  mutate(revenue_text = scales::dollar(revenue))

# Step 2 - Visualize

revenue_by_year_tbl %>% 
  
  # Setup canvas with year (x-axis) and revenue (y-axis)
  ggplot(aes(x = year, y = revenue)) + 
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # alternatively use Hex codes e.g. #2DC6D6
  geom_label(aes(label = revenue_text)) +
  geom_smooth(method = "lm", se = FALSE) + 
  
  # Formatting
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "",
    y = "Revenue"
  )


# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
revenue_by_year_cat_main_tbl <- order_items_wrangled_tbl %>% 
  
  # Select columns
  select(order_date, total_price, main_category_name) %>% 
  mutate(year = year(order_date)) %>% 
  
  # Filter  > 1.000.000
  group_by(main_category_name) %>% 
  filter(sum(total_price) > 1000000) %>% 
  ungroup() %>% 
  
  # Group by and summarize year and main catgegory
  group_by(year, main_category_name) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>% 
  
  # Format $ Text
  mutate(revenue_text = scales::dollar(revenue))


# Step 2 - Visualize
revenue_by_year_cat_main_tbl %>% 
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = revenue, fill = main_category_name)) +
  
  # Geometries
  geom_col() +
  facet_wrap(~ main_category_name) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category"
  )


# 7.0 Writing Files ----

# If you want to interact with the filesystem use the fs package
install.packages("fs")
library(fs)
fs::dir_create("00_data/01_e-commerce/04_wrangled_data_student")

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
order_items_wrangled_tbl %>% 
  write_xlsx("00_data/01_e-commerce/04_wrangled_data_student/order_items.xlsx")

# 7.2 CSV ----
# CSV is a good option when others may use different languages such as Python, Java or C++
order_items_wrangled_tbl %>% 
  write_csv("00_data/01_e-commerce/04_wrangled_data_student/order_items.csv")

# 7.3 RDS ----
# a format used exclusively by R to save any R object in it's native format
order_items_wrangled_tbl %>% 
  write_rds("00_data/01_e-commerce/04_wrangled_data_student/order_items.rds")
