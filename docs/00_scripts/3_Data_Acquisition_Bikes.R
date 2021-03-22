# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(stringr)
library(RSQLite)

# 2.0 OPEN URL ----
url_home          <- "https://www.radon-bikes.de/roadbike/carbon/bikegrid/"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for category of carbon bikes
html_home         <- read_html(url_home)

# 3.0 WEB SCRAPING ----
#Web scrape the product names
product_name_tbl <- html_home %>%
  #Get the nodes for the product names ...
  html_nodes(css = ".m-bikegrid__info > a > div > h4") %>%
  # ...and extract the text of the html attribute
  html_text() %>%
  # Delete the unnecessary formatting
  str_replace_all("[\t\n\r\v\f]" , "") %>%
  # Delete whitespace
  trimws() %>%
  # Convert vector to tibble
  enframe(name = "position", value = "product_name")

#Web scrape the product prices
product_price_tbl <- html_home %>%
  #Get the nodes for the product prices ...
  html_nodes(css = ".currency_eur > .m-bikegrid__price--active") %>%
  # ...and extract the information of the span attribute
  html_text("span") %>%
  # Convert vector to tibble
  enframe(name = "position", value = "product_price")

# 4.0 DATABASE  ----
# Create a database
conn <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "00_data/03_bike_shop/Radon_carbon_bikes.sqlite")
# Write tables to database
dbWriteTable(conn, "bike_names", product_name_tbl, overwrite=TRUE)
dbWriteTable(conn, "bike_prices", product_price_tbl, overwrite=TRUE)

# List tables of database 
# dbListTables(conn)

# Get price and respective product Name from database
dbGetQuery(conn, "SELECT product_name, product_price FROM bike_names, bike_prices WHERE bike_names.position = bike_prices.position")
dbDisconnect(conn)



