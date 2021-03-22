# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

url_home          <- "https://flx.bike/"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
categories_tbl <- html_home %>%
  html_nodes(css = "sub-menu") %>%
  html_attr('category-title')
  
  # Remove the product families Gear and Outlet and Woman
  # (because the female bikes are also listed with the others)
  #discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  
  # Convert vector to tibble
  #enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  #mutate(
    #family_id = str_glue("#{family_class}")
  )

  
  # Get the nodes for the families ...
  html_nodes(css = "HorizontalList HorizontalList--spacingExtraLoose") %>%

# 2.0 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.canyon.com/en-de"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".js-navigationDrawer__list--secondary") %>%
  # ...and extract the infromation of the id attribute
  html_attr('id') %>%
  
  # Remove the product families Gear and Outlet and Woman
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"WMN|WOMEN|GEAR|OUTLET")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

bike_family_tbl


# 3.0 COLLECT PRODUCT CATEGORIES ----

# Combine all Ids to one string so that we will get all nodes at once (seperated by the OR operator ",")
family_id_css <- bike_family_tbl %>%
  .[["family_id"]] %>%
  stringr::str_c(collapse = ", ")

# Extract the urls of the entries from the href attribute
bike_category_tbl <- html_home %>%
  
  # Select nodes by the ids
  html_nodes(css = family_id_css) %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".navigationListSecondary__listItem .js-ridestyles") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = str_glue("https://www.canyon.com{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

# 4.0 COLLECT BIKE DATA ----

# 4.1 Get URL for each bike of the Product categories

# select first bike category url
bike_category_url <- bike_category_tbl$url[1]

# Alternatives for selecting values
# bike_url <- bike_category_tbl %$% url %>% .[1]
# bike_url <- bike_category_tbl %>% pull(url) %>% .[1]
# bike_url <- deframe(bike_category_tbl[1,])
# bike_url <- bike_category_tbl %>% first %>% first

xopen(bike_category_url)

# Get the URLs for the bikes of the first category
html_bike_category  <- read_html(bike_category_url)
bike_url_tbl        <- html_bike_category %>%
  
  # Get the 'a' nodes, which are hierarchally underneath
  # the class productTile__contentWrapper
  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href") %>%
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url")

# 4.2 Extract the descriptions (since we have retrieved the data already)
bike_desc_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
  
  # Extract the content of the attribute content
  html_attr("content") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "description")

# 4.3 Get even more data from JSON files
bike_json_tbl  <- html_bike_category %>%
  
  html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
  html_attr("data-gtm-impression") %>%
  
  # Convert the JSON format to dataframe
  # map runs that function on each element of the list
  map(fromJSON) %>% # need JSON ### need lists
  
  # Extract relevant information of the nested list
  map(purrr::pluck, 2, "impressions") %>% # Need purrr and expl above
  
  # Set "not defined" and emtpy fields to NA (will be easier to work with)
  map(na_if, "not defined") %>%
  map(na_if, "") %>%
  
  # The class of the col dimension56 varies between numeric and char.
  # This converts this column in each list to numeric
  map(~mutate_at(.,"dimension56", as.numeric)) %>%
  
  # Stack all lists together
  bind_rows() %>%
  # Convert to tibble so that we have the same data format
  as_tibble() %>%
  
  # Add consecutive numbers so that we can bind all data together
  # You could have also just use bind_cols()
  rowid_to_column(var='position') %>%
  left_join(bike_desc_tbl) %>%
  left_join(bike_url_tbl)


# 4.4 Wrap it into a function

get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Get JSON data
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>%
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate_at(.,"dimension56", as.numeric)) %>%
    map(~mutate_at(.,"price", as.numeric)) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

# Run the function with the first url to check if it is working
bike_category_url <- bike_category_tbl$url[1]
bike_data_tbl     <- get_bike_data(url = bike_category_url)

bike_data_tbl


# 4.4.1 Map the function against all urls

# Extract the urls as a character vector
bike_category_url_vec <- bike_category_tbl %>%
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)

saveRDS(bike_data_tbl, "bike_data_tbl.rds")


# 4.4.1 Alternative with a for loop

# Create an empty tibble, that we can populate
bike_data_tbl <- tibble()

# Loop through all urls
for (i in seq_along(bike_category_tbl$url)) {
  
  bike_category_url <- bike_category_tbl$url[i]
  bike_data_tbl         <- bind_rows(bike_data_tbl, get_bike_data(bike_category_url))
  
  # Wait between each request to reduce the load on the server
  # Otherwise we could get blocked
  Sys.sleep(5)
  
  # print the progress
  print(i)
}

# We have some duplicates
bike_data_cleaned_tbl %>%
  group_by(id) %>%
  filter(n()>1) %>%
  View()

# Filter non Canyon bikes (based on id length) and add an empty column for the colors
bike_data_cleaned_tbl <- bike_data_tbl %>%
  filter(nchar(.$id) == 4) %>%
  distinct(id, .keep_all = T)

# 5.0 Get all color variations for each bike ----

# Extract all bike urls
bike_url_vec <- bike_data_cleaned_tbl %>%
  pull(url)

# Create function to get the variations
get_colors <- function(url) {
  
  color_variations <- url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    gsub("^[^\\{]+", "", .) %>%
    # 2. replace at the end everything after the last "}" with ""
    gsub("[^\\}]+$", "", .) %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value") %>%
    
    # Paste all results into one string
    str_c(collapse = ";")
}

# Run the function over all urls and add result to bike_data_cleaned_tbl
# This will take a long time (~ 20-30 minutes) because we have to iterate over many bikes
bike_data_colors_tbl <- bike_data_cleaned_tbl %>%
  mutate(color_variations = map(bike_url_vec, get_colors))

saveRDS(bike_data_colors_tbl, "bike_data_colors_tbl.rds")

# 6.0 Create the urls for each variation ----
bike_data_wrangled_tbl <- bike_data_colors_tbl %>%
  
  # Remove unnecessary columns
  select(name, id, category, dimension50, dimension63, price, metric4, description, url, color_variations) %>%
  
  # Rename some columns
  rename(year = dimension50) %>%
  rename(price_dollar = price) %>%
  rename(price_euro = metric4) %>%
  
  # Make the data tyidy
  # Use str_count(test$color_variations, ";") %>% max() # 2 --> max 3 color variants
  separate(col  = color_variations,
           into = c("color1","color2","color3"),
           sep  = ";", convert = T) %>%
  pivot_longer(cols           = c("color1", "color2", "color3"),
               names_to       = "color",
               values_drop_na = T) %>%
  select(-color) %>%
  rename(color = value) %>%
  
  # Merge url and query parameters for the colors
  mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={color}")) %>%
  
  # Use library(stringi) to replace the last dash (HTLM format of a dash)
  mutate(url_color = ifelse(grepl(pattern = "/", .$color),
                            stringi::stri_replace_last_fixed(.$url_color, "/", "%2F"),
                            .$url_color))


# 7.0 Stock availability for each frame size ----
get_sizes <- function(url) {
  
  json <- url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    gsub("^[^\\{]+", "", .) %>%
    # 2. replace at the end everything after the last "}" with ""
    gsub("[^\\}]+$", "", .) %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON(flatten = T) %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 2) %>%
    
    # select(id, value, available, availability)# %>%
    select(id, value, availability.onlyXLeftNumber) %>%
    
    # Rename
    rename(id_size = id) %>%
    rename(size = value) %>%
    rename(stock_availability = availability.onlyXLeftNumber) %>%
    
    # Conver to tibble
    as_tibble()
  
}

# Pull url vector
bike_url_color_vec <- bike_data_wrangled_tbl %>%
  pull(url_color)

# Map
bike_data_sizes_tbl <- bike_data_wrangled_tbl %>%
  mutate(size = map(bike_url_color_vec, get_sizes))

# Unnest
bike_data_sizes_tbl <- bike_data_sizes_tbl %>%
  select(-url) %>%
  unnest(size)

saveRDS(bike_data_sizes_tbl, "bike_data_sizes_tbl.rds")