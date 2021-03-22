# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

#specifying the url for desired amazon page to be scraped
url <- "https://www.amazon.de/s?k=electric+scooters&__mk_de_DE=%C3%85M%C3%85%C5%BD%C3%95%C3%91&ref=nb_sb_noss_2"

webdata = read_html(url)
webdata %>%
  html_nodes("span") %>%
  html_text(".a-text-normal")
