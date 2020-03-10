library(tidyverse)
library(jsonlite)
library(httr)
library(rvest)
library(WikipediR)

#scraping content of Wikipedia page using API
r <- GET("https://en.wikipedia.org/w/api.php",
          query = list(
            action = "parse",
            page = "Steven Adams",
            prop = "text",
            format = "json"
          ))

stop_for_status(r)

json <- content(r, as = "text")
test <- fromJSON(json)
test$parse$text$`*` %>% 
  read_html() %>% 
  html_text() %>% 
  str_remove_all("\n")

#scraping content of Wikipedia page using WikipediaR package
wiki = page_content(language = "en", "wikipedia", page_name = "Steven Adams")
wiki$parse$text$'*' %>% 
  read_html() %>% 
  html_text() %>% 
  str_remove_all("\n")
