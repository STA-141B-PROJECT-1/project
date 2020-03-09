library(tidyverse)
library(jsonlite)
library(httr)
library(rvest)

r <- GET("https://en.wikipedia.org/w/api.php",
          query = list(
            action = "parse",
            page = "Steven Adams",
            prop = "wikitext",
            format = "json"
          ))

stop_for_status(r)

json <- content(r, as = "text")
test <- fromJSON(json)
test$parse$wikitext %>% 
  str_remove_all("\n")
