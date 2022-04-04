# get key table 
xp <- '//*[@id="mw-content-text"]/div[1]/table'
url <- "https://en.wikipedia.org/wiki/Pitch_class"
library(rvest)
library(tidyverse)

key_table <- url %>% 
  read_html() %>% 
  html_elements(xpath = xp) %>%
  html_table() %>%
  flatten_df()

key_table <- key_table %>% 
  clean_names() %>% 
  mutate(tonal_counterparts = gsub("\\ .*", "", tonal_counterparts),
         tonal_counterparts = str_remove_all(tonal_counterparts, ","),
         pitchclass = as.integer(gsub("\\,.*", "", pitchclass))) %>% 
  select(-solfege)

saveRDS(key_table, here("data", "key_table.rds"))