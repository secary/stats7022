# Libs ----
pacman::p_load(tidyverse, rvest, magrittr)

# URL ----
URL <- "https://www.imdb.com/search/title/?year=2000&title_type=feature"
movies <- read_html(URL)

# Get info ----
# Found the main box using selectorgadget
title <- movies %>%
  html_elements(".ipc-metadata-list-summary-item") %>%
  html_element("h3") %>%
  html_text2()

description <- movies %>%
  html_elements(".ipc-metadata-list-summary-item") %>%
  html_element(".ipc-html-content-inner-div") %>%
  html_text2()

rating <- movies %>%
  html_elements(".ipc-metadata-list-summary-item") %>%
  html_element(".dli-ratings-container") %>%
  html_text2()

df <- tibble(title, description, rating)
df

# Clean data ----
df %<>%
  mutate(title = str_remove(title, "\\d+\\."),
         title = str_trim(title))

df %<>%
  mutate(rating = str_extract(rating, "\\d\\.\\d"),
         rating = parse_number(rating))

df

# Write out data ----
write_rds(df, "movies.rds")