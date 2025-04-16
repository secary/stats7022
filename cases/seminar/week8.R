pacman::p_load(tidyverse, rvest, xml2)

URL <- "https://rvest.tidyverse.org/articles/starwars.html"
star_wars <- read_html(URL)
write_html(star_wars, file = "./week8/starwars.html")
star_wars


star_wars %>% 
  html_elements("h2") %>% 
  html_text2()

star_wars %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  url_absolute(URL)

URL2 <- "https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table"
olympics <- read_html(URL2)
write_html(olympics, file = "./week8/oplympics.html")
olympics

olympics %>% 
  html_elements("table") %>% 
  pluck(4) %>% 
  html_table()

star_wars %>% 
  html_elements(".director") %>% 
  html_text2()

URL3 <-  "https://www.imdb.com/title/tt0096283/"
totoro <- read_html(URL3)
write_html(totoro, file = "./week8/totoro.html")

totoro %>% 
  html_elements("img") %>% 
  pluck(7) %>% 
  html_attr("src")

URL4 <- "https://www.imdb.com/search/title/?year=2000&title_type=feature"
imdb <- read_html(URL4)

title <- imdb %>% 
  html_elements("h3.ipc-title__text") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(1:n() - 1) %>% 
  rename(title = value)

description <- imdb %>% 
  html_elements(".ipc-html-content-inner-div") %>% 
  html_text() %>% 
  as_tibble() %>% 
  rename(description = value)
  
rate <- imdb %>% 
  html_elements(".ipc-rating-star--rating") %>% 
  html_text() %>% 
  as.numeric() %>%
  tibble(rate = .)   

imdb_data <- bind_cols(c(title, description, rate))
imdb_data
