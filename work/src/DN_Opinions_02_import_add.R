rm(list = ls())
library(here)
library(tidyverse)
library(rvest)

load(here("work", "gen", "DN_Opinions_import.RData"))

# check article lengths
summary(sapply(opinions_df$body, function(x) str_count(x, "\\W+")))

#' one article has zero word count

opinions_df |>
  filter(str_count(opinions_df$body, "\\W+") == 0) |>
  pull(id) -> id

html <- read_html(here("work", "data", "DN_Opinions", id),
  encoding = "utf-8"
)

html |>
  html_elements("div#article-body > div > p") |>
  html_text(trim = TRUE) -> body


opinions_df$body[opinions_df$id == id] <- paste(body, collapse = "  ")

# check article lengths again
summary(sapply(opinions_df$body, function(x) str_count(x, "\\W+")))

save(opinions_df, file = here("work", "gen", "DN_Opinions_import_add.RData"))
