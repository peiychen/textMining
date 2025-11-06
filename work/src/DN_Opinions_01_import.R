rm(list = ls())
library(here)
library(tidyverse)
library(rvest)

load(here("work", "gen", "DN_Opinions_scrape.RData"))

opinions <- list.files(here("work", "data", "DN_Opinions"))
# opinions <- str_remove(opinions, ".html")
hrefs <- unique(c(letters_to_hrefs_clean, editorial_hrefs_clean, columns_hrefs_clean))

# read html files
opinions_htmls <- lapply(
  opinions,
  function(x) {
    read_html(here("work", "data", "DN_Opinions", x),
      encoding = "utf-8"
    )
  }
)


# time
opinions_htmls[[1]] |>
  html_element("time") |>
  html_attr("datetime")

# author
opinions_htmls[[1]] |>
  html_element("span[itemprop='author']") |>
  html_text(trim = TRUE)

# title
opinions_htmls[[1]] |>
  html_element("h1.headline") |>
  html_text(trim = TRUE)

# body
opinions_htmls[[1]] |>
  html_elements("div#article-body > p") |>
  html_text(trim = TRUE)

opinions_df <- tibble(id = NULL, title = NULL, time = NULL, author = NULL, body = NULL)

for (i in 1:length(opinions_htmls)) {
  time <- opinions_htmls[[i]] |>
    html_element("time") |>
    html_attr("datetime")

  title <- opinions_htmls[[i]] |>
    html_element("h1.headline") |>
    html_text(trim = TRUE)

  author <- opinions_htmls[[i]] |>
    html_element("span[itemprop='author']") |>
    html_text(trim = TRUE)

  body <- opinions_htmls[[i]] |>
    html_elements("div#article-body > p") |>
    html_text(trim = TRUE)

  opinions_df <- opinions_df |>
    bind_rows(
      tibble(
        id = opinions[i],
        title = title,
        time = time,
        author = author,
        body = paste(body, collapse = "  ")
      )
    )
}

save(opinions_df, file = here("work", "gen", "DN_Opinions_import.RData"))
