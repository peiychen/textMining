rm(list = ls())
library(here)
library(tidyverse)
library(rvest)
library(xml2)


# editorial ---------------------------------------------------------------

editorial <- read_html("https://www.dailynebraskan.com/opinion/editorials/")
editorial

# extract title
editorial |>
  html_elements(".tnt-headline") |>
  html_text(trim = TRUE) -> editorial_titles
editorial_titles <- unique(editorial_titles)

# extract urls
editorial |>
  html_elements(".tnt-headline") |>
  html_element("a") |>
  html_attr("href") -> editorial_hrefs
editorial_hrefs <- unique(editorial_hrefs) # keep only unique urls
editorial_hrefs # check results
editorial_hrefs[str_detect(editorial_hrefs, "/opinion/")] # contain "opinion"
editorial_hrefs[str_detect(editorial_hrefs, "/opinion/", negate = TRUE)] # not contain "opinion"
which(str_detect(editorial_hrefs, "/opinion/", negate = TRUE)) # find the index of not contain "opinion"

# remove the irrelevant entries
editorial_hrefs_clean <- editorial_hrefs[str_detect(editorial_hrefs, "/opinion/")]
editorial_titles_clean <- editorial_titles[1:37]


# letters to --------------------------------------------------------------

letters_to <- read_html("https://www.dailynebraskan.com/opinion/letters_to/")
letters_to

letters_to |>
  html_elements(".tnt-headline") |>
  html_elements("a") |>
  html_attr("href") -> letters_to_hrefs

letters_to_hrefs <- unique(letters_to_hrefs)
letters_to_hrefs[str_detect(letters_to_hrefs, "/opinion/", negate = TRUE)]
which(str_detect(letters_to_hrefs, "/opinion/", negate = TRUE))

letters_to_hrefs_clean <- letters_to_hrefs[1:25]

letters_to |>
  html_elements(".tnt-headline") |>
  html_text(trim = TRUE) -> letters_to_titles

letters_to_titles <- unique(letters_to_titles)
letters_to_titles_clean <- letters_to_titles[1:25]


# columns -----------------------------------------------------------------

columns <- read_html("https://www.dailynebraskan.com/opinion/columns/")
columns

columns |>
  html_elements(".tnt-headline") |>
  html_elements("a") |>
  html_attr("href") -> columns_hrefs

columns_hrefs <- unique(columns_hrefs)
columns_hrefs[str_detect(columns_hrefs, "/opinion/", negate = TRUE)]
which(str_detect(columns_hrefs, "/opinion/", negate = TRUE))
columns_hrefs_clean <- columns_hrefs[1:78]

columns |>
  html_elements(".tnt-headline") |>
  html_text(trim = TRUE) -> columns_titles

columns_titles <- unique(columns_titles)
columns_titles_clean <- columns_titles[1:78]


# download html files -----------------------------------------------------

base_url <- "https://www.dailynebraskan.com"

download_one_html <- function(href, remove_patterns) {
  target <- paste0(base_url, href)
  filename <- href
  filename <- str_remove(href, remove_patterns)
  filename <- str_remove(filename, "(?<=)\\/.+") # remove anything after the slash
  target |>
    download_html(
      file = here("work", "data", "DN_Opinions", paste0(basename(filename), ".html")),
      quiet = TRUE
    )
  message(paste("downloaded:", basename(filename)))
  Sys.sleep(runif(1, 10, 30))
}


# check url structure
str_remove(columns_hrefs_clean, "/opinion/")
columns_hrefs_clean[str_detect(str_remove(columns_hrefs_clean, "/opinion/"), "columns/")]
#' n=4 urls have an extra layer nested inside opinion
# str_remove(columns_hrefs_clean, "^/opinion/(columns/)?")

lapply(
  seq_along(columns_hrefs_clean),
  function(i) {
    href <- columns_hrefs_clean[i]
    download_one_html(href, "^/opinion/(columns/)?")
    message(paste0("download #", i))
  }
)

str_remove(editorial_hrefs_clean, "/opinion/")
editorial_hrefs_clean[str_detect(str_remove(editorial_hrefs_clean, "/opinion/"), "editorials/")]

# lapply(
#   seq_along(editorial_hrefs_clean),
#   function(i) {
#     href <- columns_hrefs_clean[i]
#     download_one_html(href, "^/opinion/(editorials/)?")
#     message(paste0("download #", i))
#   }
# )

str_remove(letters_to_hrefs_clean, "/opinion/")
letters_to_hrefs_clean[str_detect(str_remove(letters_to_hrefs_clean, "/opinion/"), "letters_to/")]

# lapply(
#   seq_along(editorial_hrefs_clean),
#   function(i) {
#     href <- columns_hrefs_clean[i]
#     download_one_html(href, "^/opinion/(letters_to/)?")
#     message(paste0("download #", i))
#   }
# )

save(
  columns_hrefs_clean, columns_titles_clean,
  editorial_hrefs_clean, editorial_titles_clean,
  letters_to_hrefs_clean, letters_to_titles_clean,
  file = here("work", "gen", "DN_Opinions_scrape.RData")
)
