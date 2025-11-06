rm(list = ls())
library(here)
library(tidyverse)

load(here("work", "gen", "DN_Opinions_import_add.RData"))


# parse time --------------------------------------------------------------

# convert to POSIXct
opinions_df <- opinions_df |>
  mutate(datetime = ymd_hms(time))

# fix irregular datetime format
opinions_df |>
  filter(is.na(datetime)) |>
  select(time) |>
  pull() -> time_format_wrong

parse_date_time(
  time_format_wrong,
  orders = "I:M p, a b d, Y", tz = ""
) -> time_format_correct

opinions_df$datetime[is.na(opinions_df$datetime)] <- time_format_correct

# extract year
opinions_df <- opinions_df |>
  mutate(
    year = year(datetime),
    month = month(datetime),
    year_month = paste0(year, "-", formatC(month, width = 2, flag = 0))
  )

# check temporal trend
barplot(table(opinions_df$year))
barplot(table(opinions_df$year_month))


# parse categories --------------------------------------------------------

as.matrix(table(str_extract(str_to_upper(opinions_df$title), "^[^:]+")))

opinions_df <- opinions_df |>
  mutate(category = str_extract(str_to_upper(opinions_df$title), "^[^:]+"))

as.matrix(table(opinions_df$category))

# # correct spelling
# opinions_df$category_rc <- NA
# opinions_df$category_rc[opinions_df$category == "OPINON"] <- "OPINION"

category_check <- c(
  "FARRELL",
  "FIGHT AGAINST ALZHEIMER'S",
  "FORMER DAILY NEBRASKAN EMPLOYEE FABRICATED QUOTES",
  "KENYON",
  "LETTER FROM THE EDITOR",
  "LOOKING BACK AND MOVING FORWARD",
  "MILLER",
  "NOSTALGIA IS A CRUTCH FOR GEN Z",
  "WE CAN'T PROMISE GOOD NEWS, BUT WE CAN PROMISE QUALITY NEWS"
)

category_check <- sort(category_check)

opinions_df |>
  filter(category %in% category_check) |>
  select(category, title, author) |>
  arrange(category)

opinions_df <- opinions_df |>
  mutate(category_rc = case_match(
    category,
    c("COLUMN", "OPINON", category_check[c(1, 2, 4, 7, 8)]) ~ "OPINION",
    c("STAFF", "STAFF EDITORIAL", category_check[c(6, 9)]) ~ "EDITORIAL",
    category_check[c(3, 5)] ~ "OTHER",
    .default = category
  ))

table(opinions_df$category_rc)
table(opinions_df$category, opinions_df$category_rc)


# save data ---------------------------------------------------------------

write_csv(opinions_df, file = here("work", "gen", "opinions_df.csv"))
save(opinions_df, file = here("work", "gen", "DN_Opinions_wrangle.RData"))
