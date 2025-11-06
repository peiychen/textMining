rm(list = ls())
library(here)
library(janitor)
library(tidyverse)
library(tidytext)
library(ggthemes)

theme_set(theme_bw() +
  theme(panel.grid = element_line(linewidth = 1 / 5)))

load(here("work", "gen", "DN_Opinions_wrangle.RData"))

table(opinions_df$category_rc)

#' only n=2 as OTHER - may consider removing them in the subsequent analysis

opinions_df |>
  count(category_rc, year) |>
  ggplot(aes(x = year, y = n, fill = category_rc)) +
  geom_col() +
  labs(x = "Year", y = "# Articles", fill = "Category") +
  scale_fill_colorblind()

#' opinions concentrate in the last two years


# most frequent words by category -----------------------------------------

tidy_opinions <- opinions_df |>
  filter(category_rc != "OTHER") |>
  unnest_tokens(output = word, input = body) |>
  anti_join(stop_words)

tidy_opinions |>
  group_by(category_rc) |>
  count(word, sort = TRUE) |>
  slice_max(n, n = 20) -> category_words_top

category_words_top <- category_words_top |>
  group_by(word) |>
  mutate(
    word_count = sum(n),
    cate_count = length(category_rc)
  )

category_words_top_w <- category_words_top |>
  group_by(word) |>
  pivot_wider(names_from = category_rc, values_from = n, values_fill = 0)


category_words_top_w <- clean_names(category_words_top_w)

category_words_top_w |>
  arrange(desc(cate_count), desc(word_count)) |>
  pull(word) -> word_order

category_words_top |>
  ggplot(aes(
    x = factor(category_rc, levels = c("OPINION", "EDITORIAL", "LETTER TO THE EDITOR")),
    y = factor(word, rev(word_order)), fill = n
  )) +
  geom_tile() +
  labs(x = "", y = "", fill = "N") +
  scale_fill_viridis_c(option = "B") +
  theme(panel.grid = element_blank())

category_words_top |>
  ungroup() |>
  ggplot(aes(x = n, y = reorder_within(word, n, category_rc), fill = category_rc)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~category_rc, scales = "free") +
  labs(x = "N", y = "") +
  scale_y_reordered() +
  scale_fill_colorblind()


# TF-IDF by category ------------------------------------------------------

category_words <- tidy_opinions |>
  count(category_rc, word, sort = TRUE) |>
  bind_tf_idf(word, category_rc, n)

category_words |>
  group_by(category_rc) |>
  slice_max(tf_idf, n = 15) |>
  ungroup() |>
  ggplot(aes(
    x = tf_idf, y = reorder_within(word, tf_idf, category_rc),
    fill = category_rc
  )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~category_rc, scales = "free") +
  labs(x = "TF-IDF", y = "") +
  scale_y_reordered() +
  scale_fill_colorblind()


# save data ---------------------------------------------------------------

save.image(here("work", "gen", "DN_Opinions_eda.RData"))
