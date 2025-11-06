rm(list = ls())
library(here)
library(tidyverse)
library(tidytext)
library(ggthemes)
library(topicmodels)

theme_set(theme_bw() +
  theme(panel.grid = element_line(linewidth = 1 / 5)))

load(here("work", "gen", "DN_Opinions_eda.RData"))

opinions_df <- opinions_df |>
  filter(category_rc != "OTHER")

tidy_opinions |>
  count(word, sort = TRUE)

stop_words2 <- stop_words |>
  bind_rows(
    tibble(
      word = c(
        "students", "student", # "people",
        "it’s", "college",
        "dailynebraskan.com", "daily", "nebraskan", "dn"
      ),
      lexicon = "custom"
    )
  )

tidy_opinions <- opinions_df |>
  unnest_tokens(output = word, input = body) |>
  anti_join(stop_words2)

# convert to document-term-matrix
opinions_dtm <- tidy_opinions |>
  count(id, word) |>
  cast_dtm(id, word, n)


# LDA ---------------------------------------------------------------------

lda_init <- LDA(opinions_dtm, k = 3, control = list(seed = 68503))
lad_topics <- tidy(lda_init, matrix = "beta")

lda_top_terms <- lad_topics |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  ungroup() |>
  arrange(topic, -beta)

lda_top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(x = beta, y = factor(term))) +
  geom_col() +
  labs(x = "Beta", y = "") +
  scale_y_reordered() +
  facet_wrap(~topic, scales = "free")

lda_update <- LDA(opinions_dtm, k = 20, control = list(seed = 68503))
lad_topics <- tidy(lda_update, matrix = "beta")

lda_top_terms <- lad_topics |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  ungroup() |>
  arrange(topic, -beta)

lda_top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(x = beta, y = factor(term))) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~topic, scales = "free")


lda_documents <- tidy(lda_update, matrix = "gamma")

lda_documents <- lda_documents |>
  left_join(
    opinions_df |>
      select(id, category_rc),
    by = join_by(document == id)
  )

lda_documents |>
  group_by(category_rc, document) |>
  slice_max(gamma, n = 1) -> lda_documents_topic

lda_documents_topic |>
  group_by(category_rc, topic) |>
  tally() |>
  ungroup() |>
  group_by(topic) |>
  mutate(
    N = sum(n),
    frac = n / N
  ) |>
  ungroup() -> lda_documents_topic_summ

lda_documents_topic_summ |>
  ggplot(aes(
    x = fct_reorder(factor(topic), N, .desc = TRUE),
    y = n, fill = category_rc
  )) +
  geom_col() +
  labs(x = "Topic", y = "# Articles", fill = "Category") +
  scale_y_continuous(n.breaks = 6) +
  scale_fill_colorblind()
