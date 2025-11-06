library(stm)

processed <- textProcessor(opinions_df$body,
  metadata = opinions_df
)

plotRemoved(processed$documents, lower.thresh = seq(1, 10, by = 1))

out <- prepDocuments(
  processed$documents,
  processed$vocab,
  processed$meta,
  lower.thresh = 2
)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

stm_init <- stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = 10,
  prevalence = ~category_rc,
  max.em.its = 75,
  init.type = "Spectral",
  verbose = FALSE
)

plot(stm_init,
  type = "summary", n = 10,
  xlim = c(0, 1)
)

findingk <- searchK(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = c(5:30),
  prevalence = ~category_rc,
  verbose = FALSE
)

plot(findingk)

stm_update <- stm(
  documents = docs,
  vocab = vocab,
  data = meta,
  K = 20,
  prevalence = ~category_rc,
  max.em.its = 75,
  init.type = "Spectral",
  verbose = FALSE
)

plot(stm_update,
  type = "summary", n = 10,
  xlim = c(0, 0.75)
)

meta$category_rc <- as.factor(meta$category_rc)

predict_topics <- estimateEffect(
  formula = 1:20 ~ category_rc, stmobj = stm_update,
  metadata = meta, uncertainty = "Global"
)

lapply(1:20, function(x) summary(predict_topics, topics = x))

#' - topic 2, 13, 16, 19 - less likely to be opinion
#' - topic 10, 15 - more likely to be opinion
#' - topic 11 - more likely to be letter to the editor

labelTopics(stm_update, c(2, 13, 16, 19, 10, 15))

plot(predict_topics,
  covariate = "category_rc",
  topics = c(2, 13, 16, 19, 10, 15),
  model = stm_update,
  method = "difference",
  cov.value1 = "EDITORIAL",
  cov.value2 = "OPINION",
  labeltype = "custom",
  custom.labels = c("T2", "T13", "T16", "T19", "T10", "T15"),
  xlab = "More OPINION ... More EDITORIAL"
)

toLDAvis(stm_update, docs = docs, R = 15, reorder.topics = FALSE)
