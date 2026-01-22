
library(tidyverse)
library(tidytext)
library(textdata)
library(syuzhet)
library(sentimentr)
library(ggplot2)
library(dplyr)
library(stringr)
library(text2vec)
library(glmnet)
library(pROC)
library(caret)
library(tidytext)
library(ggthemes)
library(wordcloud)
library(RColorBrewer)
library(DT)

# Cargar léxicos
bing  <- get_sentiments("bing")
nrc <- syuzhet::get_sentiment_dictionary("nrc", language = "english")
afinn    <- get_sentiments("afinn")

books_with_chapters_shatterme_empyrean <- readRDS("data/processed/books_shatterme_empyrean.rds")

books_with_chapters_shadowhunters <- readRDS("data/processed/books_shadowhunters.rds")

# Tokenización por palabra, manteniendo estructura de capítulo
tidy_books <- books_with_chapters_shatterme_empyrean %>%
  filter(nchar(trimws(text)) > 0) %>%
  group_by(book, chapter_num, saga) %>%
  unnest_tokens(word, text) %>%
  ungroup()

tidy_books_clean <- tidy_books %>%
  mutate(word = str_replace_all(word, "[^a-z']+", "")) %>%
  filter(nchar(word) > 0)

# Eliminación de palabras contextuales no emocionales
data("stop_words")

custom_stop_words <- tibble(
  word = c(
    "professor", "commander", "dragon", "cadet", "rebel", "soldier",
    "hunter", "angel", "warlock", "shadowhunter", "fae", "highlord",
    "queen", "king", "prince", "princess", "vampire", "werewolf",
    "tracker", "demon", "magician", "seelie", "unseelie", "wyvern",
    "xaden", "tairn", "violet", "juliette", "adam", "kent", "warner",
    "rafe", "jace", "clary", "simon", "isabelle", "alec", "magnus",
    "tris", "caleb", "rhiannon", "peter", "al", "chris", "will", "tessa", "ridoc", "andarna", "jessamine"
  ),
  lexicon = "custom"
) %>%
  bind_rows(stop_words)

tidy_books_clean <- tidy_books_clean %>%
  anti_join(custom_stop_words, by = "word")

nrc_chapter <- tidy_books_clean %>%
  inner_join(nrc, by = "word") %>%
  count(book, chapter_num, sentiment, sort = TRUE) %>%
  group_by(book, chapter_num) %>%
  mutate(total_words = sum(n),
         saga = case_when(
           book %in% c("Shatter Me", "Unravel Me", "Ignite Me") ~ "Shatter Me",
           book %in% c("Fourth Wing", "Iron Flame", "Onyx Storm") ~ "Empyrean",
           TRUE ~ "Otra")) %>%
  ungroup() %>%
  mutate(prop = n / total_words)

# Capítulos con mayor proporción de "fear"
top_fear <- nrc_chapter %>%
  filter(sentiment == "fear") %>%
  arrange(desc(prop)) %>%
  head(5)

knitr::kable(top_fear, caption = "Top 5 capítulos con mayor proporción de 'fear'")

# Wordcloud de Empyrean

empyrean_words <- books_with_chapters_shatterme_empyrean %>%
  filter(book %in% c("Fourth Wing", "Iron Flame", "Onyx Storm")) %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_replace_all(word, "[^a-z']", "")) %>%
  anti_join(custom_stop_words, by = "word") %>%
  filter(nchar(word) > 2)

word_freq <- empyrean_words %>%
  count(word, sort = TRUE)

# ---- abrir dispositivo TIFF ----

tiff(
  filename = "output/graficos/wordcloud_empyrean.tiff",
  width = 2000,
  height = 1500,
  res = 300
)

set.seed(123)

wordcloud(
  words = word_freq$word,
  freq = word_freq$n,
  max.words = 150,
  random.order = FALSE,
  rot.per = 0.2,
  colors = brewer.pal(8, "Set1"),
  scale = c(4, 0.5)
)

dev.off()
set.seed(123)
wordcloud(
  words = word_freq$word,
  freq = word_freq$n,
  max.words = 150,
  random.order = FALSE,
  rot.per = 0.2,
  colors = brewer.pal(8, "Set1"),
  scale = c(4, 0.5),
  title = "Nube de palabras: saga Empyrean"
)

afinn_chapter <- tidy_books_clean %>%
  inner_join(afinn, by = "word") %>%
  group_by(book, chapter_num) %>%
  summarise(
    afinn_mean = mean(value),
    .groups = "drop"
  )

# Asignar saga según nombre del libro (AJUSTADO)
evolucion <- afinn_chapter %>%
  mutate(
    saga = case_when(
      str_detect(tolower(book), "shatter") ~ "Shatter Me",
      str_detect(tolower(book), "fourth|iron|onyx") ~ "Empyrean",
      str_detect(tolower(book), "mortal|infernal|dark|heavenly") ~ "Shadowhunters",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(saga)) %>%
  group_by(saga, chapter_num) %>%
  summarise(sent_mean = mean(afinn_mean), .groups = "drop")


# =========================
# GRÁFICO
# =========================

g1 <- ggplot(evolucion, aes(chapter_num, sent_mean, color = saga)) +
  geom_line(alpha = 0.7) +
  geom_smooth(se = FALSE, span = 0.3) +
  labs(
    x = "Número de capítulo (relativo)",
    y = "Puntuación media AFINN",
    title = "Evolución del sentimiento emocional por saga"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave(
  "output/graficos/evolucion_sentimiento.png",
  g1,
  dpi = 300,
  width = 10,
  height = 6
)


# Comparación emocional
comparacion <- nrc_chapter %>%
  mutate(
    saga = case_when(
      str_detect(tolower(book), "shatter") ~ "Shatter Me",
      str_detect(tolower(book), "fourth|iron|onyx") ~ "Empyrean",
      str_detect(tolower(book), "mortal|infernal|dark|heavenly") ~ "Shadowhunters",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(saga)) %>%
  group_by(saga, sentiment) %>%
  summarise(media_prop = mean(prop), .groups = "drop")

g2 <- ggplot(comparacion, aes(sentiment, media_prop, fill = saga)) +
  geom_col(position = "dodge") +
  labs(
    x = "Emoción",
    y = "Proporción media por capítulo",
    title = "Comparación emocional entre sagas"
  )  +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )
ggsave("output/graficos/comparacion_emocional.png", g2, dpi = 300, width = 10, height = 6)

# Evolución por capítulo relativo
nrc_chapter_saga <- nrc_chapter %>%
  mutate(
    saga = case_when(
      str_detect(tolower(book), "shatter") ~ "Shatter Me",
      str_detect(tolower(book), "fourth|iron|onyx") ~ "Empyrean",
      str_detect(tolower(book), "mortal|infernal|dark|heavenly") ~ "Shadowhunters",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(saga))

nrc_chapter_saga <- nrc_chapter_saga %>%
  arrange(saga, book, chapter_num) %>%
  group_by(saga) %>%
  mutate(chapter_rel = row_number()) %>%
  ungroup()

emocion_por_saga <- nrc_chapter_saga %>%
  group_by(saga, chapter_rel, sentiment) %>%
  summarise(prop_media = mean(prop), .groups = "drop")

g3 <- ggplot(emocion_por_saga, aes(x = chapter_rel, y = prop_media, color = saga)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ sentiment, scales = "free_y", ncol = 3) +
  labs(
    x = "Capítulo (progreso dentro de la saga)",
    y = "Proporción media de palabras con la emoción",
    title = "Evolución de emociones por saga",
    color = "Saga"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

ggsave("output/graficos/evolucion_emociones.png", g3, dpi = 300, width = 12, height = 8)
print(g3)

# Cargar como líneas
books_lines <- books_with_chapters_shatterme_empyrean %>%
  mutate(text_clean = str_squish(text)) %>%
  filter(nchar(text_clean) > 0) %>%
  group_by(book) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  select(book, linenumber, text = text_clean)


tidy_by_line <- books_lines %>%
  unnest_tokens(word, text)

WINDOW_SIZE <- 100

sentiment_by_window <- tidy_by_line %>%
  inner_join(bing, by = "word") %>%
  mutate(index = linenumber %/% WINDOW_SIZE) %>%
  count(book, index, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_net = positive - negative)

g4 <- ggplot(sentiment_by_window, aes(x = index, y = sentiment_net, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") +
  labs(
    x = glue::glue("Tramos de {WINDOW_SIZE} líneas"),
    y = "Sentimiento neto (positivo − negativo)",
    title = "Evolución del sentimiento en las novelas seleccionadas"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )
ggsave("output/graficos/sentimiento_por_ventanas.png", g4, dpi = 300, width = 12, height = 8)

emociones_clave <- c("anger", "fear", "sadness", "disgust", "surprise", "anticipation", "joy", "trust")

# Comparación por saga
top_emotion_words_by_saga <- tidy_books_clean %>%
  mutate(
    saga = case_when(
      str_detect(tolower(book), "shatter|unravel|ignite") ~ "Shatter Me",
      str_detect(tolower(book), "fourth|iron|onyx") ~ "Empyrean",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(saga)) %>%
  inner_join(nrc, by = "word") %>%
  filter(sentiment %in% emociones_clave) %>%
  count(saga, sentiment, word, sort = TRUE) %>%
  group_by(saga, sentiment) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, interaction(saga, sentiment)))

g5 <- ggplot(top_emotion_words_by_saga, aes(n, word, fill = saga)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_y_reordered() +
  facet_wrap(~sentiment, scales = "free_y", ncol = 2) +
  labs(
    x = "Frecuencia absoluta",
    y = NULL,
    title = "Top 10 palabras por emoción: comparación entre sagas",
    fill = "Saga"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )
ggsave("output/graficos/top_palabras_emocion_saga.png", g5, dpi = 300, width = 14, height = 20)

# Top global
top_emotion_words <- tidy_books_clean %>%
  inner_join(nrc, by = "word") %>%
  filter(sentiment %in% emociones_clave) %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, sentiment))

g6 <- ggplot(top_emotion_words, aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    x = "Frecuencia en el corpus",
    y = NULL,
    title = "Palabras que más activan cada emoción (NRC - Plutchik)"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )
ggsave("output/graficos/top_palabras_emocion_global.png", g6, dpi = 300, width = 14, height = 10)

nrc_polarity <- syuzhet::get_sentiment_dictionary("nrc", language = "english") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  mutate(polarity_score = ifelse(sentiment == "positive", 1, -1)) %>%
  select(word, polarity_score)

valencia_nrc <- tidy_books_clean %>%
  inner_join(nrc_polarity, by = "word") %>%
  group_by(book, chapter_num) %>%
  summarize(valencia_nrc = mean(polarity_score), .groups = "drop")

valencia_combinada <- afinn_chapter %>%
  select(book, chapter_num, afinn_mean) %>%
  inner_join(valencia_nrc, by = c("book", "chapter_num")) %>%
  mutate(saga = ifelse(str_starts(book, "shatter"), "Shatter Me", "Empyrean"))

valencia_combinada_long <- valencia_combinada %>%
  pivot_longer(cols = c(afinn_mean, valencia_nrc), 
               names_to = "método", 
               values_to = "puntuación") %>%
  mutate(método = recode(método, 
                         afinn_mean = "AFINN (-5 a +5)", 
                         valencia_nrc = "NRC (polaridad binaria)"))

g7 <- ggplot(valencia_combinada_long, aes(chapter_num, puntuación, color = método)) +
  geom_line(alpha = 0.7, size = 1) +
  facet_wrap(~saga, scales = "free_y") +
  labs(
    title = "Comparación de valencia emocional: AFINN vs NRC",
    x = "Capítulo",
    y = "Puntuación de sentimiento",
    color = "Método"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )
ggsave("output/graficos/comparacion_afinn_nrc.png", g7, dpi = 300, width = 12, height = 8)

chapters_text <- books_with_chapters_shatterme_empyrean %>%
  group_by(book, chapter_num, saga) %>%
  summarise(text_full = paste(text, collapse = " "), .groups = "drop")

sentimentr_results <- chapters_text %>%
  mutate(
    sentences = purrr::map(text_full, ~ get_sentences(.x)),
    sent_data = purrr::map(sentences, ~ sentiment(.x))
  ) %>%
  select(book, chapter_num, sent_data, saga) %>%
  unnest(sent_data)

sentimentr_results_clean <- sentimentr_results %>%
  group_by(book, chapter_num, saga) %>%
  filter(n() > 0) %>%
  ungroup()

# Versión robusta
chapters_text <- books_with_chapters_shatterme_empyrean %>%
  mutate(text_clean = str_squish(text)) %>%
  filter(nchar(text_clean) > 0) %>%
  group_by(book, chapter_num, saga) %>%
  summarise(
    text_full = paste(text, collapse = " "),
    n_chars = nchar(text_full),
    .groups = "drop"
  ) %>%
  filter(chapter_num > 0, n_chars > 50)

sentimentr_results <- chapters_text %>%
  mutate(
    sent_data = purrr::map(text_full, ~ {
      sents <- get_sentences(.x)
      if (length(sents) == 0) return(tibble::tibble(sentence = character(), sentiment = numeric()))
      sentimentr::sentiment(sents)
    })
  ) %>%
  select(book, chapter_num, sent_data) %>%
  tidyr::unnest(sent_data, keep_empty = TRUE) %>%
  mutate(
    saga = case_when(
      book %in% c("Shatter Me", "Unravel Me", "Ignite Me") ~ "Shatter Me",
      book %in% c("Fourth Wing", "Iron Flame", "Onyx Storm (The Empyrean Book 3") ~ "Empyrean",
      TRUE ~ "Otra"   # <--- Esto evita NA
    )
  ) %>%
  filter(!is.na(sentiment))



# Capítulos extremos
extremos_por_saga <- sentimentr_results %>%
  group_by(saga, book, chapter_num) %>%
  summarise(
    mean_sent = mean(sentiment),
    n_frases = n(),
    .groups = "drop"
  ) %>%
  group_by(saga) %>%
  filter(
    mean_sent == min(mean_sent) | 
      mean_sent == max(mean_sent)
  ) %>%
  arrange(saga, desc(mean_sent)) %>%
  mutate(tipo = ifelse(mean_sent == max(mean_sent), "MÁS POSITIVO", "MÁS NEGATIVO"),
         tipo = factor(tipo, levels = c("MÁS POSITIVO", "MÁS NEGATIVO"))) %>%
  ungroup()


# Tabla en kable
knitr::kable(
  extremos_por_saga %>%
    select(Saga = saga, Tipo = tipo, Libro = book, Capítulo = chapter_num, 
           `Sentimiento prom.` = mean_sent, `Frases` = n_frases) %>%
    mutate(`Sentimiento prom.` = round(`Sentimiento prom.`, 4)),
  caption = "Capítulos más positivos y negativos por saga",
  align = c("l", "l", "l", "r", "r", "r")
)






# analisis dataset books
# Limpieza de palabras
tidy_books_dataset <- readRDS("data/processed/tidy_books_dataset.rds")
books_with_chapters_dataset <- readRDS("data/processed/books_with_chapters_dataset.rds")

tidy_books_clean_dataset <- tidy_books_dataset %>%
  mutate(word = str_replace_all(word, "[^a-z']+", "")) %>%
  filter(nchar(word) > 0)

# Stop words personalizadas
custom_stop_words <- tibble(
  word = c(
    "professor", "commander", "dragon", "cadet", "rebel", "soldier",
    "hunter", "angel", "warlock", "shadowhunter", "fae", "highlord",
    "queen", "king", "prince", "princess", "vampire", "werewolf",
    "tracker", "demon", "magician", "seelie", "unseelie", "wyvern"
  ),
  lexicon = "custom"
) %>%
  bind_rows(stop_words)

# Filtrar stop words
tidy_books_clean_dataset <- tidy_books_clean_dataset %>%
  anti_join(custom_stop_words, by = "word")

# --- Resumen del corpus (ahora con saga) ---
corpus_summary_dataset <- books_with_chapters_dataset %>%
  group_by(book, saga) %>%  # ← incluir saga aquí
  summarise(
    total_lines = n(),
    total_chapters = max(chapter_num, na.rm = TRUE),
    .groups = "drop"
  )


DT::datatable(
  corpus_summary_dataset,
  caption = "Caracterización del corpus: libros del dataset agrupados por saga (sagas populares ~2010–2015)",
  options = list(
    pageLength = 25,
    searching = TRUE,
    ordering = TRUE,      # ← permite ordenar al hacer clic en columnas
    autoWidth = TRUE,
    scrollX = TRUE
  ),
  colnames = c(
    "Libro" = "book",
    "Saga" = "saga",
    "Líneas totales" = "total_lines",
    "Capítulos totales" = "total_chapters"
  )
)

# Preprocesado unificado
clean_text_for_model <- function(text) {
  text %>%
    str_to_lower() %>%
    str_replace_all("[^a-z\\s']", "") %>%
    str_squish()
}

chapters_classic <- books_with_chapters_dataset %>%
  group_by(book, chapter_num) %>%
  summarise(text_full = paste(text, collapse = " "), .groups = "drop") %>%
  mutate(text_full = clean_text_for_model(text_full)) %>%
  filter(nchar(text_full) > 100)

chapters_empyrean <- books_with_chapters_dataset %>%
  filter(book %in% c("fourth_wing", "iron_flame", "onyx_storm")) %>%
  group_by(book, chapter_num) %>%
  summarise(text_full = paste(text, collapse = " "), .groups = "drop") %>%
  mutate(text_full = clean_text_for_model(text_full)) %>%
  filter(nchar(text_full) > 100)

# Etiqueta de referencia
get_bing_afinn_score <- function(text) {
  tokens <- tibble(word = unlist(str_split(text, "\\s+"))) %>%
    filter(nchar(word) > 0)
  
  bing_score <- tokens %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    summarise(score = mean(ifelse(sentiment == "positive", 1, -1))) %>%
    pull(score)
  
  afinn_score <- tokens %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    summarise(score = mean(value)) %>%
    pull(score)
  
  if (is.na(bing_score) && is.na(afinn_score)) return(NA_real_)
  if (is.na(bing_score)) return(afinn_score)
  if (is.na(afinn_score)) return(bing_score)
  return((bing_score + afinn_score) / 2)
}

chapters_classic <- chapters_classic %>%
  mutate(
    sent_ref = map_dbl(text_full, get_bing_afinn_score),
    sent_label = ifelse(sent_ref > 0, 1, 0)
  ) %>%
  filter(!is.na(sent_ref))

# Entrenamiento
set.seed(123)
train_idx <- createDataPartition(chapters_classic$sent_label, p = 0.8, list = FALSE)
train_data <- chapters_classic[train_idx, ]
test_data  <- chapters_classic[-train_idx, ]

it_train <- itoken(train_data$text_full, tokenizer = word_tokenizer, ids = train_data$book, progressbar = FALSE)
it_test  <- itoken(test_data$text_full, tokenizer = word_tokenizer, ids = test_data$book, progressbar = FALSE)

vocab <- create_vocabulary(it_train) %>%
  prune_vocabulary(
    term_count_min = 5,
    doc_proportion_max = 0.5
  )
vocab <- vocab %>% 
  filter(!term %in% stop_words$word)

vectorizer <- vocab_vectorizer(vocab)

dtm_train <- create_dtm(it_train, vectorizer)
tfidf <- TfIdf$new()
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- create_dtm(it_test, vectorizer) %>% transform(tfidf)

model <- cv.glmnet(
  x = dtm_train_tfidf,
  y = train_data$sent_label,
  family = "binomial",
  alpha = 1,
  nfolds = 5
)

preds_test <- predict(model, dtm_test_tfidf, type = "response")
auc_test <- auc(test_data$sent_label, preds_test)
cat("AUC en test:", round(auc_test, 3), "\n")

# Aplicar a Empyrean
it_empyrean <- itoken(chapters_empyrean$text_full, tokenizer = word_tokenizer, ids = chapters_empyrean$book, progressbar = FALSE)
dtm_empyrean <- create_dtm(it_empyrean, vectorizer) %>% transform(tfidf)
preds_empyrean <- predict(model, dtm_empyrean, type = "response")

chapters_empyrean <- chapters_empyrean %>%
  mutate(sent_custom = preds_empyrean)

# Comparación con Bing
bing_empyrean <- chapters_empyrean %>%
  unnest_tokens(word, text_full) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(book, chapter_num) %>%
  summarise(bing_score = mean(ifelse(sentiment == "positive", 1, -1)), .groups = "drop")

comparison <- chapters_empyrean %>%
  left_join(bing_empyrean, by = c("book", "chapter_num")) %>%
  mutate(saga = "Empyrean")

comparison_long <- comparison %>%
  pivot_longer(cols = c(sent_custom, bing_score), names_to = "method", values_to = "sentiment") %>%
  mutate(method = recode(method, sent_custom = "Modelo personalizado", bing_score = "Léxico Bing"))

# Guardar gráfico 1
p1 <- ggplot(comparison_long, aes(chapter_num, sentiment, color = method)) +
  geom_line(alpha = 0.7, size = 0.5) +
  facet_wrap(~ method, scales = "free_y") +
  labs(
    x = "Capítulo",
    y = "Sentimiento",
    title = "Comparación: modelo personalizado vs Bing en Empyrean"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave("output/graficos/comparacion_modelo_bing.png", p1, dpi = 300, width = 10, height = 6)


# Tabla comparativa final
all_chapters <- bind_rows(
  books_with_chapters %>% mutate(corpus = "Empyrean"),
  books_with_chapters_dataset %>% mutate(corpus = "Clásicas")
) %>%
  group_by(book) %>%
  mutate(saga = case_when(
    corpus == "Empyrean" ~ "Empyrean (2023–2025)",
    TRUE ~ "Sagas Clásicas (2008–2015)"
  )) %>%
  ungroup()

tidy_all <- all_chapters %>%
  filter(nchar(trimws(text)) > 0) %>%
  group_by(book, chapter_num, saga) %>%
  unnest_tokens(word, text) %>%
  ungroup() %>%
  mutate(word = str_replace_all(word, "[^a-z']+", "")) %>%
  anti_join(custom_stop_words, by = "word")

bing_all <- tidy_all %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(saga, book, chapter_num) %>%
  summarise(bing_score = mean(ifelse(sentiment == "positive", 1, -1)), .groups = "drop") %>%
  group_by(saga) %>%
  summarise(
    Capítulos = n(),
    Tono_promedio = round(mean(bing_score), 3),
    Desviación = round(sd(bing_score), 3),
    .groups = "drop"
  )

knitr::kable(bing_all, caption = "Comparación de tono emocional usando el léxico Bing")

# Palabras más predictivas
coefs <- coef(model, s = "lambda.min")
nonzero <- which(coefs != 0)
words_impact <- data.frame(
  word = rownames(coefs)[nonzero],
  weight = as.numeric(coefs[nonzero])
) %>%
  arrange(desc(weight))

nrc_words <- nrc$word %>% unique()
words_impact_emotional <- words_impact %>%
  filter(word %in% nrc_words) %>%
  arrange(desc(abs(weight))) %>%
  slice_head(n = 30)


# Guardar gráfico 2
p2 <- ggplot(words_impact_emotional,
             aes(x = weight, y = reorder(word, weight), fill = weight > 0)) +
  geom_col() +
  scale_fill_manual(
    values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"),
    labels = c("Negativo", "Positivo"),
    name = "Polaridad"
  ) +
  labs(
    x = "Peso en el modelo",
    y = NULL,
    title = "Palabras emocionales más influyentes en el clasificador"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave("output/graficos/palabras_predictivas.png", p2, dpi = 300, width = 10, height = 8)


chapters_all <- bind_rows(
  books_with_chapters %>%
    mutate(corpus = "Empyrean"),
  books_with_chapters_dataset %>%
    mutate(corpus = "Clásicas")
)

chapters_full <- chapters_all %>%
  group_by(book, chapter_num, corpus) %>%
  summarise(text_full = paste(text, collapse = " "), .groups = "drop") %>%
  filter(nchar(text_full) > 100)

sentimentr_results_all <- chapters_full %>%
  mutate(
    sent_data = map(text_full, ~ {
      sents <- get_sentences(.x)
      if (length(sents) == 0) return(tibble(sentiment = NA_real_))
      sent_df <- sentiment(sents)
      tibble(sentiment = mean(sent_df$sentiment, na.rm = TRUE))
    })
  ) %>%
  unnest(sent_data) %>%
  mutate(
    saga = case_when(
      corpus == "Empyrean" ~ "Empyrean (2023–2025)",
      TRUE ~ "Sagas Clásicas (2008–2015)"
    )
  ) %>%
  filter(!is.na(sentiment))

p3 <- ggplot(sentimentr_results_all, aes(x = sentiment, fill = saga)) +
  geom_density(alpha = 0.6, linewidth = 0.2) +
  scale_fill_manual(
    values = c("Empyrean (2023–2025)" = "#4A90E2", "Sagas Clásicas (2008–2015)" = "#FF8C00")
  ) +
  labs(
    x = "Tono emocional (sentimentr: media por capítulo)",
    y = "Densidad",
    title = "Distribución del tono emocional por saga"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave("output/graficos/distribucion_tono_emocional.png", p3, dpi = 300, width = 10, height = 6)


