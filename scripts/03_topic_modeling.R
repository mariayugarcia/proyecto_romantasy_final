# scripts/03_topic_modeling.R
# Modelización temática con LDA guiado (seeded LDA) en The Mortal Instruments

library(tidyverse)
library(quanteda)
library(seededlda)
library(ggplot2)
library(ggthemes)
library(topicmodels)
library(knitr)
library(tidytext)


# --- 1. Cargar corpus procesado ---
books_with_chapters_shadowhunters <- readRDS("data/processed/books_shadowhunters.rds")

# Texto por capítulo
chapters_text <- books_with_chapters_shadowhunters %>%
  dplyr::filter(nchar(trimws(text)) > 0) %>%
  dplyr::group_by(book, chapter_num) %>%
  dplyr::summarise(chapter_text = paste(text, collapse = " "), .groups = "drop") %>%
  dplyr::mutate(document_id = paste(book, chapter_num, sep = "_Ch"))

# Agrupar en macro‑capítulos (cada 3)
chapters_text <- chapters_text %>%
  dplyr::mutate(big_chapter = ceiling(chapter_num / 3)) %>%
  dplyr::group_by(book, big_chapter) %>%
  dplyr::summarise(chapter_text = paste(chapter_text, collapse = " "), .groups = "drop") %>%
  dplyr::mutate(document_id = paste(book, big_chapter, sep = "_BCh"))

# Normalizar apóstrofes en el texto de capítulos
chapters_text$chapter_text <- chapters_text$chapter_text %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  str_replace_all("[‘’`]", "'")

# Crear corpus
corpus_poli <- corpus(
  chapters_text,
  text_field = "chapter_text",
  docid_field = "document_id"
)

stop_extra <- c(
  # funcionales / pronombres / auxiliares
  "the","a","an","of","to","and","i","you","he","she","it","we","they",
  "me","him","her","us","them","my","your","his","its","our","their",
  "this","that","these","those","there","here","where","when","what",
  "who","which","how","why","something","anything","nothing","everything",
  
  # verbos generales
  "am","is","are","was","were","be","been","being",
  "have","has","had","do","does","did","done","get","gets","got","getting",
  "make","makes","made","know","knew","known","think","thought","see","saw",
  "seen","come","comes","came","go","goes","went","going",
  "say","says","said","tell","tells","told",
  "want","wants","wanted","need","needs","needed",
  "can","could","may","might","must","shall","should","will","would","asked",
  "let","heard",
  
  # adverbios / partículas poco informativas
  "not","no","yes","just","now","then","ever","never","always","still",
  "even","really","very","well","too","also","back","around","away",
  "up","down","out","in","on","off","over","under","again","right","left",
  "took", "across",
  
  # partes del cuerpo y genéricos muy frecuentes
  "eyes","eye","hand","hands","hair","head","face","voice","feet","demons","fingers",
  
  # contracciones / formas sin apóstrofe
  "dont","didnt","doesnt","cant","couldnt","wouldnt","shouldnt","wont",
  "isnt","arent","wasnt","werent","havent","hasnt","hadnt",
  "im","ive","id","youre","youve","youll",
  "hes","shes","theyre","weve","theyll","ill","shell","hell","well",
  "thats","theres","whats","wheres","whos",
  "lets","youd","theyd","wed", "shed","hed","shouldve","wouldve","mightve","ought",
  
  # contracciones con apóstrofe 
  "don't","didn't","doesn't","can't","couldn't","wouldn't","shouldn't","won't",
  "isn't","aren't","wasn't","weren't","haven't","hasn't","hadn't",
  "i'm","i’ve","i'd","you're","you’ve","you'll",
  "he's","she's","they're","we're","we’ve","we'll","they’ll",
  "it's","that's","there's","what's","where's","who's",
  "let's","you'd","they'd","we'd","he'll",
  
  # palabras frecuentes
  "just","like","one","two",
  "said","say","says","tell","told","think","thought","know","knew",
  "feel","feels","felt","see","saw","seen","look","looks","looked",
  "want","wants","wanted","need","needs","needed","got","get","getting",
  "go","goes","going","went","come","comes","coming","came","back",
  "around","away","even","really","still","again", "phone","cell","police",
  "mr","mrs","maybe","someone","inside","seemed", "added","rat",
  "though","enough", "radio", "father's",
  
  
  # movimiento / lugar
  "turned","behind","toward","looking","walked","walk","running",
  "stood","standing","sat","sitting","moved","moving","around",
  
  # tiempo / espacio
  "time","room","door","long","little","much","way","light",
  "night","day","morning","evening","front","side","moment",
  "coffee","desk","chair","table","bag","car","truck","bus",
  "bar","hospital","hotel","apartment","room","building",
  "park","street","streets","road","avenue",
  
  
  # descripciones vagas
  "black","dark","white","red","blue"
)

names_extra <- c(
  "clary","jace","simon","alec","isabelle","luke","jocelyn",
  "valentine","sebastian","magnus","maia","raphael","jordan","maureen",
  "emma","hodge","will","jem","tessa","izzy","julian","helen","amatis",
  "camille","eric","maryse","jia","jace’s","clary’s","alec's","magnus's","sebastian's",
  "kyle","max","aline","zachariah","robert", "ty", "catarina", "azazel", "dorothea", "jeremiah"
)
stop_narrative <- c(
  "guy","guys","man","men","woman","women",
  "week","weeks","day","days",
  "party","band","crowd",
  "thing","things","something",
  "moment","moments", "yeah"
)

stop_all <- c(stopwords("en"), stop_extra, names_extra, stop_narrative)

# Tokenización para LDA
corpus_token <- corpus_poli %>%
  tokens(
    remove_punct   = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url     = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(stop_all)

dfm_clean <- corpus_token %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5, max_docfreq = 0.60 * ndoc(.)) %>%
  dfm_subset(ntoken(.) > 100)

dtm <- convert(dfm_clean, to = "topicmodels")

# Texto por libro
books_text <- books_with_chapters_shadowhunters %>%
  dplyr::filter(nchar(trimws(text)) > 0) %>%
  dplyr::group_by(book) %>%
  dplyr::summarise(book_text = paste(text, collapse = " "), .groups = "drop") %>%
  dplyr::mutate(document_id = book)

corpus_books <- corpus(books_text, text_field = "book_text", docid_field = "document_id")

# Tokenización específica
tokens_books <- corpus_books %>%
  tokens(
    remove_punct   = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url     = TRUE
  ) %>%
  tokens_tolower() %>%
  
  # eliminar cualquier token que contenga apóstrofe
  tokens_remove(pattern = "'.*", valuetype = "regex") %>%
  
  # eliminar restos no alfabéticos
  tokens_keep(pattern = "^[a-z]+$", valuetype = "regex") %>%
  
  # eliminar stopwords y nombres propios
  tokens_remove(stop_all)

dfm_books <- tokens_books %>% dfm()

term_freq_books <- dfm_books %>%
  convert(to = "data.frame")

# Normalizar nombre de la columna de documentos
if (".id" %in% colnames(term_freq_books)) {
  term_freq_books <- term_freq_books %>%
    dplyr::rename(doc_id = .id)
}

term_freq_books <- term_freq_books %>%
  tidyr::pivot_longer(
    cols      = -doc_id,
    names_to  = "term",
    values_to = "freq"
  ) %>%
  dplyr::filter(freq > 0) %>%
  dplyr::rename(book = doc_id)


top10_by_book <- term_freq_books %>%
  dplyr::group_by(book) %>%
  dplyr::slice_max(freq, n = 10, with_ties = FALSE) %>%
  dplyr::ungroup()

# Función de coherencia UMass
coherence_umass <- function(dfm, top_terms) {
  coherencias <- numeric(ncol(top_terms))
  for (i in seq_len(ncol(top_terms))) {
    palabras <- top_terms[, i]
    mat <- dfm[, palabras] > 0
    coh_i <- 0
    for (m in 2:length(palabras)) {
      for (l in 1:(m - 1)) {
        D_wm <- sum(mat[, m])
        D_wl_wm <- sum(mat[, l] & mat[, m])
        coh_i <- coh_i + log((D_wl_wm + 1) / D_wm)
      }
    }
    coherencias[i] <- coh_i
  }
  return(coherencias)
}

# Evaluar múltiples K
k_values <- 3:12
coherencias <- data.frame()

for (k in k_values) {
  set.seed(123)
  modelo_temp <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = 123, iter = 500))
  palabras_k <- topicmodels::terms(modelo_temp, 10)
  coh <- coherence_umass(dfm_clean, palabras_k)
  coherencias <- rbind(coherencias, data.frame(K = k, mean_coherence = mean(coh)))
}

# Guardar gráfico de coherencia
png("output/graficos/coherencia_lda.png", width = 800, height = 600)
print(
  ggplot(coherencias, aes(K, mean_coherence)) +
    geom_line(color = "red") +
    geom_point(color = "red") +
    labs(
      title = "Coherencia UMass por número de tópicos (LDA estándar)",
      x = "Número de tópicos (K)",
      y = "Coherencia media"
    ) +
    theme_economist() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(margin = margin(b = 15, t = 10), hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15))
    )
)
dev.off()

set.seed(123)
lda_k3 <- LDA(dtm, k = 3, method = "Gibbs",
              control = list(seed = 123, iter = 500))
lda_k4 <- LDA(dtm, k = 4, method = "Gibbs",
              control = list(seed = 123, iter = 500))
lda_k5 <- LDA(dtm, k = 5, method = "Gibbs",
              control = list(seed = 123, iter = 500))
lda_k6 <- LDA(dtm, k = 6, method = "Gibbs",
              control = list(seed = 123, iter = 500))
lda_k7 <- LDA(dtm, k = 7, method = "Gibbs",
              control = list(seed = 123, iter = 500))
lda_k8 <- LDA(dtm, k = 8, method = "Gibbs",
              control = list(seed = 123, iter = 500))

# Mostrar términos
kable(topicmodels::terms(lda_k3, 10),
      caption = "LDA estándar K = 3, top 10 términos")

kable(topicmodels::terms(lda_k4, 10),
      caption = "LDA estándar K = 4, top 10 términos")

kable(topicmodels::terms(lda_k5, 10),
      caption = "LDA estándar K = 5, top 10 términos")

kable(topicmodels::terms(lda_k6, 10),
      caption = "LDA estándar K = 6, top 10 términos")

kable(topicmodels::terms(lda_k7, 10),
      caption = "LDA estándar K = 7, top 10 términos")
kable(topicmodels::terms(lda_k8, 10),
      caption = "LDA estándar K = 8, top 18 términos")




topic_labels <- c(
  "1" = "Conflicto sobrenatural",
  "2" = "Mundo subterráneo",
  "3" = "Drama personal",
  "4" = "Relaciones y vínculos",
  "5" = "Consejo e instituciones"
)




# Matriz β (tópico × palabra)
lda_res <- tidy(lda_k5, matrix = "beta")

top_terms <- lda_res %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>%
  ungroup() %>%
  mutate(
    topic_label = topic_labels[as.character(topic)],
    term        = reorder_within(term, beta, topic_label)
  )

# Matriz θ (documento × tópico)
gamma_df <- tidy(lda_k5, matrix = "gamma")

gamma_books_prop <- gamma_df %>%
  mutate(
    book        = sub("_BCh.*", "", document),
    topic_label = topic_labels[as.character(topic)]
  ) %>%
  group_by(book, topic_label) %>%
  summarise(mean_gamma = mean(gamma), .groups = "drop")

dict_tmi <- dictionary(list(
  romance = c(
    "love", "kiss", "desire", "bed", "whisper",
    "heart", "together", "embrace", "touch", "closer"
  ),
  identity_parents = c(
    "mother", "father", "mom", "memory", "past", "truth", "identity", "family",
    "childhood", "lineage", "bloodline", "heritage", "curse", "morgenstern"
  ),
  shadowhunting_war = c(
    "battle", "fight", "war", "weapon", "blade",
    "demon", "angel", "rune", "stele", "seraph", "mark", "circle", "endarkened"
  ),
  clave_politics = c(
    "council", "law", "accords", "power", "inquisitor",
    "clave", "trial", "gard", "conclave", "ruling", "representative"
  ),
  downworlders = c(
    "warlock", "faerie", "werewolf", "seelie", "court",
    "vampire", "praetor", "nightchild", "unseelie", "pack", "clan"
  ),
  other = c(
    "magic", "spell", "portal", "mundane", "parabatai",
    "alec", "jace", "clary", "simon", "magnus"  
  )
))

chapters_text_seed <- books_with_chapters_shadowhunters %>%
  dplyr::filter(nchar(trimws(text)) > 0) %>%
  dplyr::group_by(book, chapter_num) %>%
  dplyr::summarise(
    chapter_text = paste(text, collapse = " "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(document_id = paste(book, chapter_num, sep = "_Ch"))


# Crear corpus
corpus_poli_seed <- corpus(
  chapters_text_seed,
  text_field = "chapter_text",
  docid_field = "document_id"
)


# Tokenización robusta: normalización + control de contracciones
corpus_token_seed <- corpus_poli_seed %>%
  tokens(
    remove_punct   = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url     = TRUE
  ) %>%
  tokens_tolower() %>%
  # Paso clave: convertir contracciones a formas sin apóstrofe
  tokens_replace(
    pattern = c("don’t", "doesn’t", "won’t", "can’t", "isn’t", "aren’t", "wasn’t", "weren’t",
                "i’m", "you’re", "he’s", "she’s", "it’s", "we’re", "they’re",
                "i’ve", "you’ve", "we’ve", "they’ve",
                "i’d", "you’d", "he’d", "she’d", "we’d", "they’d",
                "that’s", "there’s", "what’s", "who’s", "let’s"),
    replacement = c("dont", "doesnt", "wont", "cant", "isnt", "arent", "wasnt", "werent",
                    "im", "youre", "hes", "shes", "its", "were", "theyre",
                    "ive", "youve", "weve", "theyve",
                    "id", "youd", "hed", "shed", "wed", "theyd",
                    "thats", "theres", "whats", "whos", "lets")
  ) %>%
  # Eliminar stopwords
  tokens_remove(stop_all) %>%
  tokens_remove("'.*", valuetype = "regex")

corpus_token_seed <- corpus_token_seed %>%
  tokens_replace(
    pattern = "’s$",  # apóstrofe tipográfico
    replacement = "",
    valuetype = "regex"
  ) %>%
  tokens_replace(
    pattern = "'s$",  # apóstrofe recto
    replacement = "",
    valuetype = "regex"
  ) %>%
  tokens_remove("^s$", valuetype = "regex")  

dfm_clean_seed <- corpus_token_seed %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5, max_docfreq = 0.60 * ndoc(.)) %>%
  dfm_subset(ntoken(.) > 100)


set.seed(1234)
mod_seeded <- textmodel_seededlda(
  dfm_clean_seed,
  dictionary = dict_tmi,
  residual   = TRUE
)

phi <- as.matrix(mod_seeded$phi)

top_terms_seeded <- t(
  apply(phi, 1, function(x)
    names(sort(x, decreasing = TRUE))[1:8]
  )
)

top_terms_cols <- as.data.frame(
  t(top_terms_seeded),
  stringsAsFactors = FALSE
)

top_terms_cols$term_rank <- seq_len(nrow(top_terms_cols))
top_terms_cols <- top_terms_cols[, c("term_rank", setdiff(names(top_terms_cols), "term_rank"))]
colnames(top_terms_cols)[1] <- "term_rank"

knitr::kable(
  top_terms_cols,
  caption = "Términos principales por tópico - Seeded LDA"
)
