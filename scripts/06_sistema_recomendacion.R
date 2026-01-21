# scripts/06_sistema_recomendacion.R
library(tidyverse)
library(readxl)
library(text2vec)
library(igraph)
library(tidytext)
library(ggthemes)

# --- 1. Cargar métricas de redes ---
redes_por_saga <- readRDS("data/processed/redes_personajes.rds")
metricas <- tibble(
  saga = names(redes_por_saga),
  densidad_red = map_dbl(redes_por_saga, ~ edge_density(.x$grafo))
)

# --- 2. Cargar ratings de Goodreads (con el nuevo formato) ---
goodreads <- read_csv("data/processed/goodreads_procesado.csv")

# --- 3. Cargar sentimiento AFINN ---
afinn_chapter <- read_csv("data/processed/afinn_chapter.csv")

afinn_chapter <- afinn_chapter %>%
  mutate(book_norm = str_to_lower(book) %>% str_replace_all(" ", "_"))

afinn_chapter <- afinn_chapter %>%
  mutate(
    saga = case_when(
      book_norm %in% c("shatter_me", "unravel_me", "ignite_me") ~ "Shatter Me",
      book_norm %in% c("fourth_wing", "iron_flame", "onyx_storm") ~ "Empyrean",
      TRUE ~ "Shadowhunters"
    )
  )


# --- 4. Calcular rating promedio por saga ---
goodreads_promedio <- goodreads %>%
  group_by(saga) %>%
  summarise(
    rating_goodreads = mean(rating, na.rm = TRUE),
    .groups = "drop"
  )

# --- 5. Sentimiento AFINN para Shadowhunters ---
sent_sh <- afinn_chapter %>%
  filter(saga == "Shadowhunters") %>%
  summarise(sentimiento_pos = mean(afinn_mean, na.rm = TRUE)) %>%
  pull(sentimiento_pos)

# --- 6. Sentimiento AFINN para Empyrean ---
books_with_chapters_empyrean <- readRDS("data/processed/books_with_chapters_empyrean.rds")

chapters_empyrean <- books_with_chapters_empyrean %>%
  filter(chapter_num > 0) %>%
  group_by(book, chapter_num) %>%
  summarise(
    text_full = paste(text, collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(
    text_clean = str_to_lower(text_full) %>%
      str_replace_all("[^a-z\\s']", " ") %>%
      str_squish()
  ) %>%
  filter(nchar(text_clean) > 50)

tidy_empyrean <- chapters_empyrean %>%
  unnest_tokens(word, text_clean) %>%
  inner_join(get_sentiments("afinn"), by = "word")

sent_em <- tidy_empyrean %>%
  group_by(book, chapter_num) %>%
  summarise(afinn_mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
  summarise(sentimiento_pos = mean(afinn_mean, na.rm = TRUE)) %>%
  pull(sentimiento_pos)

# --- 7. Construir perfil técnico (Empyrean vs Shadowhunters) ---
perfil_tecnico <- tibble(
  saga = c("Empyrean", "Shadowhunters"),
  sentimiento_pos = c(sent_em, sent_sh),
  densidad_red = c(
    metricas$densidad_red[metricas$saga == "Empyrean"],
    metricas$densidad_red[metricas$saga == "Shadowhunters"]
  ),
  rating_goodreads = c(
    goodreads_promedio$rating_goodreads[goodreads_promedio$saga == "Empyrean"],
    goodreads_promedio$rating_goodreads[goodreads_promedio$saga == "Shadowhunters"]
  )
)

# Mostrar perfil
print(perfil_tecnico)

ratings_dataset <- goodreads %>%
  group_by(saga) %>%
  summarise(
    n_libros = n(),
    rating_promedio = mean(rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(rating_promedio))

tabla_ratings <- knitr::kable(
  ratings_dataset,
  caption = "Ratings promedio por saga (Goodreads)",
  align = c("l", "r", "r")
)

ratings_dataset <- goodreads %>%
  group_by(saga) %>%
  summarise(
    n_libros = n(),
    rating_promedio = mean(rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(rating_promedio))

# Tabla con nombres de columna personalizados
tabla_ratings <- knitr::kable(
  ratings_dataset,
  caption = "Ratings promedio por saga (Goodreads)",
  col.names = c("Saga", "Nº de libros", "Rating promedio"),
  align = c("l", "r", "r")
)

print(tabla_ratings)

# --- 8. Sistema de recomendación ---

# --- 8. Sistema de recomendación ---
ratings_saga <- goodreads %>%
  group_by(saga) %>%
  summarise(rating_goodreads = mean(rating, na.rm = TRUE), .groups = "drop")

# Crear todas las combinaciones de sagas
recomendaciones <- ratings_saga %>%
  mutate(key = 1) %>%
  left_join(ratings_saga %>% mutate(key = 1), by = "key", suffix = c("_actual", "_candidata")) %>%
  filter(saga_actual != saga_candidata) %>%
  mutate(
    diferencia_rating = abs(rating_goodreads_actual - rating_goodreads_candidata),
    similitud = 1 - diferencia_rating / max(diferencia_rating, na.rm = TRUE)
  ) %>%
  select(saga_actual, saga_candidata, similitud, diferencia_rating) %>%
  arrange(desc(similitud))

# Top 5 para Empyrean
top_empyrean <- recomendaciones %>%
  filter(saga_actual == "Empyrean") %>%
  head(5)

# --- Tabla para informe ---
tabla_recomendaciones <- knitr::kable(
  top_empyrean,
  caption = "Top 5 recomendaciones para lectores de Empyrean (basado en rating de Goodreads)",
  col.names = c("Saga actual", "Saga candidata", "Similitud", "Diferencia absoluta de rating"),
  align = c("l", "l", "r", "r")
)

# --- Tabla para informe ---
tabla_recomendaciones <- knitr::kable(
  top_empyrean,
  caption = "Top 5 recomendaciones para lectores de Empyrean (basado en rating de Goodreads)",
  align = c("l", "l", "r")
)

print(tabla_recomendaciones)


# --- 3. Guardar resultados ---
dir.create("data/processed", showWarnings = FALSE)
write_csv(ratings_saga, "data/processed/ratings_saga.csv")
write_csv(recomendaciones, "data/processed/recomendaciones_rating.csv")


# Convertir a formato largo
perfil_long <- perfil_tecnico %>%
  pivot_longer(
    cols = -saga,
    names_to = "dimension",
    values_to = "valor"
  ) %>%
  mutate(
    dimension = recode(
      dimension,
      sentimiento_pos = "Sentimiento (AFINN)",
      densidad_red = "Densidad de red",
      rating_goodreads = "Rating Goodreads"
    ),
    # Ordenar las dimensiones
    dimension = factor(dimension, levels = c("Sentimiento (AFINN)", "Densidad de red", "Rating Goodreads")),
    # Asegurar que saga es factor
    saga = factor(saga, levels = c("Empyrean", "Shadowhunters"))
  )

# Crear gráfico con valores BRUTOS
g_perfil <- ggplot(perfil_long, aes(x = dimension, y = valor, fill = saga)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Empyrean" = "#4A90E2", "Shadowhunters" = "#FF8C00")) +
  labs(
    title = "Perfil narrativo comparativo: Empyrean vs Shadowhunters",
    x = NULL,
    y = "Valor bruto",
    fill = "Saga"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12)
  )

# Guardar gráfico
dir.create("output/graficos", showWarnings = FALSE)
ggsave("output/graficos/perfil_recomendacion.png", g_perfil, dpi = 300, width = 10, height = 6)



# --- 9. Gráfico 2: ratings de todas las sagas del dataset ---
n_sagas <- nrow(ratings_dataset)
colores <- brewer.pal(n = n_sagas, name = "Set1") # Set3 tiene colores suficientes para hasta 12 sagas

# Crear gráfico
g_ratings <- ggplot(ratings_dataset, aes(x = saga, y = rating_promedio, fill = saga)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = colores) +
  labs(
    title = "Ratings de Goodreads por saga",
    x = NULL,
    y = "Rating promedio",
    fill = "Saga"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.position = "none"
  )

# Mostrar gráfico
print(g_ratings)

ggsave("output/graficos/ratings_saga.png", g_ratings, dpi = 300, width = 10, height = 6)

# --- 13. Guardar tabla de ratings ---
write_csv(ratings_dataset, "data/processed/ratings_dataset.csv")






