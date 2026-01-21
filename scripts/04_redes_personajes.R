# scripts/04_redes_personajes.R
library(tidyverse)
library(igraph)
library(stringr)
library(purrr)

# --- Cargar corpus procesado ---
books_with_chapters_shatterme_empyrean <- readRDS("data/processed/books_shatterme_empyrean.rds")
books_with_chapters_shadowhunters <- readRDS("data/processed/books_shadowhunters.rds")

books_with_chapters_empyrean <- 
  books_with_chapters_shatterme_empyrean %>% 
  filter(saga == "Empyrean")
saveRDS(books_with_chapters_empyrean, "data/processed/books_with_chapters_empyrean.rds")



books_with_chapters_shadowhunters <- 
  books_with_chapters_shadowhunters %>% 
  mutate(saga = "Shadowhunters")

capitulos <- bind_rows(
  books_with_chapters_shadowhunters,
  books_with_chapters_empyrean
)

# --- Listas de personajes por saga ---
personajes_empyrean <- c(
  "violet", "xaden", "tairn", "liessa", "dain", "andarna", "mira", "lilith", "rhiannon", "ridoc",
  "sawyer", "imogen","bodhi", "garrick","liam", "brennan"
)

personajes_shadowhunters <- c(
  "clary", "jace", "simon", "alec", "isabelle", "magnus", "valentine", "sebastian",
  "luke", "jocelyn", "maia", "raphael", "jordan",
  "max", "maryse", "robert", "hodge",
  "camille", "maureen","imogen","amatis"
)

# Normalizar a minúsculas
personajes_empyrean <- tolower(personajes_empyrean)
personajes_shadowhunters <- tolower(personajes_shadowhunters)

# --- Función para extraer personajes por capítulo ---
extraer_personajes_por_capitulo <- function(texto, lista_personajes) {
  palabras <- str_split(tolower(texto), "\\s+")[[1]]
  intersect(lista_personajes, palabras)
}

# --- Aplicar extracción por saga ---
coaparicion_empyrean <- capitulos %>%
  filter(saga == "Empyrean") %>%
  mutate(
    texto_limpio = str_to_lower(str_squish(text)),
    personajes_encontrados = map(texto_limpio, ~ extraer_personajes_por_capitulo(.x, personajes_empyrean))
  ) %>%
  filter(lengths(personajes_encontrados) >= 2) %>%
  select(saga, book, chapter_num, personajes_encontrados)

coaparicion_shadowhunters <- capitulos %>%
  filter(saga == "Shadowhunters") %>%
  mutate(
    texto_limpio = str_to_lower(str_squish(text)),
    personajes_encontrados = map(texto_limpio, ~ extraer_personajes_por_capitulo(.x, personajes_shadowhunters))
  ) %>%
  filter(lengths(personajes_encontrados) >= 2) %>%
  select(saga, book, chapter_num, personajes_encontrados)

# --- Construir red por saga ---
construir_red_saga <- function(df_saga) {
  pares <- df_saga %>%
    unnest(personajes_encontrados) %>%
    group_by(chapter_num) %>%
    summarise(personajes = list(personajes_encontrados), .groups = "drop") %>%
    mutate(
      pares = map(personajes, ~ if (length(.x) > 1) combn(.x, 2, simplify = FALSE) else list())
    ) %>%
    unnest(pares) %>%
    filter(lengths(pares) == 2) %>%
    mutate(
      nodo1 = map_chr(pares, ~ .x[1]),
      nodo2 = map_chr(pares, ~ .x[2])
    ) %>%
    count(nodo1, nodo2, sort = TRUE)
  
  grafo <- graph_from_data_frame(pares, directed = FALSE)
  return(grafo)
}

# --- Aplicar a cada saga ---
red_empyrean <- construir_red_saga(coaparicion_empyrean)
red_shadowhunters <- construir_red_saga(coaparicion_shadowhunters)

redes_por_saga <- list(
  Empyrean = list(saga = "Empyrean", grafo = red_empyrean),
  Shadowhunters = list(saga = "Shadowhunters", grafo = red_shadowhunters)
)

# --- Calcular métricas de red ---
metricas <- tibble(
  saga = names(redes_por_saga),
  densidad = map_dbl(redes_por_saga, ~ edge_density(.x$grafo)),
  nodos = map_dbl(redes_por_saga, ~ vcount(.x$grafo)),
  aristas = map_dbl(redes_por_saga, ~ ecount(.x$grafo)),
  centralidad_max = map_dbl(redes_por_saga, ~ max(degree(.x$grafo, mode = "all")))
)

# Mostrar métricas
print(metricas)

# --- Tabla formateada para informe ---
metricas_final <- metricas %>%
  mutate(
    densidad = round(densidad, 3),
    centralidad_max = round(centralidad_max, 1)
  ) %>%
  select(Saga = saga, Personajes = nodos, Conexiones = aristas, Centralidad_máx = centralidad_max)

knitr::kable(
  metricas_final,
  caption = "Comparación de redes narrativas: Shadowhunters vs Empyrean",
  align = c("l", "r", "r", "r")
)

# --- Guardar resultados ---ç
write_csv(metricas, "data/processed/metricas_redes.csv")
saveRDS(redes_por_saga, "data/processed/redes_personajes.rds")

# --- Gráfico 1: complejidad narrativa ---
metricas_long <- metricas %>%
  pivot_longer(cols = c(nodos, aristas), names_to = "métrica", values_to = "valor") %>%
  mutate(métrica = recode(métrica, nodos = "Personajes únicos", aristas = "Conexiones narrativas"))

g1 <- ggplot(metricas_long, aes(x = saga, y = valor, fill = métrica)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#4A90E2", "#FF8C00")) +
  labs(
    title = "Complejidad narrativa: Shadowhunters vs Empyrean",
    x = NULL,
    y = "Cantidad",
    fill = "Métrica"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave("output/graficos/complejidad_narrativa.png", g1, dpi = 300, width = 8, height = 6)

# --- Gráfico 2: centralidad en Empyrean ---
g_emp <- redes_por_saga[["Empyrean"]]$grafo
cent_emp <- tibble(
  personaje = names(V(g_emp)),
  centralidad = degree(g_emp)
) %>%
  arrange(desc(centralidad)) %>%
  mutate(personaje = factor(personaje, levels = personaje))

g2 <- ggplot(cent_emp, aes(x = personaje, y = centralidad)) +
  geom_col(fill = "#4A90E2", width = 0.7) +
  coord_flip() +
  labs(
    title = "Centralidad de personajes en Empyrean",
    x = NULL,
    y = "Grado (nº de conexiones)"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave("output/graficos/centralidad_empyrean.png", g2, dpi = 300, width = 8, height = 6)

# --- Gráfico 3: centralidad en Shadowhunters ---
g_sha <- redes_por_saga[["Shadowhunters"]]$grafo
cent_sha <- tibble(
  personaje = names(V(g_sha)),
  centralidad = degree(g_sha)
) %>%
  arrange(desc(centralidad)) %>%
  mutate(personaje = factor(personaje, levels = personaje))

g3 <- ggplot(cent_sha, aes(x = personaje, y = centralidad)) +
  geom_col(fill = "#FF8C00", width = 0.7) +
  coord_flip() +
  labs(
    title = "Centralidad de personajes en Shadowhunters (The Mortal Instruments)",
    x = NULL,
    y = "Grado (nº de conexiones)"
  ) +
  theme_economist() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave("output/graficos/centralidad_shadowhunters.png", g3, dpi = 300, width = 10, height = 6)
