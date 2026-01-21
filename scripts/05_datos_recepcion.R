# scripts/05_datos_recepcion.R
# Procesamiento de ratings de Goodreads para sagas clave

library(tidyverse)
library(readxl)

# --- 1. Cargar archivo Excel ---
# Asegúrate de que el archivo esté en: data/external/goodreads_rating.xlsx
goodreads_raw <- read_excel("data/external/goodreads_rating.xlsx")

# --- 2. Verificar estructura esperada ---
# El archivo debe tener al menos estas columnas:
# - "Saga" (nombre de la saga)
# - "rating_goodreads" (puntuación promedio)

# Si tu archivo tiene otro nombre de columna, ajusta aquí:
goodreads_clean <- goodreads_raw %>%
  select(saga, rating)
  

# --- 3. Validar que las sagas clave estén presentes ---
sagas_esperadas <- c("Empyrean", "Shadowhunters")
missing_sagas <- setdiff(sagas_esperadas, goodreads_clean$saga)

if (length(missing_sagas) > 0) {
  stop("Faltan sagas en Goodreads: ", paste(missing_sagas, collapse = ", "))
}

# --- 4. Guardar resultado procesado ---

write_csv(goodreads_clean, "data/processed/goodreads_procesado.csv")

