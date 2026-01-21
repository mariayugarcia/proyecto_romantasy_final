# scripts/01b_preprocesado_shadowhunters.R
library(tidyverse)
library(tidytext)

# --- Mismas funciones (o source("scripts/utils.R")) ---
clean_book_lines <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  ack_pos <- grep("^\\s*acknowledgements\\s*$", tolower(lines))
  if (length(ack_pos) > 0) lines <- lines[1:(ack_pos[1] - 1)]
  return(lines)
}

add_chapter_numbers <- function(text_lines) {
  cleaned_lines <- str_trim(text_lines)
  lower_lines <- tolower(cleaned_lines)
  
  is_explicit <- grepl("^prologue$", lower_lines) |
    grepl("^epilogue", lower_lines) |
    grepl("^chapter\\s+[ivxlcdm0-9]", lower_lines)
  
  is_numbered <- grepl("^\\d{1,2}$", cleaned_lines)
  
  chapter_ids <- integer(length(cleaned_lines))
  current_chapter <- 0
  
  for (i in seq_along(cleaned_lines)) {
    if (is_explicit[i] || (is_numbered[i] && !grepl("\\w", cleaned_lines[i]))) {
      current_chapter <- current_chapter + 1
    }
    chapter_ids[i] <- current_chapter
  }
  
  tibble(text = text_lines, chapter_num = chapter_ids)
}

# --- Procesar Shadowhunters ---
file_paths <- list.files("data/raw/shadowhunters/", "\\.txt$", full.names = TRUE)
book_names <- set_names(
  file_paths,
  map_chr(basename(file_paths), ~ {
    name <- str_remove(.x, "\\.txt$")
    str_trim(str_remove(name, " - Cassandra Clare"))
  })
)

books_with_chapters <- map2_df(
  book_names,
  names(book_names),
  ~ {
    lines <- clean_book_lines(.x)
    df <- add_chapter_numbers(lines)
    df$book <- .y
    df
  }
) %>%
  mutate(saga = "Shadowhunters")

# --- Guardar ---
saveRDS(books_with_chapters, "data/processed/books_shadowhunters.rds")
