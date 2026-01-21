# scripts/01a_preprocesado_corpuses_focales.R
library(tidyverse)
library(tidytext)


# Cargar libros desde la carpeta "libros/"
book_paths <- c(
  "shatter_me"  = "data/raw/shatter_me/Shatter Me - Tahereh Mafi.txt",
  "unravel_me"  = "data/raw/shatter_me/Unravel Me - Tahereh Mafi.txt",
  "ignite_me"   = "data/raw/shatter_me/Ignite Me - Tahereh Mafi.txt",
  "fourth_wing" = "data/raw/empyrean/Fourth Wing - Rebecca Yarros.txt",
  "iron_flame"  = "data/raw/empyrean/Iron Flame - Rebecca Yarros.txt",
  "onyx_storm"  = "data/raw/empyrean/Onyx Storm (The Empyrean Book 3 - Rebecca Yarros.txt"
)



clean_book_lines <- function(book_paths) {
  lines <- readLines(book_paths, warn = FALSE, encoding = "UTF-8")
  # Buscar la posición donde empieza "acknowledgements" (en minúscula para evitar errores de mayúsculas)
  ack_pos <- grep("^\\s*acknowledgements\\s*$", tolower(lines))
  
  if (length(ack_pos) > 0) {
    # Mantener solo las líneas antes de la primera aparición de "acknowledgements"
    lines <- lines[1:(ack_pos[1] - 1)]
  }
  
  return(lines)
}

add_chapter_numbers <- function(text_lines) {
  cleaned_lines <- stringr::str_trim(text_lines)
  lower_lines <- tolower(cleaned_lines)
  
  number_words <- c("one", "two", "three", "four", "five", "six",
                    "seven", "eight", "nine", "ten", "eleven", "twelve",
                    "thirteen", "fourteen", "fifteen", "sixteen",
                    "seventeen", "eighteen", "nineteen", "twenty",
                    "twenty-one", "twenty two", "twenty three", "twenty four",
                    "twenty five", "twenty six", "twenty seven", "twenty eight",
                    "twenty nine", "thirty", "thirty one", "thirty two",
                    "thirty three", "thirty four", "thirty five", "thirty six",
                    "thirty seven", "thirty eight", "thirty nine", "forty",
                    "forty one", "forty two", "forty three", "forty four",
                    "forty five", "forty six", "forty seven", "forty eight",
                    "forty nine", "fifty")
  
  
  # Crear patrón de palabras numéricas
  number_words_pattern <- paste0("^(", paste(number_words, collapse = "|"), ")$")
  
  # Detectar línea de capítulo
  is_chapter_start <- grepl("^chapter", lower_lines) |
    grepl("^capítulo", lower_lines) |
    grepl("^[ivxlcdm]+$", lower_lines) |
    grepl("^\\d+$", cleaned_lines) |
    grepl(number_words_pattern, lower_lines)
  
  chapter_ids <- integer(length(cleaned_lines))
  current_chapter <- 0
  
  for (i in seq_along(cleaned_lines)) {
    if (is_chapter_start[i]) {
      current_chapter <- current_chapter + 1
    }
    chapter_ids[i] <- current_chapter
  }
  
  tibble(
    text = text_lines,
    chapter_num = chapter_ids
  )
}

# Procesar todos los libros
books_with_chapters <- map2_df(
  book_paths,
  names(book_paths),
  ~ {
    lines <- clean_book_lines(.x)
    df <- add_chapter_numbers(lines)
    df$book <- .y
    df
  }
)

# Agregar columna saga
books_with_chapters_shatterme_empyrean <- books_with_chapters %>%
  mutate(
    saga = case_when(
      book %in% c("shatter_me", "unravel_me", "ignite_me") ~ "Shatter Me",
      book %in% c("fourth_wing", "iron_flame", "onyx_storm") ~ "Empyrean",
      TRUE ~ NA_character_
    )
  )

saveRDS(books_with_chapters_shatterme_empyrean, "data/processed/books_with_chapters_shatterme_empyrean.rds")




# Cargar libros del dataset
file_paths_dataset <- list.files(
  path = "data/raw/libros/dataset/",
  pattern = "\\.txt$",
  full.names = TRUE
)

clean_book_title <- function(filename) {
  name <- str_remove(filename, "\\.txt$")
  name <- str_remove(name, " - [A-Z][a-z]+( [A-Z][a-z]+)*$")
  name <- str_trim(str_remove(name, "-$"))
  return(name)
}

book_names_dataset <- set_names(
  file_paths_dataset,
  clean_book_title(basename(file_paths_dataset))
)

# Procesar dataset: leer líneas y asignar libro
books_with_chapters_dataset <- map2_df(
  file_paths_dataset,
  names(book_names_dataset),
  ~ {
    lines <- clean_book_lines(.x)
    df <- add_chapter_numbers(lines)
    df$book <- .y
    df
  }
)

# --- AÑADIR SAGA ANTES DE TOKENIZAR ---
book_to_saga <- c(
  "A Court of Frost and Starlight - Sarah J. Maas" = "A Court of Thorns and Roses",
  "A Court of Mist and Fury - Sarah J. Maas" = "A Court of Thorns and Roses",
  "A Court of Thorns and Roses - Sarah J. Maas" = "A Court of Thorns and Roses",
  "A Court of Wings and Ruin - Sarah J. Maas" = "A Court of Thorns and Roses",
  
  "Throne of Glass - Maas, Sarah J_" = "Throne of Glass",
  "Heir of Fire - Sarah J. Maas" = "Throne of Glass",
  "Queen of Shadows - Sarah J. Maas" = "Throne of Glass",
  "Empire of Storms (Throne of Gla - Sarah J. Maas" = "Throne of Glass",
  "Tower of Dawn - Sarah J. Maas" = "Throne of Glass",
  "Kingdom of Ash - Sarah J. Maas" = "Throne of Glass",
  
  "Divergent" = "Divergent",
  "Insurgent" = "Divergent",
  "Allegiant" = "Divergent",
  
  "The Maze Runner" = "The Maze Runner",
  "The Scorch Trials" = "The Maze Runner",
  "The Death Cure" = "The Maze Runner",
  "The Kill Order" = "The Maze Runner",
  "The Fever Code" = "The Maze Runner",
  
  "Vampire Academy" = "Vampire Academy",
  "Frostbite" = "Vampire Academy",
  "Shadow Kiss" = "Vampire Academy",
  "Blood Promise" = "Vampire Academy",
  "Spirit Bound" = "Vampire Academy",
  "Last Sacrifice_ A Vampire Acade" = "Vampire Academy",
  
  "The Mortal Instruments 01 - Cit" = "The Mortal Instruments",
  "The Mortal Instruments 02 - Cit" = "The Mortal Instruments",
  "The Mortal Instruments 03 - Cit" = "The Mortal Instruments",
  "The Mortal Instruments 04 - Cit" = "The Mortal Instruments",
  "The Mortal Instruments 05 - Cit" = "The Mortal Instruments",
  "The Mortal Instruments 06 - Cit" = "The Mortal Instruments",
  
  "Clockwork Prince" = "The Infernal Devices",
  "Clockwork Princess" = "The Infernal Devices",
  
  "The Lightning Thief" = "Percy Jackson & the Olympians",
  "The Sea of Monsters" = "Percy Jackson & the Olympians",
  "The Titan's Curse" = "Percy Jackson & the Olympians",
  "The Battle of the Labyrinth" = "Percy Jackson & the Olympians",
  "The Last Olympian" = "Percy Jackson & the Olympians",
  
  "Catching Fire (The Hunger Games" = "The Hunger Games",
  "Mockingjay (The Final Book of t" = "The Hunger Games",
  
  "Fallen" = "Fallen",
  "Torment" = "Fallen",
  "Passion" = "Fallen",
  "Fallen Series 04 - Rapture - Kate, Lauren" = "Fallen",
  "Unforgiven" = "Fallen",
  
  "Hush, Hush" = "Hush, Hush",
  "Hush, Hush Bk 2 - Crescendo" = "Hush, Hush",
  "Finale" = "Hush, Hush",
  
  "Shatter Me" = "Shatter Me",
  "Unravel Me" = "Shatter Me",
  "Ignite Me" = "Shatter Me"
)

# Verificar libros faltantes
missing_books <- setdiff(unique(books_with_chapters_dataset$book), names(book_to_saga))
if (length(missing_books) > 0) {
  warning("Los siguientes libros no tienen saga asignada: ", 
          paste(missing_books, collapse = ", "))
}

# Añadir columna 'saga'
books_with_chapters_dataset <- books_with_chapters_dataset %>%
  mutate(saga = book_to_saga[book])

# tokenización
tidy_books_dataset <- books_with_chapters_dataset %>%
  filter(nchar(trimws(text)) > 0) %>%
  group_by(book, saga, chapter_num) %>%
  unnest_tokens(word, text) %>%
  ungroup()


saveRDS(books_with_chapters_dataset, "data/processed/books_with_chapters_dataset.rds")
saveRDS(tidy_books_dataset, "data/processed/tidy_books_dataset.rds")
