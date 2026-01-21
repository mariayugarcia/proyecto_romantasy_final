# Proyecto Final: Evolución del Romantasy

Análisis comparativo entre *Empyrean* (2023–2025) y sagas clásicas como *The Mortal Instruments* (2007–2014).

## 📁 Estructura

El repositorio está organizado según las buenas prácticas de ciencia de datos reproducible:

- `scripts/`: código modular y autónomo dividido en 6 scripts secuenciales (preprocesado, análisis de sentimiento, modelado temático, redes de personajes, sistema de recomendación, etc.).
- `data/external/`: archivo goodreads_rating.xlsx con ratings reales de Goodreads para 9 sagas populares.
- `data/raw/`: corpus textual original compuesto por libros de Empyrean, Shatter Me y The Mortal Instruments (solo para fines académicos).
- `data/processed/`: objetos intermedios serializados (.rds, .csv) que permiten la reproducibilidad sin recalcular todo el flujo. 
- `output/graficos/`: visualizaciones clave listas para incluir en el informe (evolución de sentimiento, redes de personajes, perfil narrativo, ratings comparativos).
- `informe_final.pdf`: documento entregable con hipótesis, metodología, resultados e interpretación narrativa.

## 📊 Resultados clave
- *Empyrean* muestra una narrativa más colectiva (densidad de red = 2.04) frente al individualismo de *Shatter Me*.
- El rating de Goodreads de *Empyrean* (4.50) supera al de *Shadowhunters* (4.18), confirmando su resonancia actual.
- Sistema de recomendación basado en perfil emocional-estructural.

## 📝 Nota sobre los datos
El análisis se realizó con copias personales obtenidas legalmente.
