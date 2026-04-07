# pseudobibeR.es

Paquete compañero de `pseudobibeR` que apunta a recursos de análisis para textos en **español** y extracción de rasgos lingüísticos tipo Biber.
El código está en desarrollo activo; la cobertura de rasgos sigue el mapeo A–P de Biber (1985) documentado en los scripts y en los archivos YAML bajo `data-raw/`.[cite:47]

El paquete **pseudobibeR.es** calcula los rasgos lexicogrammaticales y funcionales descritos por Biber (1985) y ampliamente usados para estudiar tipos de texto, registro y género.
Comparte el mismo catálogo de 67 rasgos que la versión inglesa, pero esta variante se apoya en recursos de análisis morfosintáctico para español (UDPipe, y potencialmente spaCy) y en heurísticas específicas del idioma implementadas en los bloques `*_es`.[cite:36][cite:37][cite:38][cite:39][cite:40][cite:41][cite:42][cite:43][cite:46][cite:48]

---

## Descripción general

Este paquete **no** hace el etiquetado gramatical por sí mismo.
En lugar de eso, reutiliza etiquetadores y analizadores de dependencias existentes —por ahora, especialmente [udpipe](https://bnosac.github.io/udpipe/en/)— para extraer y agregar patrones lingüísticos.
Para español nos apoyamos fuertemente en las características morfológicas del UD (Universal Dependencies), por lo que es crucial pedirlas al momento del parseo (con `tagger = "default"`, `parser = "default"` en `udpipe_annotate()`).[cite:47]

La calidad de los rasgos extraídos depende directamente de la calidad del etiquetado y del análisis de dependencias.
Textos con ortografía muy irregular, puntuación no estándar o lenguaje muy especializado pueden producir salidas menos fiables salvo que el modelo UD se haya ajustado a ese dominio.

---

## Instalación

Instala la versión de desarrollo desde GitHub (tu fork en `educia`):

```r
# install.packages("devtools")
devtools::install_github("educia/pseudobibeR.esp")  # paquete pseudobibeR.es
```

> Nota: el repositorio conserva el nombre original `pseudobibeR.esp`, pero el paquete en `DESCRIPTION` se llama `pseudobibeR.es`.[cite:49]

Si en el futuro generas un tarball de versión (`pseudobibeR.es_<version>.tar.gz`), también podrás instalarlo con:

```r
install.packages("/ruta/a/pseudobibeR.es_<version>.tar.gz", repos = NULL, type = "source")
```

---

## Inicio rápido (español, con UDPipe)

La función principal para español es `biber_es()`, que toma la salida de `udpipe::udpipe_annotate()` con un modelo UD de español (por ejemplo, `spanish-gsd`) y devuelve un `data.frame` con los 67 rasgos de Biber para cada documento.[cite:48][cite:51]

### Paso 1: cargar un modelo UD de español

```r
library(udpipe)
# Descarga del modelo (solo una vez)
# udpipe_download_model(language = "spanish-gsd")

# Cargar el modelo ya descargado
audmodel <- udpipe_load_model("spanish-gsd-ud-2.5-191206.udpipe")
```

### Paso 2: texto(s) de prueba

```r
textos <- c(
  doc1 = "Este es un texto de prueba. Fue escrito por un estudiante.",
  doc2 = "Hay muchos estudios que han analizado la variación de registros."
)
```

### Paso 3: parsear con UDPipe

```r
parsed <- udpipe_annotate(
  object = audmodel,
  x      = textos,
  tagger = "default",
  parser = "default"
)
```

### Paso 4: extraer rasgos de Biber para español

```r
library(pseudobibeR.es)

feats_es <- biber_es(parsed, measure = "none", normalize = FALSE)

feats_es[
  , c("doc_id",
      "f_01_past_tense",          # verbos en pasado
      "f_03_present_tense",       # verbos en presente
      "f_17_agentless_passives",  # pasivas sin agente
      "f_18_by_passives",         # pasivas con agente (por + agente)
      "f_20_existential_there")   # construcciones existenciales con "haber"
]
```

En este ejemplo, se espera que:

- En `doc1` la oración "Fue escrito por un estudiante" incremente `f_18_by_passives` (pasiva con agente), gracias al bloque `block_passive_voice_es()` y a la detección del agente introducido por "por".[cite:46][cite:48]
- En `doc2` la oración "Hay muchos estudios..." incremente `f_20_existential_there`, ya que se trata de una construcción existencial de `haber`.[cite:46]

---

## Uso general (francés vs español)

En este repositorio conviven dos APIs conceptuales:[cite:47][cite:48][cite:51]

- `biber()`  → interfaz original de pseudobibeR, pensada para francés, con métodos S3 para objetos `spacyr_parsed` y `udpipe_connlu` y heurísticas `_fr`.
- `biber_es()` → wrapper específico para español y entrada de `udpipe::udpipe_annotate()`, que llama internamente a `parse_biber_features(..., language = "es")` y usa todos los bloques `_es` (tiempos, subordinación, pasivas, negación, modales, etc.).[cite:48][cite:51]

Para tu proyecto en español, la recomendación es usar directamente `biber_es()` sobre parseos UD de `spanish-gsd`.

---

## Dependencias mínimas para español

### Núcleo

- R (>= 3.5.0)
- dplyr, purrr, quanteda, quanteda.textstats, rlang, stringr, tibble, magrittr (ya declarados en `DESCRIPTION`).[cite:49]

### Análisis morfosintáctico

- [udpipe](https://bnosac.github.io/udpipe/en/) (en `Suggests:`) + un modelo UD para español, por ejemplo `spanish-gsd-ud-2.5-191206.udpipe`.[cite:49]
- Opcionalmente, en el futuro podrías añadir soporte explícito para `spacyr` + `es_core_news_sm` siguiendo el mismo patrón que en francés.

---

## Arquitectura de rasgos

El extractor español sigue la misma clasificación A–P de Biber que la versión francesa, pero implementada con heurísticas específicas de español en los bloques `*_es`:

| Categoría | Features | Descripción breve |
|----------|----------|-------------------|
| **A. Tiempos y aspecto** | f_01–f_03 | Pasado, aspecto perfecto, presente |
| **B. Adverbios de lugar y tiempo** | f_04–f_05 | Adverbios espaciales y temporales |
| **C. Pronombres y pro‑verbos** | f_06–f_12 | 1ª, 2ª, 3ª persona, demostrativos, indefinidos, pro‑verbo *hacer* |
| **D. Interrogativas** | f_13 | Preguntas con palabras *wh* (qué, quién, cuándo, etc.) |
| **E. Formas nominales** | f_14–f_16 | Nominalizaciones, gerundios, otros nombres |
| **F. Pasivas** | f_17–f_18 | Pasivas sin agente y con agente |
| **G. Formas estativas** | f_19–f_20 | *ser/estar* como verbo principal, existenciales con *haber* |
| **H. Subordinación** | f_21–f_38 | Complementantes (*que*, *si*), relativas, subordinadas adverbiales |
| **I. Sintagmas preposicionales, adjetivos, adverbios** | f_39–f_42 | PPs, adjetivos atributivos/predicativos, adverbios |
| **J. Especificidad léxica** | f_43–f_44 | TTR, longitud media de palabra |
| **K. Clases léxicas** | f_45–f_51 | Conjuntos, atenuadores, hedges, amplificadores, enfáticos, demostrativos |
| **L. Modales** | f_52–f_54 | Posibilidad (*poder*), necesidad (*deber*, *tener que*), predictivos (*ir a* + inf.) |
| **M. Verbos especializados** | f_55–f_58 | Verbos públicos, privados, suasivos, de apariencia |
| **N. Formas reducidas y estructuras marcadas** | f_59–f_63 | Contracciones (en español marginales), omisión de *que*, escisiones 
| **O. Coordinación** | f_64–f_65 | Coordinación frasal y clausal |
| **P. Negación** | f_66–f_67 | Negación sintética (*ningún, nadie, nada*) y analítica (*no, nunca, jamás, tampoco*, etc.) |

Los detalles de implementación viven en los archivos:

- `R/features_tense_pronouns.R` → tiempos, aspecto y pronombres (`*_es`).[cite:36][cite:37]
- `R/features_subordination.R` → subordinación y relativas (`block_clause_embedding_es()`, `block_participial_clauses_es()`, `block_relatives_es()`).[cite:41]
- `R/features_modals_verbs.R` → modales, clases verbales, adjetivos/adverbios (`*_es`).[cite:42]
- `R/features_coordination_negation.R` → coordinación, negación y membresía léxica (`*_es`).[cite:40][cite:43]
- `R/features_passive.R` → pasivas y formas estativas (`block_passive_voice_es()`).[cite:46]
- `R/parse_functions.R` → orquestador `parse_biber_features(..., language = c("fr","es"))` que llama a los bloques españoles cuando `language = "es"`.[cite:48]

---

## Datos y diccionarios

Los diccionarios y listas de palabras para español viven en `data-raw/dict.yaml` y `data-raw/word_lists.yaml` y se empaquetan en los objetos `dict` y `word_lists` mediante `data-raw/build_french_dictionaries.R` (que en tu fork adapta recursos al español).[cite:33][cite:35]

Puedes inspeccionar los recursos así:

```r
library(pseudobibeR.es)

data(dict)
data(word_lists)

names(dict)
word_lists$nominalization_suffixes   # sufijos de nominalización en español
```

Para regenerar los diccionarios tras editar los YAML:

```r
source("data-raw/build_french_dictionaries.R")  # en tu fork genera dict/word_lists para es
```

---

## Nombres heredados del paquete francés

Este repositorio parte del código original de `pseudobibeR.fr`.
Por compatibilidad histórica, algunos archivos y scripts mantienen la palabra `french` en el nombre (por ejemplo, `data-raw/build_french_dictionaries.R`, `data-raw/french_examples.yaml`, tests `test-french-*.R`).[cite:52][cite:57]

En **este fork**, esos recursos se usan para **español**:

- los YAML bajo `data-raw/` recogen diccionarios, listas de palabras y ejemplos pensados para textos en español,[cite:57]
- los scripts `build_french_*` regeneran los objetos `dict`, `word_lists` y ejemplos españoles que utiliza el extractor.

Si exploras el código fuente, ten en cuenta que cualquier referencia a `french` en rutas de archivos o nombres de scripts es un vestigio del proyecto original y **no implica que el paquete actual esté orientado al francés**.
La API recomendada para español es siempre `biber_es()` sobre modelos UD de español.

---

## Cita sugerida

Al usar `pseudobibeR.es` en tu investigación, cita al menos:

> Biber, D. (1985). Investigating macroscopic textual variation through multifeature/multidimensional analyses. *Linguistics*, 23(2), 337–360. https://doi.org/10.1515/ling.1985.23.2.337

Y, si lo deseas, una referencia a tu paquete/fork, por ejemplo en el manuscrito o en un apéndice metodológico.

---

## Contribuciones

Como este paquete está ligado a tu proyecto de investigación, las contribuciones seguramente serán internas (tuyas o de colaboradorxs), pero la estructura permite:

- ampliar diccionarios y listas léxicas,
- ajustar heurísticas de rasgos concretos,
- añadir tests que aseguren estabilidad frente a cambios en modelos UD.

Si en algún momento decides abrirlo, bastaría con actualizar este README con instrucciones de issues/pull requests.
