# test-spanish-basic.R
# Tests sintéticos para rasgos de Biber en español (sin UDPipe)
#
# Los tokens se construyen manualmente en formato UDPipe (columnas upos/xpos).
# biber_es() renombra upos→pos y xpos→tag antes de llamar a parse_biber_features().
#
# Convención: cada test verifica UN rasgo con una oración mínima.
# nolint start: line_length_linter, object_name_linter

# ─── Helper: construir tibble en formato UDPipe ────────────────────────────
make_es_tokens <- function(...) {
  tbl <- tibble::tribble(...)
  if (!"feats" %in% colnames(tbl)) tbl$feats <- NA_character_
  tbl
}

run_es <- function(tokens, normalize = FALSE) {
  biber_es(tokens, measure = "none", normalize = normalize)
}

# ─── Tiempo verbal ────────────────────────────────────────────────────────

test_that("f_01_past_tense cuenta imperfecto de indicativo", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,    ~lemma,    ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "caminaba", "caminar", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Imp|VerbForm=Fin",
    "d1",    1L,           2L,        ".",        ".",       "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_01_past_tense, 1)
  expect_equal(res$f_71_preterit,  0)
})

test_that("f_71_preterit cuenta pretérito indefinido", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "llegó",  "llegar", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "d1",    1L,           2L,        ".",      ".",      "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_71_preterit,  1)
  expect_equal(res$f_01_past_tense, 0)
})

test_that("f_03_present_tense cuenta presente de indicativo", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,    ~lemma,    ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "estudia", "estudiar","VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        ".",       ".",       "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_03_present_tense, 1)
})

test_that("f_02_perfect_aspect cuenta haber + participio", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,      ~lemma,      ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "he",        "haber",     "AUX",  "AUX",  "aux",    2L,             "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "terminado", "terminar",  "VERB", "VERB", "root",   NA_integer_,    "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part",
    "d1",    1L,           3L,        ".",         ".",         "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_02_perfect_aspect, 1)
})

# ─── Pronombres personales (pro-drop: solo explícitos) ───────────────────

test_that("f_06 cuenta pronombres de primera persona explícitos", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,      ~lemma,      ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "yo",        "yo",        "PRON", "PRON", "nsubj",  2L,             "Case=Nom|Number=Sing|Person=1|PronType=Prs",
    "d1",    1L,           2L,        "creo",      "creer",     "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",         ".",         "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_06_first_person_pronouns, 1)
})

test_that("f_07 cuenta pronombres de segunda persona", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "tú",    "tú",     "PRON", "PRON", "nsubj",  2L,             "Case=Nom|Number=Sing|Person=2|PronType=Prs",
    "d1",    1L,           2L,        "sabes", "saber",  "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",      "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_07_second_person_pronouns, 1)
})

test_that("f_08 cuenta pronombres de tercera persona", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "él",    "él",     "PRON", "PRON", "nsubj",  2L,             "Case=Nom|Gender=Masc|Number=Sing|Person=3|PronType=Prs",
    "d1",    1L,           2L,        "sabe",  "saber",  "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",      "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_08_third_person_pronouns, 1)
})

# ─── Pasiva ───────────────────────────────────────────────────────────────

test_that("f_17 cuenta pasiva perifrástica sin agente", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,     ~lemma,    ~upos,  ~xpos,  ~dep_rel,   ~head_token_id, ~feats,
    "d1",    1L,           1L,        "fue",      "ser",     "AUX",  "AUX",  "aux:pass", 2L,             "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "d1",    1L,           2L,        "aprobada", "aprobar", "VERB", "VERB", "root",     NA_integer_,    "Gender=Fem|Number=Sing|Tense=Past|VerbForm=Part|Voice=Pass",
    "d1",    1L,           3L,        ".",        ".",       "PUNCT","PUNCT","punct",    2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_17_agentless_passives, 1)
  expect_equal(res$f_18_by_passives,        0)
})

test_that("f_18 cuenta pasiva perifrástica con agente (por)", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,    ~upos,  ~xpos,  ~dep_rel,   ~head_token_id, ~feats,
    "d1",    1L,           1L,        "fue",    "ser",     "AUX",  "AUX",  "aux:pass", 2L,             "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "d1",    1L,           2L,        "escrito","escribir","VERB", "VERB", "root",     NA_integer_,    "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part|Voice=Pass",
    "d1",    1L,           3L,        "por",   "por",     "ADP",  "ADP",  "case",     4L,             NA_character_,
    "d1",    1L,           4L,        "él",    "él",      "PRON", "PRON", "obl:agent",2L,             "Case=Nom|Gender=Masc|Number=Sing|Person=3|PronType=Prs",
    "d1",    1L,           5L,        ".",     ".",       "PUNCT","PUNCT","punct",    2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_18_by_passives, 1)
})

# ─── Negación ─────────────────────────────────────────────────────────────

test_that("f_66 cuenta negación sintética (nadie, nunca, nada)", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,  ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "nadie", "nadie", "PRON", "PRON", "nsubj",  2L,             "PronType=Neg",
    "d1",    1L,           2L,        "sabe",  "saber", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",     "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_66_neg_synthetic, 1)
})

test_that("f_67 cuenta negación analítica (no + verbo)", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,  ~lemma,  ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "no",    "no",    "ADV",  "ADV",  "advmod", 2L,             "Polarity=Neg",
    "d1",    1L,           2L,        "sabe",  "saber", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           3L,        ".",     ".",     "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_67_neg_analytic, 1)
})

# ─── Contracciones ────────────────────────────────────────────────────────

test_that("f_59 cuenta contracciones del y al", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "voy",  "ir",   "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "al",   "al",   "ADP",  "ADP",  "case",   3L,             NA_character_,
    "d1",    1L,           3L,        "mercado","mercado","NOUN","NOUN","obl",   1L,             NA_character_,
    "d1",    1L,           4L,        "del",  "del",  "ADP",  "ADP",  "case",   5L,             NA_character_,
    "d1",    1L,           5L,        "trabajo","trabajo","NOUN","NOUN","obl",  1L,             NA_character_,
    "d1",    1L,           6L,        ".",    ".",    "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_59_contractions, 2)
})

# ─── Estructura de salida ─────────────────────────────────────────────────

test_that("biber_es devuelve una fila por documento sin errores", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,    ~lemma,    ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "doc1",  1L,           1L,        "estudia", "estudiar","VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "doc1",  1L,           2L,        "mucho",   "mucho",   "ADV",  "ADV",  "advmod", 1L,             NA_character_,
    "doc2",  1L,           1L,        "llegó",   "llegar",  "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "doc2",  1L,           2L,        "tarde",   "tarde",   "ADV",  "ADV",  "advmod", 1L,             NA_character_
  )
  res <- run_es(tks)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_true("doc_id" %in% colnames(res))
  expect_true("f_01_past_tense" %in% colnames(res))
  expect_true("f_71_preterit"  %in% colnames(res))
  expect_true("f_03_present_tense" %in% colnames(res))
})

test_that("biber_es con normalize=TRUE devuelve valores numéricos sin NA en columnas f_", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,      ~lemma,      ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "he",        "haber",     "AUX",  "AUX",  "aux",    2L,             "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "terminado", "terminar",  "VERB", "VERB", "root",   NA_integer_,    "Gender=Masc|Number=Sing|Tense=Past|VerbForm=Part",
    "d1",    1L,           3L,        "el",        "el",        "DET",  "DET",  "det",    4L,             "Definite=Def|Gender=Masc|Number=Sing|PronType=Art",
    "d1",    1L,           4L,        "trabajo",   "trabajo",   "NOUN", "NOUN", "obj",    2L,             "Gender=Masc|Number=Sing",
    "d1",    1L,           5L,        ".",         ".",         "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks, normalize = TRUE)
  feat_cols <- grep("^f_", colnames(res), value = TRUE)
  expect_true(length(feat_cols) > 10)
  for (col in feat_cols) {
    expect_true(is.numeric(res[[col]]),
                info = paste("Columna no numérica:", col))
  }
})

# nolint end
