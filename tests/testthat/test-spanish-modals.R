# test-spanish-modals.R
# Tests para perifrasis modales españolas (f_52–f_54)
#
# Biber (1988) distingue tres tipos de modal:
#   f_52  posibilidad  (poder, caber)
#   f_53  necesidad    (deber, tener que, haber de, haber que)
#   f_54  predictivo   (futuro sintético + ir a + inf)
#
# El español no tiene modales monoléxicos como el inglés; usa perífrasis
# verbo auxiliar + infinitivo. block_modals_es() detecta:
#   (a) lema en lista + VerbForm=Inf del dependiente
#   (b) futuro sintético: Tense=Fut, VerbForm=Fin
#   (c) ir_a + inf: lemma=ir, dep_rel=aux, head tiene VerbForm=Inf
#
# nolint start: line_length_linter, object_name_linter

make_es_tokens <- function(...) {
  tbl <- tibble::tribble(...)
  if (!"feats" %in% colnames(tbl)) tbl$feats <- NA_character_
  tbl
}

run_es <- function(tokens) {
  biber_es(tokens, measure = "none", normalize = FALSE)
}

# ─── f_52  Modal de posibilidad ──────────────────────────────────────────

test_that("f_52 cuenta poder + infinitivo", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "podemos","poder",  "AUX",  "AUX",  "aux",    2L,             "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "mejorar","mejorar","VERB", "VERB", "root",   NA_integer_,    "VerbForm=Inf",
    "d1",    1L,           3L,        ".",      ".",      "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_52_modal_possibility, 1)
  expect_equal(res$f_53_modal_necessity,   0)
  expect_equal(res$f_54_modal_predictive,  0)
})

# ─── f_53  Modal de necesidad ────────────────────────────────────────────

test_that("f_53 cuenta deber + infinitivo", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "debemos","deber",  "AUX",  "AUX",  "aux",    2L,             "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "revisar","revisar","VERB", "VERB", "root",   NA_integer_,    "VerbForm=Inf",
    "d1",    1L,           3L,        ".",      ".",      "PUNCT","PUNCT","punct",  2L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_53_modal_necessity, 1)
  expect_equal(res$f_52_modal_possibility, 0)
})

test_that("f_53 cuenta hay que + infinitivo (haber_que)", {
  # "Hay que" = haber impersonal + que (mark) + infinitivo
  # UDPipe etiqueta "hay" como VERB/AUX lemma=haber; "que" como SCONJ mark;
  # el infinitivo es xcomp/ccomp de haber.
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "hay",    "haber",  "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "que",    "que",    "SCONJ","SCONJ","mark",   3L,             NA_character_,
    "d1",    1L,           3L,        "revisar","revisar","VERB", "VERB", "xcomp",  1L,             "VerbForm=Inf",
    "d1",    1L,           4L,        ".",      ".",      "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_53_modal_necessity, 1)
})

# ─── f_54  Modal predictivo ───────────────────────────────────────────────

test_that("f_54 cuenta futuro sintético (Tense=Fut)", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,       ~lemma,      ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "presentará", "presentar", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Fut|VerbForm=Fin",
    "d1",    1L,           2L,        "los",        "los",       "DET",  "DET",  "det",    3L,             "Definite=Def|Gender=Masc|Number=Plur|PronType=Art",
    "d1",    1L,           3L,        "resultados", "resultado", "NOUN", "NOUN", "obj",    1L,             "Gender=Masc|Number=Plur",
    "d1",    1L,           4L,        ".",          ".",         "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_gte(res$f_54_modal_predictive, 1)
  expect_equal(res$f_52_modal_possibility, 0)
  expect_equal(res$f_53_modal_necessity,   0)
})

test_that("f_54 cuenta ir_a + infinitivo", {
  # "vamos a analizar" = ir (AUX, dep_rel=aux) → head analizar (VerbForm=Inf)
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,    ~lemma,    ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "vamos",   "ir",      "AUX",  "AUX",  "aux",    3L,             "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "a",       "a",       "ADP",  "ADP",  "mark",   3L,             NA_character_,
    "d1",    1L,           3L,        "analizar","analizar","VERB", "VERB", "root",   NA_integer_,    "VerbForm=Inf",
    "d1",    1L,           4L,        ".",       ".",       "PUNCT","PUNCT","punct",  3L,             NA_character_
  )
  res <- run_es(tks)
  expect_gte(res$f_54_modal_predictive, 1)
})

# ─── Distinción entre modales ─────────────────────────────────────────────

test_that("poder y deber se distinguen correctamente en el mismo documento", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "podemos","poder",  "AUX",  "AUX",  "aux",    2L,             "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "hacerlo","hacer",  "VERB", "VERB", "root",   NA_integer_,    "VerbForm=Inf",
    "d1",    1L,           3L,        ",",      ",",      "PUNCT","PUNCT","punct",  2L,             NA_character_,
    "d1",    2L,           4L,        "debemos","deber",  "AUX",  "AUX",  "aux",    5L,             "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    2L,           5L,        "intentarlo","intentar","VERB","VERB","root", NA_integer_,    "VerbForm=Inf",
    "d1",    2L,           6L,        ".",      ".",      "PUNCT","PUNCT","punct",  5L,             NA_character_
  )
  res <- run_es(tks)
  expect_equal(res$f_52_modal_possibility, 1)
  expect_equal(res$f_53_modal_necessity,   1)
})

# ─── Verbos especializados ────────────────────────────────────────────────

test_that("f_55 cuenta verbos públicos (afirmar, decir)", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,   ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "afirmó", "afirmar","VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin",
    "d1",    1L,           2L,        "que",    "que",    "SCONJ","SCONJ","mark",   3L,             NA_character_,
    "d1",    1L,           3L,        "era",    "ser",    "VERB", "VERB", "ccomp",  1L,             "Mood=Ind|Number=Sing|Person=3|Tense=Imp|VerbForm=Fin",
    "d1",    1L,           4L,        "verdad", "verdad", "NOUN", "NOUN", "attr",   3L,             NA_character_,
    "d1",    1L,           5L,        ".",      ".",      "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_gte(res$f_55_verb_public, 1)
})

test_that("f_56 cuenta verbos privados (creer, pensar)", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma,  ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "creo", "creer", "VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "que",  "que",   "SCONJ","SCONJ","mark",   3L,             NA_character_,
    "d1",    1L,           3L,        "es",   "ser",   "VERB", "VERB", "ccomp",  1L,             "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           4L,        "cierto","cierto","ADJ", "ADJ",  "attr",   3L,             NA_character_,
    "d1",    1L,           5L,        ".",    ".",     "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_gte(res$f_56_verb_private, 1)
})

test_that("f_58 cuenta verbos seem (parecer, resultar)", {
  tks <- make_es_tokens(
    ~doc_id, ~sentence_id, ~token_id, ~token,    ~lemma,   ~upos,  ~xpos,  ~dep_rel, ~head_token_id, ~feats,
    "d1",    1L,           1L,        "parece",  "parecer","VERB", "VERB", "root",   NA_integer_,    "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           2L,        "que",     "que",    "SCONJ","SCONJ","mark",   3L,             NA_character_,
    "d1",    1L,           3L,        "funciona","funcionar","VERB","VERB","ccomp",  1L,             "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "d1",    1L,           4L,        ".",       ".",      "PUNCT","PUNCT","punct",  1L,             NA_character_
  )
  res <- run_es(tks)
  expect_gte(res$f_58_verb_seem, 1)
})

# nolint end
