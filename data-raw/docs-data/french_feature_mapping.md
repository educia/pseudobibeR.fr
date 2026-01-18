# French Feature Mapping Plan

This document captures the current plan for supporting Biber-style features in the French implementation (`pseudobibeR.fr`). Each feature from the English package is categorized by expected implementation approach, required resources, and open questions.

Legend:
- **Direct**: Covered by straightforward POS/dep/morph cues or closed word lists.
- **Heuristic**: Requires custom rule engineering, composite cues, or new resources.
- **Deferred**: Currently out of scope or unsupported in French; document fallback.

## A. Tense and Aspect Markers

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_01_past_tense | Direct | UD `VerbForm=Fin`, `Tense=Imp|Past|Pqp`; spaCy `Morph` | Need to account for auxiliary-driven compound past (avoir/être + `VerbForm=Part`). |
| f_02_perfect_aspect | Heuristic | Aux lemma avoir/être + participle dep; `Mood=Ind`, `Tense=Past` | Distinguish periphrastic perfect vs. passive; handle être with movement verbs. |
| f_03_present_tense | Direct | UD `Tense=Pres` | Confirm spaCy French outputs equivalent morph traits. |

## B. Place and Time Adverbials

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_04_place_adverbials | Direct | French closed list in `dict` | Build canonical set from CSV suggestions + existing lexicons. |
| f_05_time_adverbials | Direct | French closed list | Mirror approach used in English dictionary. |

## C. Pronouns and Pro-Verbs

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_06_first_person_pronouns | Direct | Closed list; lemma normalization | Include clitic forms (m', nous). |
| f_07_second_person_pronouns | Direct | Closed list | Capture enclitics (t', vous). |
| f_08_third_person_pronouns | Direct | Closed list | Include on, leur, lui, se (when pronominal). |
| f_09_pronoun_it | Heuristic | Pattern for impersonal "il" | Differentiate referential vs. expletive; rely on dependency label (`expl`). |
| f_10_demonstrative_pronoun | Direct | Closed list + syntactic role | Use dependency context to avoid determiners. |
| f_11_indefinite_pronoun | Direct | Closed list | Align with quantifier forms (quelques-uns, etc.). |
| f_12_proverb_do | Deferred | n/a | No robust French equivalent; document omission. |

## D. Questions

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_13_wh_question | Heuristic | POS/PronType=Int; sentence-final "?" | Need to capture fronted interrogatives and inversion patterns. |

## E. Nominal Forms

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_14_nominalizations | Heuristic | suffix rules (`-tion`, `-ment`, `-ité`, `-ence`, `-age`, etc.) | Expand suffix set via linguistic refs; exclude false positives from stoplist. |
| f_15_gerunds | Heuristic | Pattern `en` + `VerbForm=Part` (`Mood=Ger`) | Use UD `VerbForm=Ger` where available; fallback to token suffix `-ant`. |
| f_16_other_nouns | Direct | Baseline noun counts minus f_14/f_15 | Ensure shared logic with English implementation. |

## F. Passives

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_17_agentless_passives | Heuristic | Aux être + past participle; dep `aux:pass` | Confirm UD French marks `aux:pass`; exclude reflexives. |
| f_18_by_passives | Heuristic | Same as f_17 plus `obl:agent` w/ lemma `par` | Validate `obl:agent` vs. `case=par`. |

## G. Stative Forms

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_19_be_main_verb | Direct | Lemma être, dep `cop` absent | Count être as main predicate (non-copular). |
| f_20_existential_there | Direct | Fixed expressions (`il y a`, `il existe`) | Create phrase-level matcher; rely on multi-token detection. |

## H. Subordination Features

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_21_that_verb_comp | Heuristic | Verb + marker lemma `que` (`mark`, `compl`) | Need verb list? may be general. |
| f_22_that_adj_comp | Heuristic | ADJ + `que` complement; closed adjective list | Build curated adjective set (content, heureux, etc.). |
| f_23_wh_clause | Heuristic | Relativizers `ce que/qui/dont`; UD `mark` | Manage multi-token sequences. |
| f_24_infinitives | Direct | `VerbForm=Inf` | Straight from morph features. |
| f_25_present_participle | Heuristic | `VerbForm=Part`, `Tense=Pres` or `VerbForm=Ger` | Focus on adverbial gerunds. |
| f_26_past_participle | Heuristic | `VerbForm=Part`, `Tense=Past` used adverbially | Detect preposed participle phrases (`participe passé détaché`). |
| f_27_past_participle_whiz | Heuristic | Post-nominal participle modifying noun | Check dep `acl:relcl`/`acl` with `VerbForm=Part`. |
| f_28_present_participle_whiz | Heuristic | Post-nominal `VerbForm=Ger|Part` with `PartType=Pres` | Likely use `acl` + morph traits. |
| f_29_that_subj | Heuristic | Relative clause on subject with `qui` | Use dep `nsubj` + `relcl`. |
| f_30_that_obj | Heuristic | Relative clause on object with `que` | Use dep `obj` + `acl:relcl`. |
| f_31_wh_subj | Heuristic | `qui`/`lequel` subject relatives | Similar to f_29 but lexical filter. |
| f_32_wh_obj | Heuristic | `que`/`lequel` object relatives | Parallel to f_30. |
| f_33_pied_piping | Heuristic | ADP + relative pronoun | Pattern for `prep + lequel/laquelle/...`. |
| f_34_sentence_relatives | Heuristic | `ce qui/que` referencing prior clause | Recognize `ce qui est` etc. |
| f_35_because | Direct | Closed list (`parce que`, `puisque`, `car`) | Add to dictionary with multi-token support. |
| f_36_though | Direct | Closed list (`bien que`, `quoique`, `même si`) | Provide synonyms list. |
| f_37_if | Direct | Closed list (`si`, `à moins que`) | Evaluate morphological variants. |
| f_38_other_adv_sub | Heuristic | Residual subordinators list | Derive from CSV; confirm UD tagging as `mark`. |

## I. Prepositional Phrases, Adjectives, Adverbs

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_39_prepositions | Direct | POS `ADP`, dep `case` | Align with UD; treat multiword preps (`au-dessus de`). |
| f_40_adj_attr | Direct | `amod` dependents (`pos=ADJ`) | Some adjectives post-nominal; same rule as English. |
| f_41_adj_pred | Heuristic | Copular être + ADJ predicate | Identify `cop` constructions, allow adjective complements. |
| f_42_adverbs | Direct | POS `ADV` | Consider negative particles counted separately. |

## J. Lexical Specificity

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_43_type_token | Direct | Quanteda textstat_lexdiv | Language-agnostic. |
| f_44_mean_word_length | Direct | Token lengths | Language-agnostic. |

## K. Lexical Classes

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_45_conjuncts | Direct | Dictionary list (cependant, de plus) | Build curated list; allow multiword. |
| f_46_downtoners | Direct | Dictionary list | Include locutions (`un peu`). |
| f_47_hedges | Direct | Dictionary list | Support periphrastic patterns (`plus ou moins`). |
| f_48_amplifiers | Direct | Dictionary list | Include `tellement`, `vraiment`. |
| f_49_emphatics | Heuristic | Mixed lexical + syntactic cues | Decide coverage (bien sûr, vraiment). |
| f_50_discourse_particles | Direct | Dictionary list (eh bien, enfin) | Manage sentence-initial detection. |
| f_51_demonstratives | Direct | Determiners `ce/cet/cette/ces` | Overlaps with pronouns; rely on dep role to avoid double counting. |

## L. Modals

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_52_modal_possibility | Direct | Lemma `pouvoir`; modal auxiliaries | Account for inflected forms (`peut`, `pourrait`). |
| f_53_modal_necessity | Direct | Lemmas `devoir`, `falloir` | Include `il faut`, `il faudrait`. |
| f_54_modal_predictive | Heuristic | Futur simple + `aller + INF` | Use morph `Tense=Fut`; detect `aller` + inf. |

## M. Specialized Verb Classes

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_55_verb_public | Heuristic | French verb lexicon | Translate/curate list; consider lemmas vs. inflections. |
| f_56_verb_private | Heuristic | Lexicon | Build lemma list (penser, croire, savoir, etc.). |
| f_57_verb_suasive | Heuristic | Lexicon | Map English list to French (ordonner, proposer). |
| f_58_verb_seem | Direct | Lemmas `sembler`, `paraître`, `avoir l'air` | Add compound expressions. |

## N. Reduced Forms and Dispreferred Structures

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_59_contractions | Deferred | n/a | French written contractions differ; omit or treat separately. |
| f_60_that_deletion | Heuristic | Verb + complement without `que` | Low frequency; may rely on dependency gap detection. |
| f_61_stranded_preposition | Heuristic | ADP without object + relative clause | Hard in French; consider detecting ADP at clause end. |
| f_62_split_infinitive | Heuristic | `à/de` + ADV + infinitive | Rare; might defer unless data supports. |
| f_63_split_auxiliary | Heuristic | Aux + ADV + participle | Manage multiword auxiliaries. |

## O. Coordination

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_64_phrasal_coordination | Heuristic | `conj` relations on phrases | Mirror English logic with French POS tags. |
| f_65_clausal_coordination | Heuristic | Clause-initial `et` | Need sentence segmentation reliability. |

## P. Negation

| Feature | Approach | Dependencies | Notes |
| --- | --- | --- | --- |
| f_66_neg_synthetic | Heuristic | Determiners `aucun`, `nul`, etc. | Build lexical list; check for double counts with analytics. |
| f_67_neg_analytic | Direct | `ne` + neg adverbs (`pas`, `plus`, `jamais`) | Use dependency pairing; ensure tokenization (ne ... pas). |

## Next Actions

1. Prototype extraction of UD morph features using sample sentences to validate availability (`udpipe` vs. spaCy French model).
2. Draft French `dict` and `word_lists` covering direct-list features (Sections B, C, K).
3. Prioritize heuristic implementations with highest coverage payoff (Sections H, P).
4. Document deferred features (f_12, f_59) in package reference and README.
