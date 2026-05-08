# Antibody outcomes report GMC. Units are either IU/mL or EU/mL (ELISA units/mL). For meta-analysis, rr, rr_lower, and rr_upper are ratio of vax to unvax
# outcome: abbreviated outcome
# outcome_asis: outcome label for plot and dropdown menu
# TE, lower, upper on log scale
# rr_estimate, rr_lower, rr_upper on linear scale
# NOTE: rows without a forest_plot_id could not be pooled (no meta-analysis performed)
# forest_plot_title: format is outcome (domain) - study_design
# tau_sq, i_sq, p: From meta analysis. NOTE: for forest_plot_id = 6, these columns are NA because 0 events in 1 one of the 2 studies pooled
# study_design: levels include RCT, Observational, Observational (adjusted)

# Upload raw data
meta <- read_csv("/Users/michaelabers/Desktop/R packages/Tdap/data-raw/tdap_all_meta_forest.csv") %>%
  select(-c(rob, study, type, type_long, follow_up, n_vax_outcome, n_vax_no_outcome, n_unvax_outcome, n_unvax_no_outcome, forest_plot_title, outcome))
names(meta) <- tolower(names(meta))
meta <- meta %>%
  rename(
    study_id = id,
    id_meta = forest_plot_id,
    #analysis_label = forest_plot_title,
    outcome = outcome_asis,
    study = study_label,
    rr = rr_estimate,
    effect_measure = est.type,
    n_vax = n.e,
    mean_vax = mean.e,
    lower_vax = lower.e,
    upper_vax = upper.e,
    n_events_vax = event.e,
    n_events_unvax = event.c,
    sd_vax = sd.e,
    se_vax = se.e,
    n_unvax = n.c,
    mean_unvax = mean.c,
    lower_unvax = lower.c,
    upper_unvax = upper.c,
    sd_unvax = sd.c,
    se_unvax = se.c
  )

# Add meta data to pooled columns
g <- meta %>%
  filter(study != "Pooled") %>%
  select(id_meta, domain, study_design, outcome, ab) %>%
  distinct()
g <- meta %>%
  filter(study == "Pooled") %>%
  select(-c(domain, study_design, outcome, ab)) %>%
  lj(g, by = "id_meta")
meta <- meta %>%
  filter(study != "Pooled") %>%
  select(-c(tau_sq, i_sq, p)) %>%
  bind_rows(g)
remove(g)

# Add columns
meta <- meta %>%
  mutate(
    rob_original = rob,
    rob = ifelse(rob == "Low (except for uncontrolled confounding)", "Low", rob),
    is_meta = ifelse(study == "Pooled", 1L, 0L),
    is_rct = ifelse(study_design == "RCT", 1L, 0L),
    study_design = ifelse(study_design == "Observational", "Observational (unadjusted)", study_design),
    outcome = gsub("Pertussis Antibodies: ", "", outcome, fixed = TRUE),
    outcome = case_when(
      outcome == "Apgar Score" ~ "Apgar score",
      outcome == "NICU Admission" ~ "NICU admission",
      outcome == "Gestational Diabetes" ~ "Gestational diabetes",
      outcome == "Preterm Birth (Premature Delivery)" ~ "Preterm birth",
      outcome == "Serious Adverse Events" ~ "Severe adverse events",
      outcome == "Miscarriage or Spontaneous Abortion" ~ "Miscarriage/spontaneous abortion",
      outcome == "Neonatal Sepsis" ~ "Neonatal sepsis",
      outcome == "Neonatal Death" ~ "Neonatal mortality",
      outcome == "Pertussis Infection: Infants" ~ "Pertussis infection (infant)",
      outcome == "Death: Mortality Attributed to Pertussis in Infants" ~ "Pertussis-related mortality (infant)",
      outcome == "Low Birth Weight" ~ "Low birth weight",
      outcome == "Complications: Severe Complications of Pertussis in Infants" ~ "Pertussis complications (infant)",
      outcome == "Congenital Abnormalities" ~ "Congenital abnormalities",
      outcome == "Other Adverse Events" ~ "Other adverse events",
      outcome == "Other Pregnancy-related Adverse Events" ~ "Other pregnancy-related adverse events",
      outcome == "Breast Milk" ~ " Ab (breast milk)",
      outcome == "Infant Blood at 2mo" ~ " Ab (infant blood, age 2 mo)",
      outcome == "Infant Blood at 5mo" ~ " Ab (infant blood, age 5 mo)",
      outcome == "Infant Cord Blood at Birth" ~ " Ab (infant blood, birth)",
      outcome == "Maternal Blood at Birth" ~ " Ab (maternal blood, delivery)",
      outcome == "Fever" ~ "Fever (maternal)",
      outcome == "Small for Gestational Age (growth restriction)" ~ "Small for gestational age",
      outcome == "Preeclampsia or Eclampsia (Hypertensive Disorders)" ~ "Preeclampsia/eclampsia",
      outcome == "Other Placental Conditions (e.g., abruption)" ~ "Other placental conditions (non-chorioamnionitis)",
      .default = outcome
    ),
    outcome = ifelse(grepl("^ Ab \\(", outcome), paste0("Anti-", ab, outcome), outcome),
    population = case_when(
      domain == "Safety - infant" | grepl("infant", outcome) ~ "Infant",
      domain == "Safety - maternal" | grepl("maternal blood|breast milk", outcome) ~ "Maternal",
      .default = NA_character_
    ),
    ab_abbrev = ab,
    ab = case_when(
      ab == "FHA" ~ "Anti-filamentous hemagglutinin",
      ab == "PRN" ~ "Anti-pertactin",
      ab == "PT" ~ "Anti-pertussis toxin",
      ab == "FIM" ~ "Anti-fimbriae",
      .default = NA_character_
    ),
    ab_footnote = case_when(
      ab == "FHA" ~ "FHA = filamentous hemagglutinin",
      ab == "PRN" ~ "PRN = pertactin",
      ab == "PT" ~ "PT = pertussis toxin",
      ab == "FIM" ~ "FIM = fimbriae",
      .default = NA_character_
    ),
    analysis_label = case_when(
      study_design == "Observational (unadjusted)" ~ "Observational studies (unadjusted)",
      study_design == "Observational (adjusted)" ~ "Observational studies (adjusted)",
      study_design == "RCT" ~ "RCTs",
      .default = NA_character_
    ),
    analysis_label = paste(outcome, analysis_label, sep = " - "),
    analysis_label = ifelse(analysis_label == "NA - NA", NA_character_, analysis_label)
  )

# Add columns to order by another variable
order_studies <- function(x, ..., group_vars = "id_meta", rev = FALSE) {
  by_cols <- dots_as_quoted(...)
  by_cols_prefix <- paste(by_cols, collapse = "_")
  x$IDX <- seq_len(nrow(x))
  x <- dplyr::arrange(x, ...)
  x <- dplyr::group_by(x, is_meta, !!!rlang::syms(group_vars))
  if (rev) {
    x <- dplyr::mutate(x, ORDER = dplyr::n():1L)
  } else {
    x <- dplyr::mutate(x, ORDER = 1L:dplyr::n())
  }
  x <- dplyr::ungroup(x)
  x$ORDER[x$is_meta] <- 0L
  names(x)[names(x) == "ORDER"] <- paste0(by_cols_prefix, "_order")
  x <- dplyr::arrange(x, IDX)
  x$IDX <- NULL
  x
}

meta$row_order <- seq_len(nrow(meta))
meta <- meta %>%
  arrange(rr, rr_lower, rr_upper) %>%
  order_studies(rr)
meta <- meta %>%
  arrange(rr_lower, rr, rr_upper) %>%
  order_studies(rr_lower)
meta <- meta %>%
  arrange(rr_upper, rr, rr_lower) %>%
  order_studies(rr_upper)
meta <- arrange(meta, row_order)
meta$row_order <- NULL
remove(order_studies)

# Add links
if (FALSE) {
  #source("~/Desktop/Statistics/R/Statistics/PubMed/convert_pmid_doi.R")
  meta <- meta %>%
    mutate(
      #pmcid = ifelse(grepl("^PMC", pmid_or_doi), pmid_or_doi, NA_character_),
      pmid = ifelse(!grepl("[A-z]", pmid_or_doi), pmid_or_doi, NA_character_),
      link = case_when(
        !is.na(pmcid) ~ paste0("https://pmc.ncbi.nlm.nih.gov/articles/", pmcid),
        !is.na(pmid) ~ paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid),
        !is.na(doi) & !grepl("https://doi.org/", doi, fixed = TRUE) ~ paste0("https://doi.org/", doi),
        !is.na(doi) ~ doi,
        .default = pmid_or_doi
      )
    )
}

# Reorder columns
meta <- meta %>%
  select(
    id_meta, domain, analysis_label,
    study,
    is_meta, is_rct, study_design,
    outcome, population,
    rr, rr_lower, rr_upper,
    tau_sq, p, i_sq,
    effect_measure, rr_order, rr_lower_order, rr_upper_order,
    rob, rob_original,
    ab, ab_abbrev, ab_footnote,
    everything()
  ) %>%
  filter(!is.na(analysis_label)) %>%
  select(-study_id)

# Remove id_meta 6
all_data <- meta
meta <- meta %>% filter(id_meta != 6, !is.na(id_meta))

# Export data
usethis::use_data(all_data, overwrite = TRUE)
usethis::use_data(meta, overwrite = TRUE)

# Create csv file for v1 folder
unlink("/Users/michaelabers/Desktop/R packages/Tdap/inst/v1/df_shiny.csv")
write_csv(meta, "df_shiny", directory = "/Users/michaelabers/Desktop/R packages/Tdap/inst/v1")
