# =============================================================================
# master_analysis.R
#
# Thesis: "Does Project Design Quality Predict Development Outcomes?
#          Evidence from World Bank Investment Projects in South Asia"
# Author: Sargam Kuckreja
# Course: EKHM73 Economic History, Lund University (LUSEM)
# Date:   2026
#
# DESCRIPTION
# -----------
# This is the single master analysis script for the thesis. It reproduces
# all results in the order they appear in the paper, from raw data inputs
# through to regression tables and figures.
#
# PREREQUISITES — files that must be in the working directory:
#   Ratings.xlsx              IEG Project Performance Ratings Database
#                             (World Bank Data Catalog, accessed March 2026)
#   url_checkpoint.csv        Document URL lookup table (from data collection)
#   pdfs/                     Folder of downloaded PAD/SAR PDFs (N = 1,127)
#   specificity_rescored.csv  Split specificity scores (from rescore step below)
#
# OUTPUTS
# -------
#   analysis_dataset_final.csv      Full scored analytical dataset (N = 1,110)
#   analysis_dataset_final_v2.csv   + tangibility variable
#   score_checkpoint.csv            Scoring cache (resume if interrupted)
#   fig1–fig9  (.pdf + .png)        All thesis figures
#   regression_models.rds           All model objects (A1–G4)
#
#   Regression tables are exported as standalone HTML files
#   (table1_A.html … table5_G.html). Open in any browser or copy-paste
#   into Word. These are generated separately — see regression_tables.R.
#
# SCRIPT STRUCTURE
# ----------------
#   Step 0:  Package installation and loading
#   Step 1:  Load and prepare project metadata
#   Step 2:  Score PDFs on three theory dimensions (RBM, PDIA, Specificity)
#   Step 3:  Merge specificity rescore (split density scores)
#   Step 4:  Build final analytical dataset
#   Step 5:  Descriptive statistics and figures (Figures 1–7)
#   Step 6:  Tangibility variable + figures (Figures 8–9)
#   Step 7:  Regression analysis (Models A1–A3, B, C, D1–D3, E1–E3)
#   Step 8:  Tangibility regression models (Models F, G1–G4)
#
# REPRODUCIBILITY NOTE
# --------------------
# PDF scoring (Step 2) uses a checkpoint file so it can be interrupted and
# resumed. Delete score_checkpoint.csv to force a complete rescore from
# scratch. The script was developed on R 4.5.x; set.seed() is not required
# as no stochastic methods are used.
# =============================================================================


# ── Step 0: Packages ──────────────────────────────────────────────────────────
setwd("~/Desktop/EAEGD/THESIS‼️/Analysis 2")

cat("── Step 0: Installing and loading packages ─────────────────\n")

required <- c(
  "readxl",        # read Ratings.xlsx
  "readr",         # read/write CSV
  "dplyr",         # data manipulation (loaded last — overrides MASS::select)
  "stringr",       # string operations for text scoring
  "pdftools",      # PDF text extraction
  "ggplot2",       # figures
  "scales",        # axis formatting
  "forcats",       # factor reordering in plots
  "tidyr",         # pivot_longer for Fig 9
  "MASS",          # polr() for ordinal logistic regression
  "sandwich",      # robust standard errors (HC1)
  "lmtest"         # coeftest() for robust inference
)

new_pkgs <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) {
  cat("Installing missing packages:", paste(new_pkgs, collapse=", "), "\n")
  install.packages(new_pkgs)
}

library(readxl); library(readr);   library(stringr)
library(pdftools); library(ggplot2); library(scales)
library(forcats);  library(tidyr);   library(MASS)
library(sandwich); library(lmtest)
library(dplyr)  # load LAST — dplyr::select() overrides MASS::select()

cat("Packages loaded.\n")


# ── Step 1: Load and prepare project metadata ─────────────────────────────────
#
# Source: IEG Project Performance Ratings Database (World Bank Data Catalog)
# URL:    https://datacatalog.worldbank.org
# Note:   Global Practice field is missing for older projects (pre-2014).
#         These are classified using a keyword classifier applied to project
#         names, validated against the non-missing subsample.

cat("\n── Step 1: Loading project metadata ────────────────────────\n")

# Keyword-based sector classifier for projects missing Global Practice field.
# Applied to project names in upper case. Returns the first matching sector,
# defaulting to "Macroeconomics, Trade and Investment" if no match found.
classify_sector <- function(name) {
  n <- toupper(as.character(name))
  if (any(sapply(c("EDUC","SCHOOL","UNIVERSITY","HIGHER ED","LITERACY",
                   "TRAINING","SKILL","VOCATIONAL","TEACHER","TECH EDUC",
                   "LEARNING"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Education")
  if (any(sapply(c("HEALTH","HOSPITAL","NUTRITION","POPULATION","HIV","AIDS",
                   "MALARIA","TUBERCULOSIS","POLIO","LEPROSY","IMMUNIZATION",
                   "CHILD HEALTH","FAMILY HEALTH","FAMILY WELFARE","BLINDNESS",
                   "DISEASE","MEDICAL","REPRODUCTIVE","PANDEMIC","COVID","POPUL"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Health, Nutrition & Population")
  if (any(sapply(c("WATER SUP","WATER &","W/S ","SEWAGE","SEWERAGE","DRAINAGE",
                   "FLOOD","WATERSHED","CANAL","WASA","IRRIG","TUBEWELL",
                   "LIFT PUMP","LOW LIFT","SCARP","BARRAGES","WATER MGT",
                   "WATER MANAGE","DAM "),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Water")
  if (any(sapply(c("POWER","ENERGY","ELECTRIC","THERMAL","GAS DEV","PETROLEUM",
                   "OIL &","OIL AND","COAL","HYDROPOWER","RENEWABLE","SOLAR",
                   "ELECTRIF","TRANSMISS","PIPELINE","WAPDA","LPG","REFINER"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Energy & Extractives")
  if (any(sapply(c("ROAD","HIGHWAY","TRANSPORT","RAILWAY","PORT","BRIDGE",
                   "TRANSIT","AIRPORT","SHIPPING","RURAL ACCESS",
                   "INLAND WATER TRANS"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Transport")
  if (any(sapply(c("AGRIC","FARM","CROP","LIVESTOCK","FOOD","FISHERI","FORESTRY",
                   "RURAL DEV","DAIRY","FERTILIZER","SEEDS","HORTICULTURE","JUTE",
                   "RUBBER","TEA","COTTON","CASHEW","SERICULT","SUGAR","MANGROVE",
                   "SHRIMP","OILSEED","GRAIN","AGRIC RES","AG RES","AG EXT",
                   "NARAYANI","SUNSARI"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Agriculture and Food")
  if (any(sapply(c("FINANCE","FINANCIAL","BANK","CREDIT","MICROFINANCE",
                   "INSURANCE","PENSION","FISCAL","TAX","PRIVATIZ","INVESTMENT",
                   "ENTERPRISE","INDUSTRY","INDUSTRIAL","SME","TRADE","EXPORT",
                   "COMPETITIV","ICICI","IDBI","NDFC","NIDC","NABARD","ARDC",
                   "ADBP","IDBP","PICIC","BSB ","DFC-","IND. CR","INDUS. CR",
                   "NCDC","CEMENT","TEXTILE"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Finance, Competitiveness and Innovation")
  if (any(sapply(c("GOVERNANCE","PUBLIC ADMIN","CIVIL SERVICE","JUDICIAL",
                   "JUSTICE","LEGAL","PROCUREMENT","AUDIT","DECENTRALIZ",
                   "CIVIL SERV","CUSTOMS","STATISTICS","PUBLIC FINANCIAL",
                   "PFM ","INSTITUTION BUILD","CAPACITY BUILD"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Governance")
  if (any(sapply(c("URBAN","CITY","CITIES","HOUSING","LAND TITLE","LAND ADMIN",
                   "SLUM","METRO","MUNICIPAL","CALCUTTA","BOMBAY URBAN",
                   "MADRAS URBAN","LAHORE URBAN","COLOMBO","KARACHI WATER"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Urban, Resilience and Land")
  if (any(sapply(c("ENVIRON","BIODIVERSITY","CLIMATE","ECOSYSTEM",
                   "NATURAL RESOURCE","POLLUTION","OZONE","ODS","CFC",
                   "WILDLIFE","CONSERVATION","GREEN","ECODEV"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Environment, Natural Resources & the Blue Economy")
  if (any(sapply(c("TELECOM","ICT","DIGITAL","E-GOVERN","COMMUNICATIONS"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Digital Development")
  if (any(sapply(c("SOCIAL PROTECT","SAFETY NET","SOCIAL ACTION",
                   "POVERTY ALLEVIATION","LIVELIHOOD","COMMUNITY DEV",
                   "SOCIAL INVEST","WOMEN","GENDER","DISABILITY",
                   "CASH TRANSFER","SOLIDARITY"),
                 function(x) grepl(x, n, fixed=TRUE)))) return("Social Policy")
  return("Macroeconomics, Trade and Investment")
}

# IEG 6-point ordinal outcome scale mapped to integer 1–6
outcome_map <- c(
  "Highly Satisfactory"       = 6,
  "Satisfactory"              = 5,
  "Moderately Satisfactory"   = 4,
  "Moderately Unsatisfactory" = 3,
  "Unsatisfactory"            = 2,
  "Highly Unsatisfactory"     = 1,
  "Not Rated"                 = NA_real_
)

# Load IEG ratings and classify sectors
raw <- read_excel("Ratings.xlsx")
gp  <- if ("Global Practice" %in% names(raw)) raw$`Global Practice` else NA_character_

ratings <- raw %>%
  mutate(
    # Use Global Practice field where available; fall back to classifier
    sector = ifelse(!is.na(gp) & gp != "",
                    gp, sapply(`Project Name`, classify_sector))
  ) %>%
  rename(
    project_id    = `Project ID`,
    project_name  = `Project Name`,
    country       = `Country / Economy`,
    instrument    = `Lending Instrument Type`,
    approval_fy   = `Approval FY`,
    closing_fy    = `Final Closing FY`,
    eval_type     = `Evaluation Type`,
    outcome_label = `Outcome`,
    quality_entry = `Quality at Entry`,
    quality_sup   = `Quality of Supervision`,
    bank_perf     = `Bank Performance`,
    me_quality    = `M&E Quality`,
    eval_fy       = `Evaluation FY`
  ) %>%
  mutate(
    outcome_score = outcome_map[outcome_label],
    # Document era: PAD format introduced 1995; earlier documents are SARs
    doc_era       = ifelse(approval_fy < 1995, "SAR", "PAD")
  ) %>%
  # Restrict to Investment Project Financing only (excludes DPF)
  filter(instrument == "IPF")

# Merge with URL checkpoint to identify which projects have PDFs on disk
url_ck <- read_csv("url_checkpoint.csv", show_col_types=FALSE) %>%
  dplyr::select(project_id, pdf_url, doc_type_found, doc_title, found)

ratings <- ratings %>% left_join(url_ck, by="project_id") %>%
  mutate(
    found         = ifelse(is.na(found), FALSE, found),
    pdf_on_disk   = file.exists(file.path("pdfs", paste0(project_id, ".pdf")))
  )

cat(sprintf("Total IPF projects in database:  %d\n",   nrow(ratings)))
cat(sprintf("PDFs located on disk:            %d\n",   sum(ratings$pdf_on_disk)))
cat(sprintf("With IEG outcome score:          %d\n",   sum(!is.na(ratings$outcome_score))))
cat(sprintf("PDF + outcome (analysis N):      %d\n\n",
            sum(ratings$pdf_on_disk & !is.na(ratings$outcome_score))))


# ── Step 2: Score PDFs on three theory dimensions ─────────────────────────────
#
# Three theory-motivated text scores are computed from the full text of each
# planning document. Scores are keyword densities (weighted counts per 1,000
# words) normalised to [0,1] via sigmoid transformation.
#
# Theoretical grounding:
#   RBM_score         Kusek & Rist (2004), OECD DAC (2010)
#   PDIA_score        Andrews, Pritchett & Woolcock (2017), Honig (2018)
#   SPECIFICITY_score Pritchett, Woolcock & Andrews (2013), Easterly (2014)
#
# NOTE: max_pages=Inf scores the full document. Delete score_checkpoint.csv
# before re-running if you previously used a truncated page limit.

cat("── Step 2: Scoring PDFs ────────────────────────────────────\n")
cat("  Uses full document text (max_pages = Inf)\n")
cat("  Checkpoint file: score_checkpoint.csv\n\n")

pdf_dir <- "pdfs"

extract_text <- function(project_id, max_pages=Inf) {
  path <- file.path(pdf_dir, paste0(project_id, ".pdf"))
  if (!file.exists(path)) return(NA_character_)
  tryCatch({
    pages <- pdf_text(path)
    paste(pages[1:min(length(pages), max_pages)], collapse="\n") %>%
      str_squish()
  }, error=function(e) NA_character_)
}

# DIM 1: RBM score
# Captures density of results-based measurement infrastructure:
# quantified targets, baseline data, M&E frameworks, verification sources,
# results-chain terminology. Higher score = stronger measurement foundation.
score_rbm <- function(text) {
  tl <- str_to_lower(text)
  dw <- max(str_count(tl, "\\S+"), 1)
  raw <-
    # Quantified targets: numbers combined with unit terms (weight 1.5)
    str_count(tl, "\\d+\\s*(%|percent|million|thousand|household|beneficiar|student|patient|farmer|km|ha|mw|unit|number of)") * 1.5 +
    # Baseline mentions (weight 2 for general, weight 3 for strong phrasing)
    str_count(tl, "\\bbaseline\\b") * 2 +
    as.numeric(str_detect(tl, "(baseline (of|data|value|survey|level)|from a baseline|against a baseline)")) * 3 +
    # Target mentions (weight 1 for general, weight 2 for strong phrasing)
    str_count(tl, "\\btarget\\b") * 1 +
    str_count(tl, "(target of \\d|target:\\s*\\d|target value|end.?line target)") * 2 +
    # Time-bound commitments (weight 1)
    str_count(tl, "(by \\d{4}|by year \\d|by end of project|by project completion|annually|by mid.?term)") * 1 +
    # M&E framework presence (weight 4 — most diagnostic of genuine RBM adoption)
    as.numeric(str_detect(tl, "(monitoring (and|&) evaluation|m(\\s*&\\s*|\\s+and\\s+)e framework|results framework|monitoring framework)")) * 4 +
    # Verification and measurement sources (weight 1.5)
    str_count(tl, "(means of verification|method of measurement|data source|data collection|verified by|survey data|administrative record)") * 1.5 +
    # Results-chain terminology (weight 3 — diagnostic of WB results framework)
    as.numeric(str_detect(tl, "(pdo.?level|outcome indicator|output indicator|intermediate result|results chain|results framework)")) * 3
  # Sigmoid normalisation: scale parameter k=30 calibrated to observed density range
  round(1 - exp(-(raw / (dw / 1000)) / 30), 4)
}

# DIM 2: PDIA score
# Captures density of complexity and adaptive management language:
# uncertainty acknowledgement, flexibility provisions, phased/pilot design,
# contingency planning, learning mechanisms, political economy engagement.
# Higher score = stronger adaptive design orientation.
score_pdia <- function(text) {
  tl <- str_to_lower(text)
  dw <- max(str_count(tl, "\\S+"), 1)
  raw <-
    # Uncertainty and complexity (weight 0.5 — common across all documents)
    str_count(tl, "\\b(uncertain|unpredictab|complex|challeng|difficult to predict|context.?depend|contingent on)\\b") * 0.5 +
    # Adaptation and flexibility (weight 2)
    str_count(tl, "(adaptive management|adaptation|flexibility|flexible implementation|adjust(ment|ing) (the )?design|revise|course correction|learning loop|iterative)") * 2 +
    # Piloting and phased implementation (weight 2)
    str_count(tl, "(phase \\d|phase (one|two|three|i|ii|iii)|pilot (phase|program|project|test)|phased approach|phased implementation|pilot and scale|test and learn)") * 2 +
    # Contingency planning (weight 1.5)
    str_count(tl, "(contingency|contingent|if (the|this|project)|in the event|should (the|this)|alternative approach|fallback|exit strategy)") * 1.5 +
    # Learning and feedback (weight 1.5)
    str_count(tl, "(lesson(s)? learned|learning (from|agenda|review)|mid.?term review|course correction|feedback (loop|mechanism)|review and adjust)") * 1.5 +
    # Political economy (weight 2 — most distinctively PDIA-aligned)
    str_count(tl, "(political (economy|will|commitment|constraint|risk)|vested interest|stakeholder power|resistance|opposition|political feasibility)") * 2 +
    # Local ownership and contextual grounding (weight 1)
    str_count(tl, "(local (context|knowledge|condition|capacit|need|partner|solution)|country.?driven|demand.?driven|locally.?led|locally.?owned|community.?led)") * 1
  # Sigmoid normalisation: k=15 (PDIA language is sparser than RBM)
  round(1 - exp(-(raw / (dw / 1000)) / 15), 4)
}

# DIM 3: Specificity score (ratio-based)
# Captures contextual grounding relative to generic boilerplate.
# Computed as: specific_density / (specific_density + generic_density).
# Higher score = more context-specific relative to generic template language.
# NOTE: This score has a ceiling problem (see Step 3 for improved split scores).
score_specificity <- function(text) {
  tl <- str_to_lower(text)
  dw <- max(str_count(tl, "\\S+"), 1)
  specific <-
    # Named local institutions and governance structures
    str_count(tl, "(ministry of|department of|directorate of|bureau of|authority of|board of|national (commission|agency|authority|institute|council|bank|fund)|provincial|district (office|government|authority)|municipality|panchayat|upazila|tehsil|gram sabha)") +
    # Specific geographic references (including South Asia-specific terms)
    str_count(tl, "(district(s)? of|province of|region of|state of|division of|upazila|thana|taluk|mandal|ward|village of|town of)") +
    # Empirical and statistical references grounded in local data
    str_count(tl, "(according to (the )?\\d{4}|\\d{4} (census|survey|data|report)|national (survey|data|statistic)|household survey|\\d+ percent of (the )?(population|household|farmer|woman|child))") +
    # Specific beneficiary groups (not generic "poor" or "vulnerable")
    str_count(tl, "(smallholder farmer|marginal farmer|landless|ultra.?poor|female.?headed household|tribal|scheduled (caste|tribe)|indigenous (people|community)|pastoralist|artisan|fisherfolk)")
  generic <-
    # Generic donor boilerplate and template language (negative signal)
    str_count(tl, "(best practice|international standard|global experience|lessons from|in line with|consistent with|aligned with|in accordance with|world class|state of the art|capacity building|institutional strengthening|good governance|gender mainstreaming|inclusive growth|holistic approach|integrated approach)")
  total <- specific + generic
  # If no specific or generic terms found, return a neutral score of 0.3
  if (total == 0) return(0.3)
  ratio <- specific / total
  # Add density bonus for absolute volume of specific references (max 0.2)
  density_bonus <- pmin(specific / (dw / 1000) / 10, 0.2)
  round(pmin(ratio + density_bonus, 1), 4)
}

# ── Scoring loop with checkpoint ──────────────────────────────────────────────
ck_file  <- "score_checkpoint.csv"
to_score <- ratings %>% filter(pdf_on_disk) %>% dplyr::select(project_id)
scores   <- to_score %>%
  mutate(RBM_score=NA_real_, PDIA_score=NA_real_, SPECIFICITY_score=NA_real_)

# Resume from checkpoint if it exists (avoids rescoring completed documents)
if (file.exists(ck_file)) {
  ck <- read_csv(ck_file, show_col_types=FALSE) %>%
    filter(project_id %in% scores$project_id)
  scores <- scores %>%
    left_join(ck %>% dplyr::select(project_id, RBM_score, PDIA_score,
                                   SPECIFICITY_score),
              by="project_id", suffix=c("", ".ck")) %>%
    mutate(
      RBM_score         = ifelse(is.na(RBM_score),         RBM_score.ck,         RBM_score),
      PDIA_score        = ifelse(is.na(PDIA_score),        PDIA_score.ck,        PDIA_score),
      SPECIFICITY_score = ifelse(is.na(SPECIFICITY_score), SPECIFICITY_score.ck, SPECIFICITY_score)
    ) %>%
    dplyr::select(project_id, RBM_score, PDIA_score, SPECIFICITY_score)
  cat(sprintf("Checkpoint found — %d already scored, resuming...\n",
              sum(!is.na(scores$RBM_score))))
}

for (i in seq_len(nrow(scores))) {
  if (!is.na(scores$RBM_score[i])) next  # skip if already scored
  pid  <- scores$project_id[i]
  text <- extract_text(pid)
  if (is.na(text) || nchar(text) < 300) next  # skip empty/corrupted PDFs
  tl <- str_to_lower(text)
  tryCatch({
    scores$RBM_score[i]         <- score_rbm(text)
    scores$PDIA_score[i]        <- score_pdia(text)
    scores$SPECIFICITY_score[i] <- score_specificity(text)
  }, error=function(e) {
    cat(sprintf("  Scoring error: %s — %s\n", pid, e$message))
  })
  if (i %% 100 == 0) {
    cat(sprintf("  Scored %d / %d\n", i, nrow(scores)))
    write_csv(scores, ck_file)
  }
}
write_csv(scores, ck_file)
cat(sprintf("Scoring complete. Valid scores: %d / %d projects\n",
            sum(!is.na(scores$RBM_score)), nrow(scores)))


# ── Step 3: Merge split specificity scores ────────────────────────────────────
#
# The ratio-based SPECIFICITY_score has a ceiling problem: ~49% of documents
# score exactly 1.0 because they contain no generic boilerplate at all, which
# leaves insufficient variation to detect a regression signal.
#
# As a robustness check, the specificity construct is split into two independent
# density components scored on the full document text (see rescore_specificity.R
# for the scoring procedure). Both components are included separately in the
# robustness specifications.
#
# SPECIFIC_density:  context-specific content per 1,000 words (higher = better)
# GENERIC_density:   generic boilerplate per 1,000 words (higher = more mimicry)

cat("\n── Step 3: Merging split specificity scores ────────────────\n")

if (file.exists("specificity_rescored.csv")) {
  spec_split <- read_csv("specificity_rescored.csv", show_col_types=FALSE) %>%
    dplyr::select(project_id, specific_density, generic_density)
  scores <- scores %>% left_join(spec_split, by="project_id")
  cat(sprintf("Split scores merged for %d projects\n",
              sum(!is.na(scores$specific_density))))
} else {
  cat("WARNING: specificity_rescored.csv not found.\n")
  cat("Run rescore_specificity.R first to generate split scores.\n")
  cat("Continuing without split scores (main results unaffected).\n")
  scores$specific_density <- NA_real_
  scores$generic_density  <- NA_real_
}


# ── Step 4: Build analytical dataset ─────────────────────────────────────────
#
# Merges metadata and scores, applies variable transformations, and adds the
# tangibility classification. Saves the final analytical dataset to CSV.

cat("\n── Step 4: Building analytical dataset ─────────────────────\n")

# Tangibility coding following Andrews, Pritchett & Woolcock (2017)
# Tangible (1):   sectors with physical outputs and short causal chains
# Intangible (0): sectors requiring institutional change and political will
# Mixed (NA):     sectors combining physical and institutional elements;
#                 retained in main sample as a separate category in regressions
tangible_sectors   <- c("Energy & Extractives","Transport","Water",
                        "Agriculture and Food","Digital Development")
intangible_sectors <- c("Governance","Finance, Competitiveness and Innovation",
                        "Macroeconomics, Trade and Investment","Social Policy")

analysis <- ratings %>%
  left_join(scores, by="project_id") %>%
  filter(!is.na(RBM_score), !is.na(outcome_score)) %>%
  mutate(
    # Temporal and document controls
    decade      = factor((approval_fy %/% 10) * 10),
    outcome_ord = factor(outcome_score, levels=1:6, ordered=TRUE),
    doc_era     = relevel(factor(doc_era), ref="SAR"),
    
    # Standardised theory scores (mean 0, SD 1) for regression
    RBM_z       = as.numeric(scale(RBM_score)),
    PDIA_z      = as.numeric(scale(PDIA_score)),
    SPEC_z      = as.numeric(scale(SPECIFICITY_score)),
    
    # Split specificity scores (standardised)
    SPEC_density_z    = as.numeric(scale(specific_density)),
    GENERIC_density_z = as.numeric(scale(generic_density)),
    
    # Factor controls with reference categories
    sector  = relevel(factor(sector),  ref="Education"),
    country = relevel(factor(country), ref="India"),
    
    # Tangibility variable
    tangible = case_when(
      sector %in% tangible_sectors   ~ 1L,
      sector %in% intangible_sectors ~ 0L,
      TRUE                           ~ NA_integer_
    ),
    tangibility_label = factor(
      case_when(tangible==1~"Tangible", tangible==0~"Intangible", TRUE~"Mixed"),
      levels=c("Tangible","Mixed","Intangible")
    )
  )

cat(sprintf("Final analytical sample: N = %d\n", nrow(analysis)))
cat("\nCountries:\n");  print(table(analysis$country))
cat("\nSectors:\n");    print(sort(table(as.character(analysis$sector)), decreasing=TRUE))
cat("\nTangibility:\n"); print(table(analysis$tangibility_label, useNA="always"))

# Score diagnostics
cat("\nTheory score descriptives:\n")
for (col in c("RBM_score","PDIA_score","SPECIFICITY_score")) {
  v <- analysis[[col]]
  cat(sprintf("  %-22s  mean=%.3f  sd=%.3f  min=%.3f  max=%.3f\n",
              col, mean(v,na.rm=T), sd(v,na.rm=T),
              min(v,na.rm=T), max(v,na.rm=T)))
}
cat("\nRaw correlations with outcome score:\n")
for (col in c("RBM_score","PDIA_score","SPECIFICITY_score")) {
  r <- cor(analysis[[col]], analysis$outcome_score, use="pairwise.complete.obs")
  cat(sprintf("  %-22s  r = %.3f\n", col, r))
}

write_csv(analysis, "analysis_dataset_final.csv")
cat("\nSaved: analysis_dataset_final.csv\n")

# ── Table 0: Descriptive statistics ─────────────────────────────────────────
desc_vars <- c(
  "outcome_score",
  "RBM_z",
  "PDIA_z",
  "SPEC_density_z",
  "GENERIC_density_z"
)

table0_numeric <- analysis %>%
  summarise(across(
    all_of(desc_vars),
    list(
      Mean = ~ mean(.x, na.rm = TRUE),
      SD   = ~ sd(.x, na.rm = TRUE),
      Min  = ~ min(.x, na.rm = TRUE),
      Max  = ~ max(.x, na.rm = TRUE)
    )
  )) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.+)_(Mean|SD|Min|Max)"
  )

table0_shares <- tibble::tibble(
  Variable = c("Share tangible", "Share intangible"),
  Mean = c(
    mean(analysis$tangible == 1, na.rm = TRUE),
    mean(analysis$tangible == 0, na.rm = TRUE)
  ),
  SD = NA_real_,
  Min = NA_real_,
  Max = NA_real_
)

table0 <- bind_rows(table0_numeric, table0_shares) %>%
  mutate(across(c(Mean, SD, Min, Max), ~ round(.x, 3)))

table0_ft <- flextable(table0) %>%
  set_caption("Table 0 — Descriptive statistics") %>%
  autofit()

flextable::save_as_html(
  table0_ft,
  path = "table0_descriptive_statistics.html"
)

cat("\nSaved: table0_descriptive_statistics.html\n")

# ── Step 5: Descriptive statistics and figures (Figures 1–7) ─────────────────
#
# All figures saved as both PDF (for thesis) and PNG (for previewing).
# Figures 1–7 appear in the Data chapter (Chapter 4).

cat("\n── Step 5: Descriptive figures ─────────────────────────────\n")

# Shared theme for all thesis figures
theme_thesis <- function(base_size=11) {
  theme_minimal(base_family="sans", base_size=base_size) +
    theme(
      plot.title         = element_text(face="bold", size=12, colour="#003366",
                                        margin=margin(b=3)),
      plot.subtitle      = element_text(size=9, colour="#555555",
                                        margin=margin(b=10)),
      plot.caption       = element_text(size=7.5, colour="#888888",
                                        hjust=0, margin=margin(t=8)),
      axis.title         = element_text(size=9, colour="#444444"),
      axis.text          = element_text(size=9, colour="#333333"),
      panel.grid.major.y = element_line(colour="#eeeeee", linewidth=0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill="white", colour=NA),
      panel.background   = element_rect(fill="white", colour=NA),
      legend.position    = "none"
    )
}

# Colour palette
NAVY <- "#003366"; BLUE <- "#2166AC"; TEAL <- "#1B7C6E"
CAPTION <- paste0(
  "Analytical sample: N = ", nrow(analysis), " scored IPF projects, South Asia.\n",
  "Source: World Bank IEG Project Performance Ratings Database."
)
overall_mean <- mean(analysis$outcome_score, na.rm=TRUE)

# Helper to save both PDF and PNG
save_fig <- function(p, name, w=7, h=5) {
  ggsave(paste0(name, ".pdf"), plot=p, width=w, height=h, device=cairo_pdf)
  ggsave(paste0(name, ".png"), plot=p, width=w, height=h, dpi=300)
  cat(sprintf("  Saved: %s\n", name))
}

# -- Figure 1: Projects by country --
f1_data <- analysis %>%
  count(country) %>%
  mutate(country=fct_reorder(country, n))

save_fig(
  ggplot(f1_data, aes(x=n, y=country)) +
    geom_col(fill=NAVY, width=0.65) +
    geom_text(aes(label=n), hjust=-0.25, size=3.2, colour="#333333") +
    scale_x_continuous(expand=expansion(mult=c(0, 0.14))) +
    labs(title="Analytical sample: projects by country",
         subtitle="IPF projects with PAD or SAR scored",
         x="Number of projects", y=NULL, caption=CAPTION) +
    theme_thesis(),
  "fig1_projects_by_country", w=6.5, h=4.5
)

# -- Figure 2: Projects by sector --
f2_data <- analysis %>%
  mutate(sector_short = as.character(sector) %>%
           str_replace("Environment, Natural Resources & the Blue Economy","Environment & NRM") %>%
           str_replace("Finance, Competitiveness and Innovation","Finance & Competitiveness") %>%
           str_replace("Health, Nutrition & Population","Health, Nutrition & Pop.") %>%
           str_replace("Urban, Resilience and Land","Urban, Resilience & Land") %>%
           str_replace("Macroeconomics, Trade and Investment","Macroeconomics & Trade") %>%
           str_wrap(width=24)) %>%
  count(sector_short) %>%
  filter(n >= 3) %>%
  mutate(sector_short=fct_reorder(sector_short, n))

save_fig(
  ggplot(f2_data, aes(x=n, y=sector_short)) +
    geom_col(fill=BLUE, width=0.7) +
    geom_text(aes(label=n), hjust=-0.25, size=3, colour="#333333") +
    scale_x_continuous(expand=expansion(mult=c(0, 0.14))) +
    labs(title="Analytical sample: projects by sector",
         x="Number of projects", y=NULL, caption=CAPTION) +
    theme_thesis() +
    theme(axis.text.y=element_text(size=8.5)),
  "fig2_projects_by_sector", w=7, h=6
)

# -- Figure 3: Projects by decade --
f3_data <- analysis %>% count(decade)

save_fig(
  ggplot(f3_data, aes(x=decade, y=n)) +
    geom_col(fill=TEAL, width=0.7) +
    geom_text(aes(label=n), vjust=-0.5, size=3.2, colour="#333333") +
    scale_y_continuous(expand=expansion(mult=c(0, 0.12))) +
    labs(title="Analytical sample: projects by approval decade",
         x="Approval decade", y="Number of projects", caption=CAPTION) +
    theme_thesis() +
    theme(panel.grid.major.y=element_line(colour="#eeeeee", linewidth=0.4)),
  "fig3_projects_by_decade", w=7, h=4.5
)

# -- Figure 4: IEG outcome distribution --
out_levels <- c("Highly Satisfactory","Satisfactory","Moderately Satisfactory",
                "Moderately Unsatisfactory","Unsatisfactory","Highly Unsatisfactory")
out_colors <- c("#2ca25f","#74c476","#fed976","#fd8d3c","#e31a1c","#800026")
f4_data <- analysis %>%
  filter(!is.na(outcome_label)) %>%
  count(outcome_label) %>%
  mutate(outcome_label=factor(outcome_label, levels=out_levels),
         pct=round(n/sum(n)*100, 1)) %>%
  arrange(outcome_label)

save_fig(
  ggplot(f4_data, aes(x=outcome_label, y=pct, fill=outcome_label)) +
    geom_col(width=0.72) +
    geom_text(aes(label=paste0(pct,"%")), vjust=-0.45, size=3.2, colour="#333333") +
    scale_fill_manual(values=out_colors) +
    scale_x_discrete(labels=function(x) str_wrap(x, width=12)) +
    scale_y_continuous(expand=expansion(mult=c(0,0.12)),
                       labels=function(x) paste0(x,"%")) +
    labs(title="Distribution of IEG outcome ratings",
         subtitle=paste0("N = ", sum(f4_data$n)),
         x=NULL, y="Share of projects (%)", caption=CAPTION) +
    theme_thesis() +
    theme(panel.grid.major.y=element_line(colour="#eeeeee", linewidth=0.4),
          axis.text.x=element_text(size=8.5)),
  "fig4_outcome_distribution", w=7.5, h=5
)

# -- Figure 5: Mean outcome by country --
f5_data <- analysis %>%
  group_by(country) %>%
  summarise(mean_out=mean(outcome_score,na.rm=TRUE), n=n(),
            se=sd(outcome_score,na.rm=TRUE)/sqrt(n), .groups="drop") %>%
  mutate(country=fct_reorder(country, mean_out))

save_fig(
  ggplot(f5_data, aes(x=mean_out, y=country)) +
    geom_col(fill=NAVY, width=0.65) +
    geom_text(aes(label=sprintf("%.2f", mean_out)),
              x=0.15, hjust=0, size=3, colour="white", fontface="bold") +
    geom_errorbar(aes(xmin=mean_out-1.96*se, xmax=mean_out+1.96*se),
                  width=0.3, colour="#aaaaaa", linewidth=0.55) +
    geom_text(aes(x=mean_out+1.96*se+0.08, label=paste0("n=",n)),
              hjust=0, size=2.8, colour="#666666") +
    geom_vline(xintercept=overall_mean, linetype="dashed",
               colour="#888888", linewidth=0.5) +
    scale_x_continuous(limits=c(0,6.2), breaks=0:6,
                       expand=expansion(mult=c(0,0.02))) +
    labs(title="Mean IEG outcome score by country",
         subtitle="Error bars = 95% CI. Dashed line = overall sample mean.",
         x="Mean IEG outcome score  (1 = Highly Unsatisfactory → 6 = Highly Satisfactory)",
         y=NULL, caption=CAPTION) +
    theme_thesis(),
  "fig5_outcome_by_country", w=7.5, h=4.5
)

# -- Figure 6: Mean outcome by sector --
f6_data <- analysis %>%
  mutate(sector_short = as.character(sector) %>%
           str_replace("Environment, Natural Resources & the Blue Economy","Environment & NRM") %>%
           str_replace("Finance, Competitiveness and Innovation","Finance & Competitiveness") %>%
           str_replace("Health, Nutrition & Population","Health, Nutrition & Pop.") %>%
           str_replace("Urban, Resilience and Land","Urban, Resilience & Land") %>%
           str_replace("Macroeconomics, Trade and Investment","Macroeconomics & Trade") %>%
           str_wrap(width=24)) %>%
  group_by(sector_short) %>%
  summarise(mean_out=mean(outcome_score,na.rm=TRUE), n=n(),
            se=sd(outcome_score,na.rm=TRUE)/sqrt(n), .groups="drop") %>%
  filter(n >= 5) %>%
  mutate(sector_short=fct_reorder(sector_short, mean_out))

save_fig(
  ggplot(f6_data, aes(x=mean_out, y=sector_short)) +
    geom_col(fill=TEAL, width=0.7) +
    geom_text(aes(label=sprintf("%.2f", mean_out)),
              x=0.15, hjust=0, size=2.9, colour="white", fontface="bold") +
    geom_errorbar(aes(xmin=mean_out-1.96*se, xmax=mean_out+1.96*se),
                  width=0.3, colour="#aaaaaa", linewidth=0.5) +
    geom_text(aes(x=mean_out+1.96*se+0.08, label=paste0("n=",n)),
              hjust=0, size=2.7, colour="#666666") +
    geom_vline(xintercept=overall_mean, linetype="dashed",
               colour="#888888", linewidth=0.5) +
    scale_x_continuous(limits=c(0,6.2), breaks=0:6,
                       expand=expansion(mult=c(0,0.02))) +
    labs(title="Mean IEG outcome score by sector",
         subtitle="Sectors with fewer than 5 projects excluded. Dashed line = overall mean.",
         x="Mean IEG outcome score  (1 = Highly Unsatisfactory → 6 = Highly Satisfactory)",
         y=NULL, caption=CAPTION) +
    theme_thesis() +
    theme(axis.text.y=element_text(size=8.5)),
  "fig6_outcome_by_sector", w=7.5, h=6
)

# -- Figure 7: Mean outcome over time --
f7_data <- analysis %>%
  mutate(decade_num=as.numeric(as.character(decade))) %>%
  group_by(decade_num) %>%
  summarise(mean_out=mean(outcome_score,na.rm=TRUE), n=n(),
            se=sd(outcome_score,na.rm=TRUE)/sqrt(n), .groups="drop")

save_fig(
  ggplot(f7_data, aes(x=decade_num, y=mean_out)) +
    geom_ribbon(aes(ymin=mean_out-1.96*se, ymax=mean_out+1.96*se),
                fill=BLUE, alpha=0.15) +
    geom_line(colour=NAVY, linewidth=1.1) +
    geom_point(colour=NAVY, size=3.5, fill="white", shape=21, stroke=1.5) +
    geom_text(aes(label=paste0(round(mean_out,2),"\n(n=",n,")")),
              vjust=-1.1, size=2.8, colour="#444444", lineheight=0.85) +
    geom_hline(yintercept=overall_mean, linetype="dashed",
               colour="#888888", linewidth=0.5) +
    scale_x_continuous(breaks=unique(f7_data$decade_num),
                       labels=paste0(unique(f7_data$decade_num),"s")) +
    scale_y_continuous(limits=c(1,7), breaks=1:6) +
    labs(title="Mean IEG outcome score over time",
         subtitle="Shaded band = 95% CI. Dashed line = overall mean.",
         x="Approval decade",
         y="Mean outcome score\n(1 = Highly Unsat. → 6 = Highly Sat.)",
         caption=paste0("Note: 1950s (n=1) and 2020s (n=7) should be interpreted with caution.\n",
                        CAPTION)) +
    theme_thesis() +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.major.y=element_line(colour="#eeeeee", linewidth=0.4)),
  "fig7_outcome_over_time", w=8, h=5
)

cat("Figures 1–7 saved.\n")


# ── Step 6: Tangibility variable and figures (Figures 8–9) ────────────────────
#
# Tangibility is a binary sector-level classification distinguishing projects
# with tractable causal chains (tangible) from those requiring institutional
# change (intangible). Motivated by Andrews, Pritchett & Woolcock (2017).
# Figure 8 appears in the Findings chapter; Figure 9 in Discussion.

cat("\n── Step 6: Tangibility figures ──────────────────────────────\n")

tang_colors <- c("Tangible"="#1B7C6E","Mixed"="#888780","Intangible"="#993C1D")

# -- Figure 8: Mean outcome by tangibility --
f8_data <- analysis %>%
  filter(!is.na(tangibility_label)) %>%
  group_by(tangibility_label) %>%
  summarise(mean_out=mean(outcome_score,na.rm=TRUE), n=n(),
            se=sd(outcome_score,na.rm=TRUE)/sqrt(n), .groups="drop")

save_fig(
  ggplot(f8_data, aes(x=tangibility_label, y=mean_out, fill=tangibility_label)) +
    geom_col(width=0.6) +
    geom_errorbar(aes(ymin=mean_out-1.96*se, ymax=mean_out+1.96*se),
                  width=0.2, colour="#555555", linewidth=0.5) +
    geom_text(aes(label=sprintf("%.2f\n(n=%d)", mean_out, n)),
              vjust=-0.3, size=3, colour="#333333") +
    scale_fill_manual(values=tang_colors) +
    scale_y_continuous(limits=c(0, 5.5), breaks=1:5) +
    labs(title="Mean IEG outcome score by project tangibility",
         subtitle="Tangible = infrastructure/service delivery; Intangible = institutional reform",
         x=NULL, y="Mean outcome score (1–6)", caption=CAPTION) +
    theme_thesis(),
  "fig8_outcome_by_tangibility", w=6, h=4.5
)

# -- Figure 9: Theory scores by tangibility --
# This figure appears in the Findings or Discussion chapter.
# Key finding: intangible projects have HIGHER RBM scores despite
# worse outcomes — consistent with the isomorphic mimicry hypothesis.
f9_data <- analysis %>%
  filter(!is.na(tangibility_label)) %>%
  tidyr::pivot_longer(cols=c(RBM_score, PDIA_score, SPECIFICITY_score),
                      names_to="theory", values_to="score") %>%
  mutate(theory=recode(theory, "RBM_score"="RBM", "PDIA_score"="PDIA",
                       "SPECIFICITY_score"="Specificity")) %>%
  group_by(tangibility_label, theory) %>%
  summarise(mean_score=mean(score,na.rm=TRUE),
            se=sd(score,na.rm=TRUE)/sqrt(n()), .groups="drop")

save_fig(
  ggplot(f9_data, aes(x=tangibility_label, y=mean_score, fill=tangibility_label)) +
    geom_col(width=0.6) +
    geom_errorbar(aes(ymin=mean_score-1.96*se, ymax=mean_score+1.96*se),
                  width=0.2, colour="#555555", linewidth=0.5) +
    facet_wrap(~theory, scales="free_y") +
    scale_fill_manual(values=tang_colors) +
    labs(title="Mean theory scores by project tangibility",
         subtitle="Intangible projects score higher on RBM despite worse outcomes — consistent with mimicry hypothesis",
         x=NULL, y="Mean score (0–1)", caption=CAPTION) +
    theme_thesis() +
    theme(strip.text=element_text(face="bold")),
  "fig9_theory_scores_by_tangibility", w=8, h=4
)

cat("Figures 8–9 saved.\n")

# Save v2 dataset (adds tangibility)
write_csv(analysis, "analysis_dataset_final_v2.csv")
cat("Saved: analysis_dataset_final_v2.csv (includes tangibility variable)\n")


# ── Step 7: Regression analysis — main models (A–E) ──────────────────────────
#
# All models include sector, country, decade, and document era controls.
# Reference categories: Education (sector), India (country),
#                       SAR (doc_era), 1950s (decade).
# Standard errors are heteroskedasticity-robust throughout.
#
# Models:
#   A1–A3  Each theory tested separately (isolation test)
#   B      Horse race — all three simultaneously (main OLS specification)
#   C      Ordinal logistic (preferred specification for ordered DV)
#   D1–D3  Theory × sector interactions (does effect vary by sector?)
#   E1–E3  Theory × decade interactions (has effect changed over time?)

cat("\n── Step 7: Regression analysis ─────────────────────────────\n")

# Helper to print key coefficients with stars
extract_coef <- function(model, term) {
  s <- summary(model)$coefficients
  if (!term %in% rownames(s)) return("not in model")
  r <- s[term, ]
  stars <- ifelse(r[4]<.001,"***", ifelse(r[4]<.01,"**",
                                          ifelse(r[4]<.05,"*",    ifelse(r[4]<.1, ".", " "))))
  sprintf("beta=%+.3f  se=%.3f  p=%.3f  %s", r[1], r[2], r[4], stars)
}

# Define control formula component (common to all models)
ctrl <- "sector + country + decade + doc_era"

# -- Models A1–A3: each theory in isolation --
cat("\n── Models A1–A3: each theory separately ────────────────────\n")
mA1 <- lm(as.formula(paste("outcome_score ~ RBM_z  +", ctrl)), data=analysis)
mA2 <- lm(as.formula(paste("outcome_score ~ PDIA_z +", ctrl)), data=analysis)
mA3 <- lm(as.formula(paste("outcome_score ~ SPEC_z +", ctrl)), data=analysis)
cat("RBM:         ", extract_coef(mA1,"RBM_z"),  "\n")
cat("PDIA:        ", extract_coef(mA2,"PDIA_z"), "\n")
cat("Specificity: ", extract_coef(mA3,"SPEC_z"), "\n")

# -- Model B: horse race (main specification) --
cat("\n── Model B: Horse race (all three simultaneously) ───────────\n")
mB <- lm(as.formula(paste("outcome_score ~ RBM_z + PDIA_z + SPEC_z +", ctrl)),
         data=analysis)
cat("RBM:         ", extract_coef(mB,"RBM_z"),  "\n")
cat("PDIA:        ", extract_coef(mB,"PDIA_z"), "\n")
cat("Specificity: ", extract_coef(mB,"SPEC_z"), "\n")
cat(sprintf("N=%d  R²=%.4f  Adj-R²=%.4f\n",
            nrow(analysis), summary(mB)$r.squared, summary(mB)$adj.r.squared))

# -- Model C: ordinal logistic (preferred specification) --
cat("\n── Model C: Ordinal logistic regression ──────────────────────\n")
mC <- polr(
  as.formula(paste("outcome_ord ~ RBM_z + PDIA_z + SPEC_z +", ctrl)),
  data=analysis, Hess=TRUE
)
ct  <- coef(summary(mC))
pv  <- pnorm(abs(ct[,"t value"]), lower.tail=FALSE) * 2
top <- !grepl("\\|", rownames(ct))
cat("Key theory coefficients (ordinal logit):\n")
for (term in c("RBM_z","PDIA_z","SPEC_z")) {
  if (term %in% rownames(ct)) {
    stars <- ifelse(pv[term]<.001,"***",ifelse(pv[term]<.01,"**",
                                               ifelse(pv[term]<.05,"*",ifelse(pv[term]<.1,".","  "))))
    cat(sprintf("  %-12s  coef=%+.4f  p=%.3f  %s\n",
                term, ct[term,"Value"], pv[term], stars))
  }
}

# -- Models D1–D3: sector interactions --
cat("\n── Models D1–D3: Theory × sector interactions ───────────────\n")
mD1 <- lm(as.formula(paste("outcome_score ~ RBM_z  * sector +",
                           "country + decade + doc_era")), data=analysis)
mD2 <- lm(as.formula(paste("outcome_score ~ PDIA_z * sector +",
                           "country + decade + doc_era")), data=analysis)
mD3 <- lm(as.formula(paste("outcome_score ~ SPEC_z * sector +",
                           "country + decade + doc_era")), data=analysis)

# Report only significant interactions (p < 0.10)
for (m_name in c("mD1","mD2","mD3")) {
  m <- get(m_name)
  s <- summary(m)$coefficients
  ix <- s[grep(":", rownames(s)),,drop=FALSE]
  sg <- ix[ix[,4] < 0.10,,drop=FALSE]
  if (nrow(sg)>0) {
    cat(sprintf("\n%s significant interactions (p < 0.10):\n", m_name))
    print(round(sg, 4))
  }
}

# -- Models E1–E3: decade interactions --
cat("\n── Models E1–E3: Theory × decade interactions ───────────────\n")
mE1 <- lm(outcome_score ~ RBM_z  * decade + PDIA_z + SPEC_z +
            sector + country + doc_era, data=analysis)
mE2 <- lm(outcome_score ~ PDIA_z * decade + RBM_z  + SPEC_z +
            sector + country + doc_era, data=analysis)
mE3 <- lm(outcome_score ~ SPEC_z * decade + RBM_z  + PDIA_z +
            sector + country + doc_era, data=analysis)

# Decade trend in theory scores (RBM paradox diagnostic)
cat("\nDecade trends in theory scores (RBM paradox check):\n")
print(analysis %>%
        mutate(decade_num=as.numeric(as.character(decade))) %>%
        group_by(decade_num) %>%
        summarise(n=n(),
                  RBM  = round(mean(RBM_score, na.rm=TRUE), 3),
                  PDIA = round(mean(PDIA_score, na.rm=TRUE), 3),
                  SPEC = round(mean(SPECIFICITY_score, na.rm=TRUE), 3),
                  Outcome = round(mean(outcome_score, na.rm=TRUE), 3),
                  .groups="drop"))


# ── Step 8: Tangibility regression models (F, G1–G4) ─────────────────────────
#
# Model F:     tangibility as a main-effect control alongside theory scores
# Models G1–G3: each theory × tangibility interaction separately
# Model G4:    all three × tangibility simultaneously (joint test)
#
# Sample: binary subset only (tangible + intangible; Mixed excluded)
# Reference for binary: Intangible (tangible_f baseline)

cat("\n── Step 8: Tangibility regression models ────────────────────\n")

# Model F: full sample with tangibility as control (replaces sector)
mF <- lm(outcome_score ~ RBM_z + PDIA_z + SPEC_z +
           tangibility_label + country + decade + doc_era,
         data=analysis %>% filter(!is.na(tangibility_label)))
cat("\nModel F — tangibility as main effect:\n")
cat("RBM:                 ", extract_coef(mF,"RBM_z"),  "\n")
cat("PDIA:                ", extract_coef(mF,"PDIA_z"), "\n")
cat("Specificity:         ", extract_coef(mF,"SPEC_z"), "\n")
cat("Mixed vs Tangible:   ", extract_coef(mF,"tangibility_labelMixed"), "\n")
cat("Intangible vs Tang:  ", extract_coef(mF,"tangibility_labelIntangible"), "\n")

# Binary subset for interaction models
df_bin <- analysis %>%
  filter(!is.na(tangible)) %>%
  mutate(tangible_f=factor(tangible, labels=c("Intangible","Tangible")))
cat(sprintf("\nBinary sample (tangible/intangible only): N = %d\n", nrow(df_bin)))

# Models G1–G3: individual theory × tangibility interactions
mG1 <- lm(outcome_score ~ RBM_z  * tangible_f + PDIA_z + SPEC_z +
            country + decade + doc_era, data=df_bin)
mG2 <- lm(outcome_score ~ PDIA_z * tangible_f + RBM_z  + SPEC_z +
            country + decade + doc_era, data=df_bin)
mG3 <- lm(outcome_score ~ SPEC_z * tangible_f + RBM_z  + PDIA_z +
            country + decade + doc_era, data=df_bin)

# Model G4: all three interactions jointly
mG4 <- lm(outcome_score ~
            RBM_z  * tangible_f +
            PDIA_z * tangible_f +
            SPEC_z * tangible_f +
            country + decade + doc_era,
          data=df_bin)

cat("\nModel G4 — all three theories × tangibility (joint test):\n")
for (term in c("RBM_z","RBM_z:tangible_fTangible",
               "PDIA_z","PDIA_z:tangible_fTangible",
               "SPEC_z","SPEC_z:tangible_fTangible")) {
  cat(sprintf("  %-35s  %s\n", term, extract_coef(mG4, term)))
}
cat(sprintf("N=%d  R²=%.4f  Adj-R²=%.4f\n",
            nrow(df_bin), summary(mG4)$r.squared, summary(mG4)$adj.r.squared))

# Save all model objects
saveRDS(
  list(mA1=mA1, mA2=mA2, mA3=mA3, mB=mB, mC=mC,
       mD1=mD1, mD2=mD2, mD3=mD3,
       mE1=mE1, mE2=mE2, mE3=mE3,
       mF=mF, mG1=mG1, mG2=mG2, mG3=mG3, mG4=mG4),
  "regression_models.rds"
)
cat("\nSaved: regression_models.rds (all 16 model objects)\n")

# =============================================================================
# END OF PIPELINE
# =============================================================================
cat("\n==============================================================\n")
cat("Pipeline complete.\n\n")
cat("Outputs:\n")
cat("  analysis_dataset_final.csv     — scored analytical dataset\n")
cat("  analysis_dataset_final_v2.csv  — + tangibility variable\n")
cat("  score_checkpoint.csv           — scoring cache\n")
cat("  regression_models.rds          — all 16 model objects (A1-G4)\n")
cat("  fig1-fig9 (.pdf + .png)        — all thesis figures\n")
cat(sprintf("  Final analytical N: %d\n", nrow(analysis)))
cat("==============================================================\n")

