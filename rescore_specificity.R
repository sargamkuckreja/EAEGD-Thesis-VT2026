# ============================================================================
# rescore_specificity.R
#
# Splits the SPECIFICITY_score into two independent components:
#   SPECIFIC_density  — context-specific content per 1000 words
#   GENERIC_density   — donor boilerplate per 1000 words
#
# Both are included separately in regression, which is more
# theoretically transparent and avoids the ceiling problem
# in the ratio-based score.
#
# Run from your working directory alongside analysis_dataset_final.csv
# and the pdfs/ folder.
# ============================================================================
setwd("~/Desktop/EAEGD/THESIS‼️/Analysis 2")
library(readr); library(dplyr); library(stringr); library(pdftools); library(MASS)

df <- read_csv("analysis_dataset_final.csv", show_col_types=FALSE) %>%
  mutate(
    decade      = factor((approval_fy %/% 10) * 10),
    outcome_ord = factor(outcome_score, levels=1:6, ordered=TRUE),
    RBM_z       = as.numeric(scale(RBM_score)),
    PDIA_z      = as.numeric(scale(PDIA_score)),
    sector      = relevel(factor(sector),  ref="Education"),
    country     = relevel(factor(country), ref="India"),
    doc_era     = relevel(factor(doc_era), ref="SAR")
  )

pdf_dir <- "pdfs"

extract_text <- function(project_id) {
  path <- file.path(pdf_dir, paste0(project_id, ".pdf"))
  if (!file.exists(path)) return(NA_character_)
  tryCatch({
    pages <- pdf_text(path)
    paste(pages, collapse="\n") %>% str_squish() %>% str_to_lower()
  }, error=function(e) NA_character_)
}

# ── Specific keyword patterns ─────────────────────────────────────────────────
specific_patterns <- c(
  # Named institutions
  "ministry of","department of","directorate of","bureau of",
  "authority of","board of","national commission","national agency",
  "national authority","national institute","national council",
  "national bank","national fund","provincial","district office",
  "district government","district authority","municipality",
  "panchayat","upazila","tehsil","gram sabha",
  # Places
  "district of","province of","region of","state of","division of",
  "thana","taluk","mandal","ward","village of","town of",
  # Empirical references
  "census","household survey","national survey","national statistic",
  "percent of the population","percent of household",
  "percent of farmer","percent of women","percent of children",
  # Specific beneficiaries
  "smallholder farmer","marginal farmer","landless","ultra.poor",
  "female.headed household","scheduled caste","scheduled tribe",
  "indigenous people","indigenous community","pastoralist",
  "artisan","fisherfolk"
)

# ── Generic boilerplate patterns ──────────────────────────────────────────────
generic_patterns <- c(
  "best practice","international standard","global experience",
  "lessons from","in line with","consistent with","aligned with",
  "in accordance with","world class","state of the art",
  "capacity building","institutional strengthening","good governance",
  "gender mainstreaming","inclusive growth","holistic approach",
  "integrated approach","sustainable development","cross.cutting",
  "participatory approach","multi.stakeholder","evidence.based"
)

# ── Rescoring loop ────────────────────────────────────────────────────────────
ck_file <- "spec_rescore_checkpoint.csv"
scores  <- df %>% dplyr::select(project_id) %>%
  mutate(specific_density=NA_real_, generic_density=NA_real_)

if (file.exists(ck_file)) {
  ck <- read_csv(ck_file, show_col_types=FALSE)
  scores <- scores %>%
    left_join(ck, by="project_id", suffix=c("",".ck")) %>%
    mutate(
      specific_density = ifelse(is.na(specific_density),
                                specific_density.ck, specific_density),
      generic_density  = ifelse(is.na(generic_density),
                                generic_density.ck,  generic_density)
    ) %>%
    dplyr::select(project_id, specific_density, generic_density)
  cat(sprintf("Checkpoint: %d already scored\n",
              sum(!is.na(scores$specific_density))))
}

for (i in seq_len(nrow(scores))) {
  if (!is.na(scores$specific_density[i])) next
  text <- extract_text(scores$project_id[i])
  if (is.na(text) || nchar(text) < 300) next
  
  word_count <- max(str_count(text, "\\S+"), 1)
  
  # Count each pattern occurrence
  spec_count <- sum(sapply(specific_patterns,
                           function(p) str_count(text, p)))
  gen_count  <- sum(sapply(generic_patterns,
                           function(p) str_count(text, p)))
  
  # Density: counts per 1000 words, then sigmoid normalise
  spec_raw <- spec_count / (word_count / 1000)
  gen_raw  <- gen_count  / (word_count / 1000)
  
  # Sigmoid: 1 - exp(-x/k), where k controls steepness
  # k=5 for specific (meaningful range 0–50 per 1000 words)
  # k=3 for generic (meaningful range 0–30 per 1000 words)
  scores$specific_density[i] <- round(1 - exp(-spec_raw / 5),  4)
  scores$generic_density[i]  <- round(1 - exp(-gen_raw  / 3),  4)
  
  if (i %% 100 == 0) {
    cat(sprintf("  Scored %d / %d\n", i, nrow(scores)))
    write_csv(scores[!is.na(scores$specific_density),], ck_file)
  }
}
write_csv(scores, ck_file)
cat(sprintf("Scored: %d projects\n", sum(!is.na(scores$specific_density))))

# ── Diagnostics ───────────────────────────────────────────────────────────────
df2 <- df %>% left_join(scores, by="project_id") %>%
  filter(!is.na(specific_density)) %>%
  mutate(
    SPEC_density_z  = as.numeric(scale(specific_density)),
    GENERIC_density_z = as.numeric(scale(generic_density))
  )

cat("\n── New score distributions ──────────────────────────────────\n")
cat("SPECIFIC_density (more = better contextual grounding):\n")
print(summary(df2$specific_density))
cat(sprintf("  %% scoring > 0.95: %.1f%%\n",
            mean(df2$specific_density > 0.95) * 100))
cat("\nGENERIC_density (more = more boilerplate):\n")
print(summary(df2$generic_density))
cat(sprintf("  %% scoring > 0.95: %.1f%%\n",
            mean(df2$generic_density > 0.95) * 100))

cat("\nCorrelations with outcome score:\n")
cat(sprintf("  SPECIFIC_density:  r = %.4f\n",
            cor(df2$specific_density, df2$outcome_score, use="pairwise")))
cat(sprintf("  GENERIC_density:   r = %.4f\n",
            cor(df2$generic_density, df2$outcome_score, use="pairwise")))
cat(sprintf("  Old SPEC ratio:    r = %.4f\n",
            cor(df2$SPECIFICITY_score, df2$outcome_score, use="pairwise")))

# ── Regression with split scores ──────────────────────────────────────────────
cat("\n── OLS with split specificity scores ────────────────────────\n")
m_split <- lm(outcome_score ~
                RBM_z + PDIA_z +
                SPEC_density_z + GENERIC_density_z +
                sector + country + decade + doc_era,
              data=df2)

s <- summary(m_split)$coefficients
for (term in c("RBM_z","PDIA_z","SPEC_density_z","GENERIC_density_z")) {
  r <- s[term,]
  stars <- ifelse(r[4]<.001,"***",ifelse(r[4]<.01,"**",
                                         ifelse(r[4]<.05,"*",ifelse(r[4]<.1,".","  "))))
  cat(sprintf("  %-25s  beta=%+.3f  se=%.3f  p=%.3f  %s\n",
              term, r[1], r[2], r[4], stars))
}

cat("\nInterpretation guide:\n")
cat("  SPEC_density_z significant and POSITIVE → more contextual\n")
cat("    language predicts better outcomes (supports mimicry theory)\n")
cat("  GENERIC_density_z significant and NEGATIVE → more boilerplate\n")
cat("    predicts worse outcomes (supports mimicry theory)\n")
cat("  Neither significant → mimicry hypothesis not supported\n")
cat("    in aggregate (consistent with original finding)\n")

write_csv(df2 %>% dplyr::select(project_id, specific_density,
                                generic_density, SPEC_density_z,
                                GENERIC_density_z),
          "specificity_rescored.csv")
cat("\nSaved: specificity_rescored.csv\n")
