# Does Planning Quality Matter? Results Frameworks, Adaptive Design, and Development Outcomes in World Bank Projects in South Asia

**Sargam Kuckreja**
Master's Thesis in Economic Development, Lund University, May 2026
Supervisor: Erik Green

---

## Overview

This repository contains the data, code, and figures for my master's thesis, which examines whether the quality of World Bank project planning documents, measured at appraisal, predicts the development outcomes those projects eventually achieve. Using a sample of 1,110 Investment Project Financing operations in South Asia approved between 1958 and 2021, the thesis constructs quantitative text scores operationalising three competing theoretical frameworks — Results-Based Management, Problem-Driven Iterative Adaptation, and the isomorphic mimicry hypothesis — and tests their predictive power against Independent Evaluation Group outcome ratings across sixteen regression models.

---

## Repository Structure

```
does-planning-quality-matter/
│
├── README.md
├── code/
│   ├── master_analysis.R        # Main analysis script: scoring, cleaning, all regressions
│   └── rescore_specificity.R    # Supplementary script: specificity score recalculation
│
├── data/
│   ├── raw/
│   │   └── ieg_world_bank_project_performance_ratings_03-24-2026.csv   # Raw IEG ratings
│   └── processed/
│       ├── analysis_dataset_final_v2.csv    # Final analytical dataset used in all models
│       └── url_checkpoint.csv               # Document retrieval log
│
└── figures/
    ├── fig1_projects_by_country.png
    ├── fig2_projects_by_sector.png
    ├── fig3_projects_by_decade.png
    ├── fig4_outcome_distribution.png
    ├── fig5_outcome_by_country.png
    ├── fig6_outcome_by_sector.png
    ├── fig7_outcome_over_time.png
    ├── fig8_outcome_by_tangibility.png
    └── fig9_theory_scores_by_tangibility.png
```

---

## Data Sources

**IEG Project Performance Ratings Database**
World Bank Independent Evaluation Group outcome ratings for all completed Bank-financed operations. Accessed March 2026 via the World Bank Data Catalog.
Available at: https://financesone.worldbank.org/ieg-world-bank-project-performance-ratings/DS00053 

**Project Appraisal Documents and Staff Appraisal Reports**
Planning documents for all World Bank Investment Project Financing operations. Publicly available via the World Bank Documents and Reports portal.
Available at: https://documents.worldbank.org

Note: The raw PDF planning documents are not included in this repository due to file size. They are publicly available at the link above and can be retrieved using the URL checkpoint file provided in `data/processed/url_checkpoint.csv`.

---

## How to Reproduce the Analysis

Run the scripts in the following order:

1. `rescore_specificity.R` — recalculates the specificity score
2. `master_analysis.R` — runs all text scoring, data cleaning, merging, and the full set of sixteen regression models, and produces all figures and tables

All scripts are written in R. The following packages are required:

```r
install.packages(c("tidyverse", "pdftools", "stringr", "MASS", 
                   "sandwich", "lmtest", "ggplot2", "stargazer"))
```

---

## Key Variables

| Variable | Description |
|---|---|
| `ieg_outcome` | IEG outcome rating (1–6, Highly Unsatisfactory to Highly Satisfactory) |
| `rbm_score` | Standardised Results-Based Management text score |
| `pdia_score` | Standardised Problem-Driven Iterative Adaptation text score |
| `spec_score` | Standardised context specificity text score |
| `tangible` | Binary indicator: 1 = tangible project, 0 = intangible |
| `pad_era` | Binary indicator: 1 = post-1995 Project Appraisal Document |
| `country` | Recipient country (8 South Asian countries) |
| `sector` | World Bank Global Practice sector classification (15 categories) |
| `approval_decade` | Decade of project approval (1950s–2020s) |

---

## Main Findings

- Results-based management infrastructure at appraisal is a positive and significant predictor of IEG outcome ratings, holding across all sixteen models, sectors, and project types.
- Adaptive design language (PDIA) shows no predictive power in any model, suggesting adaptive management operates during implementation rather than at the design stage.
- Context specificity predicts better outcomes only in the Governance sector, where institutional diagnosis is most consequential.
- Tangibility shapes baseline difficulty but does not moderate the relationship between planning quality and outcomes.
- Planning quality explains approximately 6% of outcome variance; country institutions and implementation conditions remain the dominant determinants of project success.

---

## Citation

Kuckreja, S. (2026) *Does Planning Quality Matter? Results Frameworks, Adaptive Design, and Development Outcomes in World Bank Projects in South Asia.* Master's Thesis, Lund University.

---

## Contact

Sargam Kuckreja
sa3230ku-s@student.lu.se
