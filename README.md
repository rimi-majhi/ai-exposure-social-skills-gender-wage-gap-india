# Empathy Premium, Generative AI Exposure, and the Gender Wage Gap in India

## Overview
This repository contains the research paper and R analysis pipeline examining how occupational social-skill intensity (O*NET) and potential Generative AI task exposure (ILO, 2025) relate to gender wage differentials among regular salaried workers in India using Periodic Labour Force Survey (PLFS) data (2021–22 and 2023–24).

## Repository Structure
- `paper/` — Working paper and appendices (`rimimajhi_genAIexposure.docx.pdf`)
- `scripts/` — Numbered R scripts (`01_*.R` through `23_*.R`) covering data auditing, crosswalk construction, survey design validation, regression modeling, and robustness/inference checks
- `data_reference/` — Occupational crosswalks, CPI files, and reference exposure tables (subject to source terms and file-size limits)

## Data Sources
- **Labour Force Microdata:** PLFS (2021–22 and 2023–24), regular salaried workers (`cws_code == "31"`)
- **Occupational Skills & Classifications:** O*NET database, SOC-2010 to ISCO-08 crosswalks, ISCO-08, and NCO-2015 concordance materials
- **AI Exposure & Deflators:** ILO (2025) / Gmyrek et al. (2025) ISCO-08 Generative AI exposure scores and CPI series

## Notes on Reproducibility
- Several scripts were developed iteratively across local environments; if running locally, replace any machine-specific working directory paths with project-relative paths (`here::here()`).
- Large raw survey files and bulky reference archives are excluded from version control.

## Author
Rimi Majhi (Integrated B.Sc.–M.Sc. Economics, DTU)
