# Florida Scrub-Jay waxworm color preference

Analysis code and data accompanying *Learned Avoidance of Aposematic Prey in Florida Scrub-Jays (Aphelocoma coerulescens)*.

## Files

- `FINAL_ANALYSIS_PLOTS.R`: sample counts, two chi-squared tests, effect sizes, consumption bar plots, and relative depletion curves.
- `final_data.csv`: supplied analysis dataset, read directly by the script.
- `waxworm_trial.csv`: original field-record table, retained as supporting source data. It is not read by the analysis script and can be renamed independently.
- `Florida_Scrub_Jay.Rproj`: RStudio project configured not to restore or save workspace state.

## Run the analysis

Install R and the required packages. The script uses the native pipe operator and requires R 4.1 or later.

```r
install.packages(c("dplyr", "tidyr", "tibble", "flextable", "ggplot2", "ggpattern", "patchwork"))
```

Open `Florida_Scrub_Jay.Rproj` in RStudio and run:

```r
source("FINAL_ANALYSIS_PLOTS.R")
```

Alternatively, open a terminal in this folder and run:

```sh
Rscript --vanilla FINAL_ANALYSIS_PLOTS.R
```

The script creates `outputs/` with sample counts, Table 1 as CSV and HTML, contingency tables, Figure 2 plotting data, both figures as PNG and PDF, and R session information. Generated outputs are excluded from Git by default.

## Analysis definitions

The analysis uses the supplied age and personality classifications. Tests compare consumed-worm color counts between old and young birds and between bold and shy birds. Both tests explicitly use Yates's continuity correction. Phi, Cramer's V, and the contingency coefficient retain the manuscript script's calculations from the corrected statistic.

Missing bird IDs on `event = 0` rows are intentional placeholders for uneaten worms. They are excluded from individual bird counts. Only `event = 1` rows contribute to consumption tests and figures. The supplied data contain 68 identified birds: 53 old and 15 young; 60 have personality classifications (40 bold and 20 shy).

Figure 2 shows a relative depletion index for each color within each group:

```text
100 * (1 - cumulative consumption of that color / total consumption of both colors in the group)
```

This is a descriptive index, not the percentage of the original offered worms remaining. No formal consumption-order or rate test is performed.

## Data columns

| `final_data.csv` column | Meaning |
|---|---|
| `Trial.id` | Trial identifier as supplied |
| `Jay.ID` | Bird identifier; missing for uneaten-worm placeholders |
| `worm_color` | Green or Orange (orange/black treatment) |
| `worm_number` | Worm sequence identifier within trial and color |
| `Trial.time` | Recorded event or assigned censoring time, in seconds |
| `event` | 1 for a consumption record; 0 for an uneaten-worm placeholder |
| `Age` | Supplied numeric age |
| `Age_bin1` | Old/young classification used in the manuscript |
| `Age_bin2` | Alternate supplied age classification, unused by this script |
| `personality` | Bold/shy classification, missing when unavailable |

The raw file contains territory, date, trial and time fields, bird IDs, Orange/Green entries, and Drop/Wash annotations. Trial metadata appear in header rows with observation rows below. This repository reproduces analysis from the supplied `final_data.csv`; it does not provide a raw-to-final cleaning pipeline or establish that the raw and final files reconcile exactly. Both data files are preserved without alteration.

## Upload to GitHub

Use this folder as the repository root. Upload its files, including `.gitignore` and `.gitattributes`. Do not upload the parent workspace, R session files, or package libraries. If you rename `waxworm_trial.csv`, update its name in this README; the analysis code does not need changing.

No software or data license has been assigned in this preparation. Add the authors' chosen license and final manuscript citation when available.
