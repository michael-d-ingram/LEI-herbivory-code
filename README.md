[README.md](https://github.com/user-attachments/files/34558623/README.md)
# LEI herbivory code

R code for the statistical analyses and figures in:

Ingram, M. D., et al. *Nitrate-enriched groundwater discharge and ecosystem response on a tropical coral reef island.* Manuscript LO-25-0913, in review at *Limnology and Oceanography*.

The analyses cover two things: seasonal and spatial variation in δ¹⁵N of corals and benthic microalgae on the reef flats of Lady Elliot Island, southern Great Barrier Reef, and a herbivore-exclusion experiment testing whether grazing prevents algal accumulation under nitrate-enriched groundwater discharge.

## Scripts

| Script | Analysis |
| --- | --- |
| `01_d15N_reef_flats_LMM.R` | Site-level linear mixed model of coral δ¹⁵N, `lmer(d15N ~ Season + Site + (1\|Year))`, with Type II ANOVA, estimated marginal means, Tukey contrasts among years, and the reef-flat comparison plot |
| `01b_d15N_reef_flats_LMM_BMA.R` | The same model for benthic microalgae δ¹⁵N |
| `02_d15N_western_coral_LMM.R` | Western reef flat coral δ¹⁵N, `lmer(d15N ~ Season + (1\|Year))`, with model diagnostics and a one-way ANOVA on year |
| `03_d15N_western_BMA_LMM.R` | The same model for western reef flat benthic microalgae |
| `04_chlorophyll_two_way_ANOVA.R` | Early two-treatment chlorophyll model, caged versus exposed only |
| `05_chlorophyll_within_site_ANOVA.R` | Exploratory one-way ANOVAs of treatment within each reef flat separately |
| `06_rerun_all_models.R` | Verification re-run of every δ¹⁵N model, including a sensitivity test that removes duplicated February 2021 records, and a printout of package versions |
| `07_chlorophyll_three_treatment_ANOVA.R` | Three-treatment chlorophyll model including the semi-caged cage control |
| `08_chlorophyll_model_and_figure.R` | Final chlorophyll analysis and figure |

### Which scripts produced the published results

The δ¹⁵N results in the manuscript come from `01`, `01b`, `02` and `03`, with `06` as an independent re-run of the same models.

The chlorophyll results come from `08`. That script fits one three-treatment model crossed with reef flat and replaces the earlier split approach in `04`, `05` and `07`, where the caged versus exposed contrast came from a two-treatment model and the cage-control contrasts from a separate three-treatment model, giving two different p-values for the same comparison. Scripts `04`, `05` and `07` are kept for transparency about how the analysis developed. They are not the source of any number in the manuscript.

## Data

The data files are not included in this repository.

Reef-organism δ¹⁵N data from the companion study are available at https://doi.org/10.25918/data.331.

The δ¹⁵N, water chemistry, reef metabolism and chlorophyll data supporting this manuscript will be deposited in a public repository with a DOI on acceptance, and this README will be updated with that DOI.

The scripts read tab-delimited text files:

| File | Used by |
| --- | --- |
| `15N_coral_site_linearmodel.txt` | `01`, `06` |
| `15N_BMA_site_linearmodel.txt` | `01b`, `06` |
| `15N_coral_linearmodel.txt` | `02`, `06` |
| `15N_BMA_linearmodel.txt` | `03`, `06` |
| `chl_corr.txt` | `05`, `07`, `08` |
| `chl_final.txt` | `04` |

Column names are as follows. The δ¹⁵N files hold `d15N`, `Season`, `Site` and `Year`. The chlorophyll files hold `chl`, `Site` and `Treatment`, where `Site` is `C` for the western reef flat and `A` for the eastern reef flat, and `Treatment` is `C` for caged, `S` for semi-caged and `O` for exposed. The site codes `C` and `A` correspond to the quadrants formerly labelled D and B.

## Requirements

R, with:

```r
install.packages(c("tidyverse", "lme4", "lmerTest", "emmeans", "car", "ggplot2", "dplyr"))
```

Degrees of freedom for the mixed models are Satterthwaite approximations from `lmerTest`, so results depend on that package being loaded. `06_rerun_all_models.R` prints the package versions used.

## Running the scripts

File paths are hard-coded and will need editing. Scripts `01`, `01b`, `02` and `03` begin with a `setwd()` call pointing at a local directory, and `06` reads from absolute paths on a different machine again. Either edit those lines or place the data files in the working directory and delete the `setwd()` calls. `08` expects the chlorophyll data at `data/03_chlorophyll_tiles/chl_corr.txt` and writes to an `outputs/` directory, which needs to exist.

The scripts were run interactively rather than as a pipeline, so run them one at a time rather than sourcing them in sequence.

## Citation

If you use this code, please cite the paper above and this repository:

Ingram, M. D. (2026). *LEI herbivory code.* GitHub. https://github.com/michael-d-ingram/lei-herbivory-code

## Contact

Michael Ingram, Southern Cross University
ORCID: https://orcid.org/0009-0009-3956-0826
