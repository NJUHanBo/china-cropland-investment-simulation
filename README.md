# Linking driver analysis to scenario simulation for cropland investment in China

Code and county-level data for the article *Linking driver analysis to scenario simulation for cropland investment in China* (submitted to *The Innovation*).

The analysis estimates county-level effects of four cropland investment areas (conservation, landscape pattern, use intensity and soil–water conditions) on three dimensions of cropland productivity (change, stability and acceleration) in 1,560 Chinese counties, groups counties into seven response types, fits one Gaussian Bayesian network per type on a causal graph corrected by the effect estimates, and simulates 945 scenarios (15 investment portfolios × 21 spatial targeting strategies × 3 levels of external pressure).

## Requirements

R 4.4.1 with grf 2.6.1, bnlearn 5.0.2, cluster 2.1.6, mgcv 1.9.1, sensemakr 0.1.6, EValue, dplyr 1.1.4, tidyr 1.3.1, ggplot2 4.0.2, cowplot 1.1.3, patchwork 1.3.0, ggrepel 0.9.6, sf 1.1.0, ragg 1.3.3, svglite 2.1.3 and systemfonts 1.1.0. Figures use the Roboto font; install it system-wide or place the `.ttf` files from [Google Fonts](https://fonts.google.com/specimen/Roboto) in `fonts/`.

All scripts are run from the project root, e.g. `Rscript scripts/01_train_causal_forest.R`.

## Inputs

| File | Content |
|---|---|
| `data/basic_data.csv` | 1,560 counties: productivity dimensions (CPI, CPS, CPA, composite CPDS, rescaled to [0, 1]), agricultural zone, and 26 driver indicators (Table S1) |
| `data/CPI_20year_mean_by_county.csv` | Mean Crop Growth Index 2001–2020, used by two spatial targeting strategies |
| `models/stage3_factor_weights_detail.csv` | Indicator weights from the five-method machine-learning ensemble (Table S2) |
| `models/enhanced_network_results/detailed_edge_statistics.csv` | Links of the a priori causal graph (Table S6) |

County, province and South China Sea boundary files used for the maps (scripts 20, 26 and 27) are not redistributed. Place them in `boundaries/` (`county_shp.shp`, `province_shp2 2.shp`, `南海九段线.shp`, `南海诸岛.shp`) or edit the path at the top of those scripts.

## Files not included

Three outputs exceed GitHub's file-size limit and are regenerated exactly by the pipeline: `models/causal_forest_results/cf_models.RDS` (script 01; needed only by script 33), `output/csv/scenario_results_cluster_bn.RDS`/`.csv` (script 04) and `output/csv/tradeoff_county_level.csv` (script 05). All other model outputs and result tables used in the article are included.

## Pipeline

| Step | Script | Output | Used in |
|---|---|---|---|
| Smoke test | `00_test_pipeline.R` | console | — |
| Effect estimation | `01_train_causal_forest.R` | `models/causal_forest_results/` | Fig. 3, Tables S3–S4 |
| Response types | `02_cluster_counties.R` | `models/cluster_results/` | Fig. 2, Table S5 |
| Causal graph and networks | `03_build_cluster_bn.R` | `models/corrected_bn/` | Fig. 1, Table S6 |
| Scenario simulation | `04_scenario_simulation.R` | `output/csv/scenario_results_cluster_bn.RDS` | Figs 5–7 |
| Triple-gain classification | `05_tradeoff_analysis.R` | `output/csv/tradeoff_*.csv` | Figs 6–7 |
| National aggregation | `06_national_analysis.R` | `output/csv/national_effects_by_indicator.csv` | Figs 5, 7 |
| Robustness | `08a`–`08d`, `11_oster_evalue_sensitivity.R` | `output/csv/sensitivity/`, `output/csv/oster_evalue_sensitivity.csv` | Figs S2–S5, Tables S3, S9 |
| Model comparison | `13_ablation.R` | `output/csv/ablation_*.csv` | Fig. 4 |
| Variance decomposition, pressure gradient | `14_variance_and_pressure_sweep.R` | `output/csv/cate_variance_decomposition.csv`, `external_multiplier_sweep.csv` | Fig. 7A, Table S4 |
| Main figures | `20`–`26_fig_*.R` | `output/innov_figures/` | Figs 1–7 |
| Graphical abstract | `27_graphical_abstract.R` | `output/innov_figures/graphical_abstract.*` | — |
| Supplemental analyses | `30_si_analysis.R`, `32_interaction_pressure.R`, `33_si_checks.R` | `output/si/` | Tables S1–S9 |
| Supplemental figures | `31_si_figures.R` | `output/innov_figures/si/` | Figs S1–S7 |

Changes in all scenarios are measured from the model prediction under current conditions without additional investment (`lib/change_reference.R`), not from observed values.

## Library functions (`lib/`)

| File | Purpose |
|---|---|
| `ml_guided_factor_calculation.R` | Aggregates indicators into six dimension scores with the weights above |
| `causal_forest_functions.R` | Causal forest training and graph correction |
| `cluster_bn_functions.R` | Type-specific Bayesian networks and closed-form prediction |
| `scenario_functions.R` | Portfolio and spatial targeting definitions (Tables S7–S8) |
| `change_reference.R` | Model-baseline reference for scenario changes |
| `fig_style_innov.R` | Shared figure style |

## License

Code is released under the MIT License; data in `data/`, `models/` and `output/` under CC BY 4.0 (see `LICENSE`).

## Contact

Bo Han, hanbonju@nju.edu.cn
