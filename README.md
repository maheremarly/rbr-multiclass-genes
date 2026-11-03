# A Bayesian Feature Selection Method for Multiclass Classification with Application to Cancer Gene Expression Data

Maher Emarly, Ayman Alzaatreh, Luai Al-Labadi

Preprint · [doi:10.21203/rs.3.rs-9106857/v1](https://doi.org/10.21203/rs.3.rs-9106857/v1) · under review

Microarray studies measure tens of thousands of genes on a few dozen patients, and most of those
genes say nothing about which tumour a patient has. This repository extends the relative belief ratio
filter from two classes to k: each gene is scored by the Bayesian evidence against equality of its
class-conditional means, where the discrepancy is the distance from each class mean to their
precision-weighted pool. The resulting *strength* ranks the genes, and the ranking is benchmarked
against information gain, symmetric uncertainty, mRMR and ReliefF on nine cancer microarray
datasets, scored by micro-averaged F1 under five-fold cross-validation.

## Repository structure

```text
.
├── configs
│   └── datasets.yml                # Dataset list, seeds and run settings
├── docs
│   └── reproducing.md              # Commands for every stage, for every dataset
├── R
│   ├── filters
│   │   ├── prior.R                 # The conjugate normal-gamma prior, as the paper specifies it
│   │   ├── rbr.R                   # k-class relative belief ratio filter and its Monte Carlo
│   │   └── baselines.R             # Information gain, symmetric uncertainty, mRMR, ReliefF
│   ├── models
│   │   └── classifiers.R           # The four classifiers, tuned, and the subset-size harness
│   └── utils
│       ├── setup.R                 # Packages, module loading, seeding
│       ├── args.R                  # Command line arguments for scripts/
│       ├── config.R                # configs/datasets.yml to a resolved run configuration
│       ├── preprocess.R            # ANOVA screen, cleaning and the stratified half-split
│       ├── rankings.R              # Score columns and sort direction, one spec per method
│       ├── metrics.R               # Micro F1, results table layout, fold averaging
│       ├── comparisons.R           # Paired Wilcoxon tests and median gaps
│       └── plots.R                 # The four-panel figure
├── scripts
│   ├── rank_features.R             # Stage 1: filter scores and the shortlist of 100 genes
│   ├── evaluate_classifiers.R      # Stage 2: cross-validated micro F1
│   ├── make_figure.R               # Stage 3: the paper's figure
│   └── compare_methods.R           # Stage 4: RBR against each baseline
├── .gitignore
├── DESCRIPTION
├── LICENSE
└── README.md
```

Running the pipeline adds `output/<dataset>/{ranking,evaluation,figure}/` (scores, shortlists,
workbooks and figures) and `output/comparisons/` (the paired tests).

## Installation

Tested with R 4.6.1 on macOS and R 4.3 on Windows 10.

```r
install.packages(c(
  "yaml", "dplyr", "FSelector", "praznik", "CORElearn", "caret",
  "glmnet", "class", "e1071", "randomForest", "abind", "reshape2",
  "ggplot2", "gridExtra", "scales", "openxlsx", "remotes"
))
remotes::install_github("ramhiser/datamicroarray")
```

Two things are not a plain `install.packages` away:

- **`datamicroarray`** is not on CRAN. It carries the gene-expression datasets and is installed from
  GitHub as above.
- **Java 8 or newer**, which `FSelector` needs through `RWeka` for information gain and symmetric
  uncertainty. On macOS, `brew install --cask temurin`; on Linux, your distribution's `default-jdk`;
  on Windows, a JDK with `JAVA_HOME` pointing at it. Run `R CMD javareconf` afterwards if `rJava`
  still fails to load.

## Data

All datasets come from [`datamicroarray`](https://github.com/ramhiser/datamicroarray), which collects
published cancer microarray studies as an expression matrix plus a class vector. Nothing is
downloaded at run time and nothing is redistributed here; `configs/datasets.yml` lists the eleven
datasets the study produced figures for, and `datamicroarray::describe_data()` gives each one's
source, sample count and class count.

No file paths are involved: `--dataset sorlie` loads `sorlie` from the package.

## Usage

Run from the repository root. For each dataset, three stages:

1. Score and rank the genes (`rank_features.R`). Orders the genes by FDR-adjusted ANOVA p-value,
   cleans, splits into stratified halves, elicits the prior, scores every gene with every filter
   on the ranking half, and shortlists the top 100 per method.
2. Cross-validate the shortlists (`evaluate_classifiers.R`). For each method and each of the ten
   retained fractions of its shortlist, fits elastic-net logistic regression, k-nearest neighbours, a
   random forest and an SVM on the evaluation half, scoring micro-averaged F1 over five folds. Each
   model's hyperparameters are chosen by a further cross-validation inside the training partition.
3. Draw the figure (`make_figure.R`). Micro F1 against fraction of the shortlist retained, one panel
   per classifier.
4. Compare the methods (`compare_methods.R`). Two-sided Wilcoxon signed-rank tests of RBR against
   each baseline over the ten subset sizes, with the median paired gap in percentage points.

```bash
Rscript scripts/rank_features.R        --dataset sorlie
Rscript scripts/evaluate_classifiers.R --dataset sorlie
Rscript scripts/make_figure.R          --dataset sorlie
Rscript scripts/compare_methods.R
```

The RBR filter is the expensive stage: 5,000 prior and 5,000 posterior draws per gene, reseeded per
gene so that every gene sees the same prior sample. The full commands for every dataset are in
[docs/reproducing.md](docs/reproducing.md).

## Acknowledgements

This work was carried out at the Department of Mathematics and Statistics, American University of
Sharjah, with the Department of Mathematical and Computational Sciences, University of Toronto
Mississauga, and was supported by the American University of Sharjah under fund FRG22-C-S58. It
extends the two-class filter of Alzaatreh, Emarly, Mitra and Al-Labadi (Physica Scripta 100(8),
086010, 2025), whose code is in [rbr-binary-fs](../rbr-binary-fs). The relative belief ratio machinery follows Evans' work on
Bayesian measures of evidence.

## License

MIT; see [LICENSE](LICENSE).

## Citation

```bibtex
@misc{Emarly_Multiclass_RBR,
  author       = {Emarly, Maher and Alzaatreh, Ayman and Al-Labadi, Luai},
  title        = {A Bayesian feature selection method for multiclass classification
                  with application to cancer gene expression data},
  howpublished = {Preprint, under review},
  doi          = {10.21203/rs.3.rs-9106857/v1}
}
```
