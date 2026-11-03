# Reproducing the experiments

All commands are run from the repository root. Stage 1 must finish before stage 2, and stage 2 before
stages 3 and 4; each stage reads the previous one's files from `output/<dataset>/`.

Every stage reseeds from `configs/datasets.yml`, so a stage can be rerun on its own and will give the
same answer. Nothing depends on the state of an interactive session, and nothing depends on which
dataset was run before it.

## 1. One dataset

```bash
Rscript scripts/rank_features.R        --dataset sorlie
Rscript scripts/evaluate_classifiers.R --dataset sorlie
Rscript scripts/make_figure.R          --dataset sorlie
```

Writes, under `output/sorlie/`:

```text
ranking/sorlie_important_score.csv   # every gene's score under every filter
ranking/feature_rankings.csv         # the top 100 genes per method, in rank order
evaluation/sorlie_results.xlsx       # one sheet per classifier
evaluation/cv_results.rds            # the same tables, read by stage 3
figure/sorlie_results_micro.pdf      # the four-panel micro-F1 figure
```

Each workbook sheet has one column per method and ten rows, for the fractions 100% down to 10% of
that method's 100-gene shortlist. The values are micro-averaged F1, meaned over the five folds.

## 2. All datasets

```bash
for dataset in alon burczynski golub gordon gravier khan \
               pomeroy shipp sorlie su yeoh; do
    Rscript scripts/rank_features.R        --dataset "$dataset" &&
    Rscript scripts/evaluate_classifiers.R --dataset "$dataset" &&
    Rscript scripts/make_figure.R          --dataset "$dataset"
done
```

Stage 1 dominates the runtime. The RBR filter draws 5,000 prior and 5,000 posterior values per gene,
so its cost is linear in the number of genes surviving the cleaning step — which for microarray data
is most of them, often well over ten thousand. Expect hours per dataset. Lower `rbr_draws` in the
configuration for a quick check, but report only runs at 5,000.

The datasets are independent: each run loads, screens and splits its own dataset and writes to its
own directory, so a failed dataset does not affect the others and the loop can be resumed at any
point.

## 3. Options

### Hyperparameter search

`inner_folds` in `configs/datasets.yml` sets the number of folds used inside each training partition
to choose the mixing parameter and penalty for logistic regression, the neighbour count for KNN, the
split width for the random forest, and the cost and kernel width for the SVM. Lower it for a faster
run; the grids themselves are declared at the top of `R/models/classifiers.R`.

### `--prior paper | solved`

Which prior the RBR filter uses. The default, `paper`, is the specification the manuscript states:
bounds `s_1^2 = 1`, `s_2^2 = 4` giving `lambda0 = 0.5`, with `alpha0 = beta0 = 2` held fixed.
`solved` runs a Newton elicitation instead, on bounds of 4 and 16, giving `alpha0 = 6.4546`,
`beta0 = 26.8415` and `lambda0 = 0.25`.

```bash
Rscript scripts/rank_features.R --dataset khan --prior solved
```

A configuration file other than `configs/datasets.yml`:

```bash
Rscript scripts/rank_features.R --dataset khan --config configs/my_datasets.yml
```

A smaller shortlist, a cheaper Monte Carlo, or a different fold count: edit `defaults:` in the
configuration, or add a per-dataset override:

```yaml
data:
  khan:
    shortlist: 50
    rbr_draws: 1000
    cv_folds: 3
```

## 4. Adding a dataset

Any dataset in `datamicroarray` can be added by name alone — there is no file to place:

```yaml
data:
  nakayama: {}
```

`datamicroarray::describe_data()` lists what is available. The target must have at least two classes
and the features must be continuous: `clean_dataset()` drops every categorical column other than the
target.

To run on a dataset of your own instead, replace `load_dataset()` in `R/utils/preprocess.R`. It must
return a data frame of numeric features with a factor column named `y`; everything downstream is
independent of where the data came from.

## 4a. Comparing the methods

Once stage 2 has run for the datasets of interest:

```bash
Rscript scripts/compare_methods.R
Rscript scripts/compare_methods.R --datasets alon,golub,khan
```

With no `--datasets` it uses every dataset that has saved results. It writes
`output/comparisons/method_comparisons.xlsx`: one sheet of every comparison, and one
baseline-by-classifier table of p-values per dataset.

## 5. Which command produces which result in the paper

| In the paper | Produced by |
|---|---|
| Multiclass gene datasets table | `configs/datasets.yml` (nine entries, with sample and class counts in the comments) |
| Figures of micro F1 against features retained, one per dataset | `make_figure.R`, from stage 2 |
| Per-gene importance scores | `rank_features.R`, as `<dataset>_important_score.csv` |
| Wilcoxon signed-rank p-values and median paired gaps in pp | `compare_methods.R`, as `method_comparisons.xlsx` |
| Multiclass synthetic results table (Friedman) | not in this repository |
