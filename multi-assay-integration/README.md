# Multi-assay integration

Analyses that combine family-level results from several SORMI assays.

## Code

- [`code/01-multi-assay-family-survival-prediction.Rmd`](code/01-multi-assay-family-survival-prediction.Rmd):
  integrates resazurin curve features, gill Na+/K+-ATPase and citrate synthase
  to look for the combination that best predicts family heat survival
  (`heat-survivorship` composite score). Uses exhaustive search for apparent
  fit, nested leave-one-family-out cross-validation and permutation tests for
  honest performance.

## Outputs

Results tables are written to `outputs/<code file name>/`.
