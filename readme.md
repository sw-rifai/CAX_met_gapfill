## Ongoing ...

-   as of 2023-01-26

## To do:

-   ~~finish GEE exports~~

-   ~~reform imports~~

-   ~~realign w/met obs~~

-   re-fit models, or update models

    -   previous version used GAMs. Might be better to use ~~XGBoost~~ GBM
    -   Current strategy is apply h2o autoML with GBM. Fit (only) three models because the default hyperparams seem to work okay. Check the variable importance as a check to ensure it's scaling from something ..reasonable..

-   generate predictions, gapfill, export to parquet
