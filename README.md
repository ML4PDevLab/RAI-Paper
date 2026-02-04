# rai-activity-intro

This repo contains the replication data and files for *Foreign Influence by Authoritarian Governments: Introducing New Data and Evidence*. 

## Writing

- The `writing/full_paper.qmd` document contains the manuscript and the bulk of the analysis. The `writing/appendix.qmd` document contains the Supplementary Materials. 
- The `writing/output` subfolder contains output (tables, figures, and data) from R code in the `writing/full_paper.qmd` document.
- The `writing/mermaid_output.qmd` write-out a png that visualizes the data production pipeline. Note: this relies on webshot and ghostscript, so may not run on all machines.

## Updating Data

- Run `code/refresh_rai_from_mlp_intro.R` to build `writing/output/rai_latest.csv` from the `mlp-data-intro` pipeline output and copy `data/rai_vars.csv` into this repo.

## Deriving and Testing Expectations of Russian Influence Behavior
## AI-assisted Qualitative Case Studies

- Analysis in `code/validation/changepoint_gpt/`
- `gpt_summary.csv` is output from `full_paper.qmd`. The file identifies changepoint months, for which articles are extracted from the DB and fed to GPT for summarization.
- `event_prompts.csv` contains the prompts used to solicit GPT summaries.
- `event_summaries_gpt_summary.csv` contains the results, as well as hand-coding (by Jeremy)

## Increased Russian Activity

- `writing/output/dfpr.csv` includes change in Russian influence; used in the push/pull analysis
- The `code/push_factors.R` script contains the code to conduct the push-pull factors analysis. Currently, this relies on a local file (`AgreementScoresAll_Sep2023.csv`) stored outside of the repo. This file is too large to store in the repo, but can be retrieved from [this dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LEJUQZ).

## Appendix

- The `writing/keywords` subfolder contains spreadsheets for keywords designed to isolate foreign influence attributed to Russian and China, respectively.













