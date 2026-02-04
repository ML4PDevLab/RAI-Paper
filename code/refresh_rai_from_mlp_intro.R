# Build a paper-ready RAI dataset from the mlp-data-intro pipeline output.
# - Reads full-rai-data.csv and rai_vars.csv from the sibling repo.
# - Computes article_total and theme-level rai_idx_* columns (per 10k articles).
# - Writes a paper-ready CSV into writing/output/ and a copy of rai_vars.csv into data/.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})

input_data <- here::here("..", "mlp-data-intro", "data", "final-counts", "full-rai-data.csv")
input_vars <- here::here("..", "mlp-data-intro", "data", "rai_vars.csv")

output_data <- here::here("writing", "output", "rai_latest.csv")
output_vars <- here::here("data", "rai_vars.csv")

if (!file.exists(input_data)) {
  stop("Missing input data file: ", input_data)
}
if (!file.exists(input_vars)) {
  stop("Missing rai_vars file: ", input_vars)
}

message("Reading: ", input_data)
rai_raw <- readr::read_csv(input_data, show_col_types = FALSE)

message("Reading: ", input_vars)
rai_vars_raw <- readr::read_csv(input_vars, show_col_types = FALSE)

# Standardize influencer names to match existing paper code expectations
rai_raw <- rai_raw %>%
  mutate(
    influencer = dplyr::recode(
      influencer,
      "russia" = "Russia",
      "china" = "China",
      "combined" = "Combined",
      .default = influencer
    )
  )

# Standardize column name to match older code expectations
rai_vars <- rai_vars_raw %>%
  rename(variable = id)

# Update display names to the latest definitions and ensure optional categories exist
name_updates <- c(
  arms_transfer_security_aid_assistance = "Arms Transfer/Security Aid/Assistance",
  bribery_economic_corruption = "Bribery/Economic Corruption",
  diplomatic_mediation = "Diplomatic Mediation",
  diplomatic_ties = "Diplomatic Ties",
  diplomatic_action = "Diplomatic Action",
  diplomatic_statement = "Diplomatic Statement",
  diplomatic_meeting = "Diplomatic Meeting",
  foreign_aid_assistance = "Foreign Aid/Assistance",
  intelligence_counterintelligence = "Intelligence/Counterintelligence",
  cyber_attack = "Cyber Attack",
  foreign_investment = "Foreign Investment",
  joint_security_force_exercise = "Joint Security Force Exercise",
  media_campaign_intervention = "Media Campaign/Intervention",
  political_process_policy_intervention = "Political Process/Policy Intervention",
  security_engagement = "Security Engagement",
  military_activity = "Military Activity",
  social_academic_cultural_activity = "Social/Academic/Cultural Activity",
  tech_transfer_investment = "Technology Transfer/Investment",
  trade_agreement_exchange = "Trade Agreement/Exchange",
  trade_financial_sanction = "Trade/Financial Sanction",
  transnational_organization_crime = "Transnational Organized Crime"
)

rai_vars <- rai_vars %>%
  mutate(name = if_else(variable %in% names(name_updates), name_updates[variable], name))

# Copy rai_vars.csv into this repo for reproducibility (with updated names)
if (!dir.exists(dirname(output_vars))) {
  dir.create(dirname(output_vars), recursive = TRUE)
}
rai_vars_out <- rai_vars %>%
  rename(id = variable)
readr::write_csv(rai_vars_out, output_vars)
message("Wrote: ", output_vars)

# Identify event variables present in the new data
event_vars <- intersect(rai_vars$variable, names(rai_raw))
missing_vars <- setdiff(rai_vars$variable, event_vars)
if (length(missing_vars) > 0) {
  warning("Missing event columns in input data: ", paste(missing_vars, collapse = ", "))
}

# Compute total articles (article_total)
if (all(c("-999", "-999Norm") %in% names(rai_raw))) {
  rai_raw <- rai_raw %>%
    mutate(article_total = if_else(`-999Norm` > 0, round(`-999` / `-999Norm`), NA_real_))
} else if ("article_total" %in% names(rai_raw)) {
  rai_raw <- rai_raw %>%
    mutate(article_total = as.numeric(article_total))
} else {
  stop("No way to compute article_total: expected -999 & -999Norm or article_total column.")
}

# Build per-10k event columns with old-style names
norm_cols <- paste0(event_vars, "Norm")
if (all(norm_cols %in% names(rai_raw))) {
  rai_events <- rai_raw %>%
    transmute(
      country,
      influencer,
      date,
      article_total,
      across(all_of(norm_cols), ~ .x * 10000, .names = "{str_remove(.col, 'Norm')}")
    )
} else {
  # Fallback: compute from raw counts
  rai_events <- rai_raw %>%
    transmute(
      country,
      influencer,
      date,
      article_total,
      across(all_of(event_vars), ~ (.x / article_total) * 10000)
    )
}

# If any expected event columns are missing, add as zeros
missing_cols <- setdiff(rai_vars$variable, names(rai_events))
if (length(missing_cols) > 0) {
  rai_events[missing_cols] <- 0
}

# Preserve the intended event column order
event_vars_order <- rai_vars$variable
event_vars_order <- event_vars_order[event_vars_order %in% names(rai_events)]

# Theme mapping
theme_vars <- split(rai_vars$variable, rai_vars$theme)
# Keep only present columns
theme_vars <- lapply(theme_vars, function(v) v[v %in% names(rai_events)])

# Assemble output with theme indices
rai_out <- rai_events %>%
  mutate(
    rai_idx_total = rowSums(dplyr::select(., all_of(event_vars_order)), na.rm = TRUE),
    rai_idx_bal = rowSums(dplyr::select(., all_of(theme_vars$bal)), na.rm = TRUE),
    rai_idx_di = rowSums(dplyr::select(., all_of(theme_vars$di)), na.rm = TRUE),
    rai_idx_dip = rowSums(dplyr::select(., all_of(theme_vars$dip)), na.rm = TRUE),
    rai_idx_ep = rowSums(dplyr::select(., all_of(theme_vars$ep)), na.rm = TRUE),
    rai_idx_hp = rowSums(dplyr::select(., all_of(theme_vars$hp)), na.rm = TRUE),
    rai_idx_sp = rowSums(dplyr::select(., all_of(theme_vars$sp)), na.rm = TRUE)
  ) %>%
  select(
    country,
    influencer,
    date,
    all_of(event_vars_order),
    article_total,
    rai_idx_total,
    rai_idx_bal,
    rai_idx_di,
    rai_idx_dip,
    rai_idx_ep,
    rai_idx_hp,
    rai_idx_sp
  )

# Write output
readr::write_csv(rai_out, output_data)
message("Wrote: ", output_data)
