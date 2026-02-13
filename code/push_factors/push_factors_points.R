# This script analyzes the strategic factors that predict increases in Russian influence
# in 2022-2024 compared to 2017-2021. We look at UN Ideal Point distances, characteristics
# like distance and language, and trade factors
library(countrycode)
library(modelsummary)
library(tradestatistics)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(kableExtra)
library(flextable)
library(gt)

# Optional: set to TRUE if you want all model figures regenerated
generate_all_model_figures <- FALSE

save_gt_png <- function(gt_obj, filename, width = 2400, height = 1200, res = 200) {
  tmp_dir <- tempfile("gt_tex_")
  dir.create(tmp_dir)
  tmp_tex_fragment <- file.path(tmp_dir, "table_fragment.tex")
  tmp_tex <- file.path(tmp_dir, "table.tex")
  tmp_pdf <- file.path(tmp_dir, "table.pdf")
  tmp_pdf_cropped <- file.path(tmp_dir, "table_cropped.pdf")

  gt::gtsave(gt_obj, filename = tmp_tex_fragment)

  fragment <- readLines(tmp_tex_fragment, warn = FALSE)
  tex_doc <- c(
    "\\documentclass[landscape]{article}",
    "\\usepackage[margin=0.2in]{geometry}",
    "\\usepackage{longtable}",
    "\\usepackage{booktabs}",
    "\\usepackage{array}",
    "\\usepackage{colortbl}",
    "\\usepackage{xcolor}",
    "\\usepackage{graphicx}",
    "\\usepackage{multirow}",
    "\\usepackage{float}",
    "\\usepackage{caption}",
    "\\pagenumbering{gobble}",
    "\\begin{document}",
    "\\thispagestyle{empty}",
    "\\centering",
    "\\large",
    fragment,
    "\\end{document}"
  )
  writeLines(tex_doc, tmp_tex)

  tex_bin <- Sys.getenv("TEX_BIN", Sys.getenv("LUALATEX_BIN", "xelatex"))
  system2(tex_bin,
          c("-interaction=nonstopmode",
            "-halt-on-error",
            "-output-directory", tmp_dir,
            tmp_tex),
          stdout = FALSE, stderr = FALSE)

  if (!file.exists(tmp_pdf)) {
    log_file <- file.path(tmp_dir, "table.log")
    if (file.exists(log_file)) {
      log_lines <- readLines(log_file, warn = FALSE)
      message("LaTeX log excerpt:\n", paste(tail(log_lines, 40), collapse = "\n"))
    }
    stop("Failed to render gt table to PDF.")
  }

  # Crop excess whitespace if pdfcrop is available
  if (nzchar(Sys.which("pdfcrop"))) {
    system2("pdfcrop",
            c(tmp_pdf, tmp_pdf_cropped),
            stdout = FALSE, stderr = FALSE)
  }

  pdf_for_png <- if (file.exists(tmp_pdf_cropped)) tmp_pdf_cropped else tmp_pdf

  system2("pdftoppm",
          c("-singlefile", "-png", pdf_for_png,
            file.path(tmp_dir, "table")),
          stdout = FALSE, stderr = FALSE)

  tmp_png <- file.path(tmp_dir, "table.png")
  if (!file.exists(tmp_png)) {
    stop("Failed to render gt table PNG.")
  }

  file.copy(tmp_png, filename, overwrite = TRUE)
}


# Read-in gpt summary data ------------------------------------------------

dat = read_csv(here::here("code", "validation", "changepoint_gpt", "event_summaries_gpt_summary.csv")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         Period = case_when( Date < "2022-02-01" ~ "Aug-Jan",
                             Date == "2022-02-01" & Flag == "before invasion" ~ "Feb (Pre)",
                             Date == "2022-02-01" & Flag == "after invasion" ~ "Feb (Post)",
                             Date > "2022-02-01" ~ "Mar-Dec"),
         Period = factor(Period, levels = c("Aug-Jan", 
                                            "Feb (Pre)", "Feb (Post)",
                                            "Mar-Dec") ),
         substance = ifelse(Signal == "Nothing", "False Positive", "True Positive")) %>%
  rename(Accuracy = substance)


# number of articles

dat %>%
  filter(Date < "2022-02-01" & Date > "2021-07-01") %>%
  filter(Event %in% c("Diplomacy", "Hard Power", "Economic Power")) %>%
  summarise(count = sum(Num_Articles))




# Create frequency tables -------------------------------------------------

create_proc_freq_table <- function(dat, event_type) {
  proc_freq(dat %>% filter(Event %in% c(event_type)), "Period", "Accuracy", 
            include.row_percent = TRUE,
            include.column_percent = FALSE, 
            include.table_percent = FALSE) %>%
    fontsize(size = 18, part = "header") %>%
    fontsize(size = 16, part = "body") %>%
    labelizor(part = "header", labels = stringr::str_to_title) %>%
    height_all(height = 10) %>%
    width(j = 1, width = 2) %>%
    width(j = 2, width = 0.8) %>%
    width(j = 3, width = 1) %>%
    width(j = 4, width = 1)
}

# Example usage:
create_proc_freq_table(dat, "Diplomacy")
create_proc_freq_table(dat, "Hard Power")
create_proc_freq_table(dat, "Economic Power")



# Compare historical averages ---------------------------------------------

cp = read_csv(here::here("writing", "output", "change-points_plot.csv"))

# Define the end of the last six-month period and the start of the first period
end_period <- as.Date("2022-01-31")
start_period <- as.Date("2012-01-01")

# Create six-month periods, going backward from the defined end period
breaks_6m <- seq.Date(from = start_period, to = end_period, by = "6 months")
labels_6m <- breaks_6m[-length(breaks_6m)]

data <- cp %>%
  mutate(period = cut(date,
                      breaks = breaks_6m,
                      labels = labels_6m,
                      right = FALSE)) %>%
  mutate(period = as.Date(as.character(period)))


# Summarize the data by period
summary_data <- data %>%
  group_by(period, theme) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>%
  mutate(period = as.Date(period))

# Create a complete sequence of six-month periods
all_periods <- tibble(
  period = seq(from = start_period, to = end_period, by = "6 months")
)

# Merge the complete sequence with the summarized data
complete_summary <- all_periods %>%
  left_join(summary_data, by = "period") %>%
  mutate(total_count = ifelse(is.na(total_count), 0, total_count))



# Compare historical averages ---------------------------------------------

cp = read_csv(here::here("writing", "output", "change-points_plot.csv"))

# Define the six-month periods
periods <- data.frame(
  start_date = seq.Date(from = as.Date("2012-02-01"), to = as.Date("2021-08-01"), by = "6 months"),
  end_date = seq.Date(from = as.Date("2012-07-31"), to = as.Date("2022-01-31"), by = "6 months")
)

# Create a dataframe with all combinations of `theme` and periods
themes <- unique(cp$theme)
all_combinations <- expand.grid(theme = themes, periods = seq_len(nrow(periods)))

# Add period labels
periods$period_label <- seq_len(nrow(periods))
all_combinations <- all_combinations %>%
  left_join(periods %>% select(period_label, start_date, end_date), by = c("periods" = "period_label"))

# Assign data rows to their respective periods
data <- cp %>%
  filter(date < "2022-02-01") %>%
  mutate(period = findInterval(date, periods$start_date, rightmost.closed = TRUE)) %>%
  filter(period > 0) # Ensure dates outside periods are removed

# Aggregate counts for each period and theme
summarized_data <- data %>%
  group_by(theme, period) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = 'drop')

# Merge with all combinations to ensure every theme and period is represented
complete_data <- all_combinations %>%
  left_join(summarized_data, by = c("theme", "periods" = "period")) %>%
  mutate(total_count = ifelse(is.na(total_count), 0, total_count))

# Initialize an empty list to store models
models <- list()
outcomes = unique(complete_data$theme)

# Define the names of the models
model_names <- c('Backlash', 'Domestic Interference', 'Diplomacy', 'Economic Power', 'Hard Power', 'Soft Power')


complete_data = complete_data %>%
  mutate(final = case_when(periods == 20 ~ 1,
                           TRUE ~ 0))

# Loop through outcomes and model names to create the models
for (i in seq_along(outcomes)) {
  outcome <- outcomes[i]
  formula <- as.formula(paste("total_count", "~", "final"))
  model_name <- paste(model_names[i], sep = " - ")
  models[[model_name]] <- lm(formula, data = complete_data %>% filter(theme == outcome))
}


cp_tab <- modelsummary(
  models,
  coef_map = c('(Intercept)' = "Intercept",
               'final' = "Pre-Invasion"),
  estimate  = "{estimate}{stars} ({std.error})",
  #statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.',
  statistic = c("p.value"),
  output = "gt"
)
save_gt_png(cp_tab, here::here("writing", "figures", "change-point-regression.png"))

# Compare historical averages ---------------------------------------------

cp = read_csv(here::here("writing", "output", "change-points_plot.csv"))


cp = cp %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         period = case_when( date > "2021-07-01" & date < "2022-02-01" ~ "Aug-Jan",
                             date > "2017-07-01" & date < "2021-08-01" ~ "2017-2021",
                             TRUE ~ "2012-2017"))

cp_sum = cp %>%
  group_by(period, theme) %>%
  summarise( count = sum(count) ) %>%
  ungroup() %>%
  mutate(months = case_when( period == "Aug-Jan" ~ 6,
                             period == "2017-2021" ~ 60,
                             period == "2012-2017" ~ 55)) %>%
  mutate(years = months / 12,
         count_py = count / years) %>%
  select(period, theme, count, count_py) %>%
  arrange(theme)



# Isolate pre-invasion cp months ------------------------------------------

tdat = dat %>%
  filter(Period %in% c("Aug-Jan", "Feb (Pre)") ) %>%
  mutate(ccode1 = countrycode(Country, "country.name", "cown")) %>%
  mutate(ccode1 = case_when(Country == "Serbia" ~ 345,
                            TRUE ~ ccode1)) %>%
  select(Country, Accuracy, Event, ccode1)

targets = tdat %>%
  pivot_wider(names_from = Event, values_from = Accuracy)


# Identify target countries in trade dataset  -----------------------------

## Function to return countries
target_countries <- function(data, economic_var = "Economic Power", tp_only = T) {
  if (tp_only) {
    # Return countries where 'Economic Power' is not NA and 'Diplomacy' is "True Positive"
    return(data[!is.na(data[[economic_var]]) & data$Diplomacy == "True Positive", ]$Country)
  } else {
    # Return countries where 'Economic Power' is not NA
    return(data[!is.na(data[[economic_var]]), ]$Country)
  }
}


## Read-in strategic countries (`trade_strategic_countries.R`)
sc <- readr::read_csv(here::here("data/strategic_countries.csv")) %>%
  mutate(dip = case_when(country %in% target_countries(targets, economic_var = "Diplomacy", tp_only = F) ~ 1,
                         TRUE ~0 ),
         dip_tp = case_when(country %in% target_countries(targets, economic_var = "Diplomacy", tp_only = T) ~ 1,
         TRUE ~0 ),
         hp = case_when(country %in% target_countries(targets, economic_var = "Hard Power", tp_only = F) ~ 1,
                        TRUE ~0 ),
         hp_tp = case_when(country %in% target_countries(targets, economic_var = "Hard Power", tp_only = T) ~ 1,
                           TRUE ~0 ),
         ep = case_when(country %in% target_countries(targets, economic_var = "Economic Power", tp_only = F)  ~ 1,
                        TRUE ~0 ),
         ep_tp = case_when(country %in% target_countries(targets, economic_var = "Economic Power", tp_only = T)   ~ 1,
         TRUE ~0 )) %>%
  mutate(ccode1 = countrycode(country, "country.name", "cown")) %>%
  mutate(ccode1 = case_when(country == "Serbia" ~ 345,
                            TRUE ~ ccode1))

# Merge with trade data and UN data ---------------------------------------

## Read-in UN Ideal Points (`un_ideal_points.R`)
unip <- readr::read_csv(here::here("data/un_ideal_points_avg.csv")) %>%
  group_by(ccode1, country) %>%
  summarise(IdealPointDistance = mean(IdealPointDistance, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = "2017-2021") %>%
  select(ccode1, country, year, IdealPointDistance) %>%
  rename(major_power = country)

## Pivot unit wider
unip = unip %>%
  tidyr::pivot_wider(names_from = major_power, 
              values_from = IdealPointDistance, 
              names_prefix = "idp_") %>%
  rename(idp_UUF = idp_AVG_UKO_FRA_USA)

## Read-in distance and cultural data
dist = tradestatistics::ots_distances %>%
  # Filter rows where either country1 or country2 is "rus"
  filter(country1 == "rus" | country2 == "rus") %>%
  # Create a new column that keeps the value from the column that is not "rus"
  mutate(new_country = if_else(country1 == "rus", country2, country1)) %>%
  select( -country2, -country1) %>%
  # Can't find COD or SRB, so replace values with nearby country
  mutate(new_country = case_when(new_country == "cog" ~ "cod",
                                 new_country == "bih" ~ "srb",
                                 TRUE ~ new_country) ) %>%
  rename(reporter_iso = new_country) %>%
  mutate(ccode1 = countrycode(reporter_iso, "iso3c", "cown")) %>%
  mutate(ccode1 = case_when(reporter_iso == "srb" ~ 345,
                            TRUE ~ ccode1)) %>%
  mutate(dist = dist/1000)


## Merge all datasets
dat <- sc %>%
  left_join(unip, by = "ccode1") %>%
  left_join(dist, by = "ccode1") 

# unique(df$country)[!unique(df$country) %in% dat$country]

# Hard Power Models -------------------------------------------------------

# Define the covariates
covariates <- list(
  expt = "impt_export_reliance_import_reliant",
  impt = "expt_import_reliance_exporter",
  wr = "idp_UUF + idp_RUS",
  chn = "idp_CHN",
  dist = "dist",
  full = "idp_UUF + idp_RUS + impt_export_reliance_import_reliant + expt_import_reliance_exporter"
)

# Define the outcome variables
outcomes <- c("dip", "dip_tp", "hp", "hp_tp", "ep", "ep_tp")

# Define the names of the models
model_names <- c('Diplomacy (All)', 'Diplomacy (True +)', 
                 'Hard Power (All)', 'Hard Power (True +)',
                 'Economic Power (All)', 'Economic Power (True +)')

# Initialize an empty list to store models
models <- list()

# Loop through outcomes and model names to create the models
for (i in seq_along(outcomes)) {
  outcome <- outcomes[i]
  for (cov in names(covariates)) {
    formula <- as.formula(paste(outcome, "~", covariates[[cov]]))
    model_name <- paste(model_names[i], cov, sep = " - ")
    models[[model_name]] <- lm(formula, data = dat)
  }
}

if (generate_all_model_figures) {
  for (outcome in model_names) {
    modelsummary(
      models[ grepl(outcome, names(models), fixed = T ) ],
      coef_map = c('(Intercept)' = "Intercept",
                   'impt_export_reliance_import_reliant' = "Importer",
                   'expt_import_reliance_exporter' = 'Exporter', 
                   'idp_UUF' = "UNIPD (West)",
                   'idp_CHN' = "UNIPD (China)",
                   'idp_RUS' = "UNIPD (Russia)",
                   'dist' = "Distance (1k km)"),
      col.names = NULL,  # This removes the column titles
      estimate  = "{estimate}{stars} ({std.error})",
      statistic = NULL,
      gof_omit = 'IC|RMSE|Log|F|R2$|Std.',
      output = here::here("writing", "figures", paste0(gsub("\\s", "_", gsub("\\(|\\)|%\\s","",outcome)), ".png"))
    )
  }
}


# Concentrated Table
tp_full = models[ grepl("(True +) - f", names(models), fixed = T ) ]
names(tp_full) = gsub(" \\(True \\+\\) - full" ,"", names(tp_full) )

tp_chn = models[ grepl("(True +) - c", names(models), fixed = T ) ]
names(tp_chn) = gsub(" \\(True \\+\\) - chn" ,"", names(tp_chn) )

tp_models = c(tp_full[1], tp_chn[1],
              tp_full[2], tp_chn[2],
              tp_full[3], tp_chn[3])

names(tp_models) = NULL

tp_tab = modelsummary(
  tp_models, 
  coef_map = c('(Intercept)' = "Intercept",
               'impt_export_reliance_import_reliant' = "Importer",
               'expt_import_reliance_exporter' = 'Exporter', 
               'idp_UUF' = "UNIPD (West)",
               'idp_CHN' = "UNIPD (China)",
               'idp_RUS' = "UNIPD (Russia)",
               'dist' = "Distance (1k km)"),
  fmt = 2, 
  estimate  = "{estimate}{stars}\n({std.error})",
  statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.', 
  output = "gt" ) %>%
  tab_spanner(label = 'Diplomacy', columns = 2:3) %>%
  tab_spanner(label = 'Hard Power', columns = 4:5) %>%
  tab_spanner(label = 'Economic Power', columns = 6:7) 

save_gt_png(tp_tab, here::here("writing", "figures", "tp_full.png"))

# Also export a LaTeX tabular for direct inclusion in the paper
tp_tex <- modelsummary(
  tp_models, 
  coef_map = c('(Intercept)' = "Intercept",
               'impt_export_reliance_import_reliant' = "Importer",
               'expt_import_reliance_exporter' = 'Exporter', 
               'idp_UUF' = "UNIPD (West)",
               'idp_CHN' = "UNIPD (China)",
               'idp_RUS' = "UNIPD (Russia)",
               'dist' = "Distance (1k km)"),
  fmt = 2, 
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  gof_omit = 'IC|RMSE|Log|F|R2$|Std.',
  output = "latex_tabular"
)
writeLines(tp_tex, here::here("writing", "figures", "tp_full.tex"))

