#' @describeIn path Path to github folder (that is on git).
#' @export
path_git <- function(...) {
  if (Sys.info()["nodename"]=="jeremy-System-Product-Name") {
    base <- "/home/jeremy/Dropbox/github-repos/"
  }
  
  # donald
  if(Sys.info()["user"] == "skybl") {
    if(Sys.info()["nodename"] == "DONALDS_DESKTOP"){
      base <- "C:/Users/skybl/Dropbox/My PC (LAPTOP-IKUHQKMA)/Documents/GitHub/"
    } else{
      base <- "D:/Dropbox/My PC (LAPTOP-IKUHQKMA)/Documents/GitHub/"
    }
  }

  # Check to make sure the directory exists and is correct
  if (!dir.exists(base)) {
    stop("Go edit path_root in R/paths.R and add your path to Github")
  }
  
  file.path(base, ...)
}

#' @describeIn path Path to the "ML for Peace" Dropbox folder
#' @export
path_dropbox <- function(...) {
  # default guess
  base <- "~/Dropbox/ML for Peace"
  
  # jeremy: office windows
  if (Sys.info()["user"]=="jerem") {
    base <- "C:/Users/jerem/Dropbox/ML for Peace"
  }
  # jeremy: home linux
  if (Sys.info()["nodename"]=="jeremy-System-Product-Name") {
    base <- "/home/jeremy/Dropbox/ML for Peace"
  }
  
  # donald
  if(Sys.info()["user"] == "skybl") {
    if(Sys.info()["nodename"] == "DONALDS_DESKTOP"){
      base <- "C:/Users/skybl/Dropbox/ML for Peace"
    } else{
      base <- "D:/Dropbox/ML for Peace"
    }
  }
  
  # Check to make sure the directory exists and is correct
  if (!dir.exists(base)) {
    stop("Go edit path_dropbox in R/misc.R and add your path to the ML4P Dropbox folder")
  }
  
  file.path(base, ...)
}

#library here
library(here)

# Conditional data load
load(paste0(path_git(),"/rai.atari/rai.atari/data/rai.rda"))
save(rai, file = here("data", paste0("rai_", Sys.Date())) )


folder_path <- paste0(path_dropbox(),"/ml4p.forecasting/1-raw-counts-econ")
file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
data <- map_df(file_list, read_csv)

df <- data %>%
  mutate(across(-c(total_articles, cs_999, rai_999, country, date), ~ . / total_articles, .names = "{.col}Norm"))

write_csv(df, "/home/jeremy/Downloads/civic_space.csv")