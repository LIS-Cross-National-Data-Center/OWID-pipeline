# OWID Pipeline 

# Gonçalo Marques 
# 21st October 2025

# --------------- MANUAL DATASET SELECTION ------------------------------------

mds = "upload" # 2 options: 1) "all", 2) "upload"

#------------------------------------------------------------------------------


# libraries ---------------------------------------------------------------

library(tidyverse)
library(lissyrtools)
library(lubridate)
library(assertthat)  
library(tictoc)
library(furrr)

# check if the package version is the most updated one
 assertthat::assert_that(
   packageVersion("lissyrtools") == lissyrtools::check_github_version()
 ) # if TRUE we are sure to be working with the most updated set of countries and datasets as well.

# Paths 
source("path_definer.R")
# Import auxilliary functions
source("auxilliary_functions.R")


log_file_name <- paste0("log_file_", stringr::str_replace_all(today(), "-", "_"), ".txt")
log_file_path <- file.path("log", log_file_name)
log_file <- file(log_file_path, open = "wt")
sink(log_file, split = TRUE, type = "output")
sink(log_file, type = "message")


# ---- SIGNAL START ----
cat("==============================\n")
cat("Starting full data pipeline\n")
cat(format(Sys.time(), "%d-%B-%Y %H:%M:%S"), "\n")
cat("==============================\n\n")





# Data Preparation

cat(">>> Running data preparation (1_data_prep.R)\n")
source("1_data_prep.R")

# Computation of aggregate figures ------------------------------------------------
cat(">>> Starting aggregate figure computations <<<", "\n")
cat("Progress can be checked by refreshing S:/Projects/2025-OWID-Pipeline/outputs", "\n")

plan(multisession, workers = 4)  # works on Windows & macOS/Linux

scripts <- c(
  "2_1_dhi_eqv.R",
  "2_2_dhi_pc.R",
  "2_3_mi_eqv.R",
  "2_4_mi_pc.R"
)


results <- furrr::future_map(scripts, function(script) {
  cat("Running: ", script, "\n")
  library(tidyverse)
  library(lissyrtools)
  source("path_definer.R")
  source("auxilliary_functions.R")
  source(script)

},
.options = furrr::furrr_options(globals = c("prep_data", "prep_data_ppp_adj", "prep_data_mi", "prep_data_ppp_adj_mi"))
)

  
# Restructuring & Final Output ------------------------------------------------

cat(">>> Running final restructuring (3_final_restructuring_OWID.R)\n")
source("3_final_restructuring_OWID.R")



cat("==============================\n")
cat("Pipeline finished successfully\n")
cat(format(Sys.time(), "%d-%B-%Y %H:%M:%S\n"))
cat("==============================\n")

sink()
sink(type = "message")
