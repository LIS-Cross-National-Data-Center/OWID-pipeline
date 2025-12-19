# OWID Pipeline 

# Gonçalo Marques 
# 21st October 2025


# libraries ---------------------------------------------------------------

library(tidyverse)
library(lissyrtools)
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
paste0("==============================")
paste0("Starting full data pipeline")
paste0(format(Sys.time(), "%d-%B-%Y %H:%M:%S"))
paste0("==============================")





# Data Preparation

paste0(">>> Running data preparation (1_data_prep.R)")
source("1_data_prep.R")

# Computation of aggregate figures ------------------------------------------------
paste0(">>> Starting aggregate figure computations <<<")

plan(multisession, workers = 4)  # works on Windows & macOS/Linux

scripts <- c(
  "2_1_dhi_eqv.R",
  "2_2_dhi_pc.R",
  "2_3_mi_eqv.R",
  "2_4_mi_pc.R"
)

results <- future_map(scripts, function(script) {
  paste0("Running ", script)
  library(tidyverse)
  library(lissyrtools)
  source("path_definer.R")
  source("auxilliary_functions.R")
  source(script)

},
.options = furrr_options(globals = c("prep_data", "prep_data_ppp_adj", "prep_data_mi", "prep_data_ppp_adj_mi"))
)

  
# Restructuring & Final Output ------------------------------------------------

paste0(">>> Running final restructuring (3_final_restructuring_OWID.R)")
source("3_final_restructuring_OWID.R")



paste0("==============================")
paste0("Pipeline finished successfully")
paste0(format(Sys.time(), "%d-%B-%Y %H:%M:%S"))
paste0("==============================")

sink()
sink(type = "message")
