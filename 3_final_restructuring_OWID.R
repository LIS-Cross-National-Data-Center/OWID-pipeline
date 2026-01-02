
# Final restructuring for OWID 

today_date <- stringr::str_replace_all(today(), "-", "_")



    # 1) Dealing with intermediate output files -----------------------------------


# Check that all the files in output_path_int (that must have been rewritten in 2.1 2.2 2.3 and 2.4) are from today. 

check_csvs_today <- function(folder) {
  files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No CSV files found in the folder.")
  }

  file_dates <- as.Date(file.info(files)$mtime)

  if (!all(file_dates == Sys.Date())) {
    bad_files <- files[file_dates != Sys.Date()]
    stop(
      "These CSV files are NOT from today:\n",
      paste(basename(bad_files), collapse = "\n")
    )
  }

  print("All CSV files in output_path_int are from today.")
  invisible(TRUE)
}
check_csvs_today(output_path_int)


csv_files_new <- list.files(output_path_int, pattern = "\\.csv$", full.names = TRUE)

data <- lapply(csv_files_new, function(file) {
  read_csv(file, show_col_types = FALSE)
})

# Name each list element with the file name (without .csv)
names(data) <- tools::file_path_sans_ext(basename(csv_files_new))


    # 2) Production of final output files ---------------------------------------------------

csv_files_old <- list.files(output_path_OWID, pattern = "\\.csv$", full.names = TRUE)


if (mds == "all"){   # ------------- Relocate old output files and write new ones

file.rename(
  from = csv_files_old,
  to   = file.path(old_output_path, basename(csv_files_old))
)

  
 # -----------------------------------------------------------------------

  # 1) -------------

ineq <-  data[str_detect(names(data),"inequality")] %>% 
  bind_rows()  %>% 
  select(-year_ppp)

write_csv(ineq, paste0(output_path_OWID, paste0("inequality_", today_date, ".csv")))
print(paste0("inequality_", today_date, ".csv", " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


  # 2) -------------

incomes <- data[str_detect(names(data),"incomes")] %>%
  bind_rows() %>% 
  filter(!(str_starts(indicator, "p"))) %>% 
  select(-year_ppp)

write_csv(incomes, paste0(output_path_OWID, paste0("incomes_", today_date, ".csv")))
print(paste0("incomes_", today_date, ".csv", " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


  # 3) -------------

relative_pov <- data[str_detect(names(data),"relat")] %>% 
  bind_rows()  %>% 
  select(-year_ppp)

write_csv(relative_pov, paste0(output_path_OWID, paste0("relative_poverty_", today_date, ".csv")))
print(paste0("relative_poverty_", today_date, ".csv", " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


  # 4) -------------

abs_pov <- data[str_detect(names(data),"abs")] %>% 
  bind_rows() %>% 
  select(-year_ppp)                 
  #filter(!(str_detect(indicator, "3$|4\\.2$|8\\.3$"))) %>%  # "2.15|3.65|6.85" --> 2017 ppp WB thresholds
  

write_csv(abs_pov, paste0(output_path_OWID, paste0("absolute_poverty_", today_date, ".csv")))
print(paste0("absolute_poverty_", today_date, ".csv", " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))

  
} else if (mds == "upload") { # create copy on 'old_versions' folder, and proceed to change the ones in 'output_path_OWID'

file.copy(
  from = csv_files_old,
  to   = file.path(old_output_path, basename(csv_files_old))
)

  
 # 1) -------------

 file_name <- list.files(
  output_path_OWID,
  pattern = "^inequality.*\\.csv$",
  full.names = TRUE
)

ineq_no_upload <- read_csv(file_name) %>% 
  filter(!(dname %in% stringr::str_sub(lispostharmtools::identify_ikf_files_from_upload(),1,4)))
  
ineq_to_append <- data[str_detect(names(data),"inequality")] %>% 
  bind_rows()  %>% 
  select(-year_ppp)
  
ineq = bind_rows(ineq_no_upload, ineq_to_append)  

file.remove(file_name)  
write_csv(ineq, paste0(output_path_OWID, paste0("inequality_", today_date, ".csv")))
print(paste0("inequality_", today_date, ".csv", " replaced on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


  # 2) -------------

 file_name <- list.files(
  output_path_OWID,
  pattern = "^incomes.*\\.csv$",
  full.names = TRUE
)

incomes_no_upload <- read_csv(file_name) %>% 
  filter(!(dname %in% stringr::str_sub(lispostharmtools::identify_ikf_files_from_upload(),1,4)))
  
incomes_to_append <- data[str_detect(names(data),"incomes")] %>%
  bind_rows() %>% 
  filter(!(str_starts(indicator, "p"))) %>% 
  select(-year_ppp)
  
incomes = bind_rows(incomes_no_upload, incomes_to_append) 

file.remove(file_name)  
write_csv(incomes, paste0(output_path_OWID, paste0("incomes_", today_date, ".csv")))
print(paste0("incomes_", today_date, ".csv", " replaced on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


  # 3) -------------

 file_name <- list.files(
  output_path_OWID,
  pattern = "^relative_poverty.*\\.csv$",
  full.names = TRUE
)

relative_pov_no_upload <- read_csv(file_name) %>% 
  filter(!(dname %in% stringr::str_sub(lispostharmtools::identify_ikf_files_from_upload(),1,4)))
  
relative_pov_to_append <- data[str_detect(names(data),"relat")] %>% 
  bind_rows()  %>% 
  select(-year_ppp)
  
relative_pov = bind_rows(relative_pov_no_upload, relative_pov_to_append)

file.remove(file_name)  
write_csv(relative_pov, paste0(output_path_OWID, paste0("relative_poverty_", today_date, ".csv")))
print(paste0("relative_poverty_", today_date, ".csv", " replaced on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


  # 4) -------------

 file_name <- list.files(
  output_path_OWID,
  pattern = "^absolute_poverty.*\\.csv$",
  full.names = TRUE
)

abs_pov_no_upload <- read_csv(file_name) %>% 
  filter(!(dname %in% stringr::str_sub(lispostharmtools::identify_ikf_files_from_upload(),1,4)))
  
abs_pov_to_append <- data[str_detect(names(data),"abs")] %>% 
  bind_rows() %>% 
  select(-year_ppp)                 
  #filter(!(str_detect(indicator, "3$|4\\.2$|8\\.3$"))) %>%  # "2.15|3.65|6.85" --> 2017 ppp WB thresholds
  
abs_pov = bind_rows(abs_pov_no_upload, abs_pov_to_append)


file.remove(file_name)  
write_csv(abs_pov, paste0(output_path_OWID, paste0("absolute_poverty_", today_date, ".csv")))
print(paste0("absolute_poverty_", today_date, ".csv", " replaced on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))
  
  
}
