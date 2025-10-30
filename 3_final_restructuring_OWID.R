
# Final restructuring for OWID 



  # 0) ------------- Relocate old output files, import new output files ---------------------------------

today_date <- stringr::str_replace_all(today(), "-", "_")

csv_files_old <- list.files(output_path_OWID, pattern = "\\.csv$", full.names = TRUE)

file.rename(
  from = csv_files_old,
  to   = file.path(old_output_path, basename(csv_files_old))
)


csv_files_new <- list.files(output_path_int, pattern = "\\.csv$", full.names = TRUE)

data <- lapply(csv_files_new, function(file) {
  read_csv(file, show_col_types = FALSE)
})

# Name each list element with the file name (without .csv)
names(data) <- tools::file_path_sans_ext(basename(csv_files_new))


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
  filter(!(str_detect(indicator, "3$|4\\.2$|8\\.3$"))) %>%  # "2.15|3.65|6.85" --> 2017 ppp WB thresholds
  select(-year_ppp)

write_csv(abs_pov, paste0(output_path_OWID, paste0("absolute_poverty_", today_date, ".csv")))
print(paste0("absolute_poverty_", today_date, ".csv", " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))






