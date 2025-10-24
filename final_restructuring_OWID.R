
# Final restructuring for OWID 



  # 0) ------------- Import output files ---------------------------------

path <- output_path_int
csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)


data <- lapply(csv_files, read_csv)

# Name each list element with the file name (without .csv)
names(data) <- tools::file_path_sans_ext(basename(csv_files))

today_date <- stringr::str_replace_all(today(), "-", "_")

 # -----------------------------------------------------------------------

  # 1) -------------

ineq <-  data[str_detect(names(data),"inequality")] %>% 
  bind_rows()  %>% 
  select(-year_ppp)

write_csv(ineq, paste0(output_path_OWID, paste0("inequality_", today_date, ".csv")))

  # 2) -------------

incomes17 <- map(data[str_detect(names(data),"incomes") & !(str_detect(names(data),"21"))], ~ .x  %>% 
  rename(value17 = value)
) %>% 
  bind_rows() %>% 
  select(-year_ppp)

incomes21 <- map(data[str_detect(names(data),"incomes") & str_detect(names(data),"21")], ~ .x  %>% 
  rename(value21 = value)
) %>% 
  bind_rows() %>% 
  select(-year_ppp)

final_incomes <- left_join(
  incomes17, incomes21, 
  by = c("cname", "year", "indicator", "dname", "variable",  "equiv")
) %>% 
  filter(!(str_starts(indicator, "p")))


write_csv(final_incomes, paste0(output_path_OWID, paste0("incomes_", today_date, ".csv")))

  # 3) -------------

relative_pov <- data[str_detect(names(data),"relat")] %>% 
  bind_rows()  %>% 
  select(-year_ppp)

write_csv(relative_pov, paste0(output_path_OWID, paste0("relative_poverty_", today_date, ".csv")))

  # 4) -------------

abs_pov_17 <- map(data[str_detect(names(data),"abs") & !(str_detect(names(data),"21"))], ~ .x  %>% 
  rename(value17 = value)) %>% 
  bind_rows()  %>% 
  select(-year_ppp)

abs_pov_21 <- map(data[str_detect(names(data),"abs") & str_detect(names(data),"21")], ~ .x  %>% 
  rename(value21 = value)) %>% 
  bind_rows() %>% 
  select(-year_ppp)

final_abs_pov <- left_join(
  abs_pov_17, abs_pov_21, 
  by = c("cname", "year", "indicator", "dname", "variable",  "equiv")
) %>% 
  filter(!(str_detect(indicator, "2.15|3.65|6.85")))

write_csv(final_abs_pov, paste0(output_path_OWID, paste0("absolute_poverty_", today_date, ".csv")))

