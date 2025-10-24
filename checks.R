library(assertthat)

path <- "S:\\Projects\\2025-OWID-Pipeline\\outputs"
csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)


data <- lapply(csv_files, read_csv)

# Name each list element with the file name (without .csv)
names(data) <- tools::file_path_sans_ext(basename(csv_files))





# Check number of columns --------------------------------------------------------------
print("Check number of columns is the same in all datasets")
assertthat::are_equal(
  length(unique(map_int(data, ~ length(names(.x))))),
  1
)
#------------------------------------------------------------------------------------


# Check year_ppp column --------------------------------------------------------------
print("Check we have unique year_ppp, equal to 2021")
assertthat::are_equal(
  data[str_detect(names(data), "21")] %>% bind_rows() %>% select(year_ppp) %>% unique() %>% pull(),
  2021
)
print("Check we have unique year_ppp, equal to 2017")
assertthat::are_equal(
  data[!(str_detect(names(data), "21"))] %>% bind_rows() %>% select(year_ppp) %>% unique() %>% pull(),
  2017
)
#------------------------------------------------------------------------------------


# Check equiv column ----------------------------------------------------------------
print("Check we have unique per capita, on 'pc' files")
assertthat::are_equal(
  data[str_detect(names(data), "pc")]  %>% bind_rows() %>% select(equiv) %>% pull() %>% unique(),
  "per capita"
)

print("Check we have unique square root, on 'pc' files")
assertthat::are_equal(
  data[str_detect(names(data), "eqv")]  %>% bind_rows() %>% select(equiv) %>% pull() %>% unique(),
  "square root"
)
#------------------------------------------------------------------------------------

# Check variable column --------------------------------------------------------------
print("Check we have unique dhi on variable column")
assertthat::are_equal(
  data[str_detect(names(data), "_dhi")]  %>% bind_rows() %>% select(variable) %>% pull() %>% unique(),
  "dhi"
)
print("Check we have unique mi on variable column")
assertthat::are_equal(
  data[str_detect(names(data), "_mi")] %>% bind_rows()%>% select(variable) %>% pull() %>% unique(),
  "mi"
)
#------------------------------------------------------------------------------------





# Checks on 1) "inequality" -----------------------------------------------------------------------------------------

data[str_starts(names(data), "inequality")] %>% imap(~ .x %>% select(indicator) %>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows()
data[str_starts(names(data), "inequality")] %>% imap(~ .x %>% filter(is.na(value))%>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows() # No
#---------------------------------------------------------------------------------------------------------------------


# Checks on 2) "incomes" -----------------------------------------------------------------------------------------
data[str_starts(names(data), "incomes")] %>% imap(~ .x %>% select(indicator) %>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows()
data[str_starts(names(data), "incomes")] %>% imap(~ .x %>% filter(is.na(value))%>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows() # No


  # Need to be the same as no PPP adjustment was needed 
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "decile_shares"))  %>% summarise(std_dev = sd(value))) %>% bind_rows()
data[str_starts(names(data), "income") & !(str_detect(names(data), "21"))] %>% map(~ .x %>% filter(str_detect(indicator, "decile_shares")) %>% summarise(std_dev = sd(value))) %>% bind_rows()

  # Need to be the same as no PPP adjustment was needed 
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "percentile_shares"))  %>% summarise(std_dev = sd(value))) %>% bind_rows()
data[str_starts(names(data), "income") & !(str_detect(names(data), "21"))] %>% map(~ .x %>% filter(str_detect(indicator, "percentile_shares")) %>% summarise(std_dev = sd(value))) %>% bind_rows()

  # Should be different as PPP adjustment was needed 
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "decile_ave"))  %>% summarise(std_dev = sd(value))) %>% bind_rows()
data[str_starts(names(data), "income") & !(str_detect(names(data), "21"))] %>% map(~ .x %>% filter(str_detect(indicator, "decile_ave")) %>% summarise(std_dev = sd(value))) %>% bind_rows()
  
  # Should be different as PPP adjustment was needed 
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "percentile_ave"))  %>% summarise(std_dev = sd(value))) %>% bind_rows()
data[str_starts(names(data), "income") & !(str_detect(names(data), "21"))] %>% map(~ .x %>% filter(str_detect(indicator, "percentile_ave")) %>% summarise(std_dev = sd(value))) %>% bind_rows()

  # Check Tawain exists on shares but nor on averages 
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "decile_shares") & cname == "Taiwan"))
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "percentile_shares") & cname == "Taiwan"))
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "decile_ave") & cname == "Taiwan"))
data[str_starts(names(data), "income") & str_detect(names(data), "21")] %>% map(~ .x %>% filter(str_detect(indicator, "percentile_ave") & cname == "Taiwan"))

#---------------------------------------------------------------------------------------------------------------------

# Checks on 3) "relative" -----------------------------------------------------------------------------------------

data[str_starts(names(data), "relat")] %>% imap(~ .x %>% select(indicator) %>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows()
data[str_starts(names(data), "relat")] %>% imap(~ .x %>% filter(is.na(value))%>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows() # No

#---------------------------------------------------------------------------------------------------------------------

# Checks on 4) "absolute" -----------------------------------------------------------------------------------------
data[str_starts(names(data), "abs")] %>% imap(~ .x %>% select(indicator) %>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows()
data[str_starts(names(data), "abs")] %>% imap(~ .x %>% filter(is.na(value))%>% summarise(rows = n()) %>% mutate(dname = .y)) %>% bind_rows() # No


data[str_starts(names(data), "abs")] %>% 
  imap(~ .x %>% 
    filter(is.na(value)) %>% 
     summarize(rows = n(), n_indicators =  n_distinct(indicator)) %>%
   mutate(dname = .y)
) %>% 
  bind_rows() %>% 
  arrange(rows)
#---------------------------------------------------------------------------------------------------------------------






