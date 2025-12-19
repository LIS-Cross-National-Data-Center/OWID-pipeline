

# 1) Loading the data  ---------------------------------------------------------------
variables_owid <- c("dhi", "hifactor", "hiprivate", "hi33", "grossnet")

tic()
full_data_hhd <- lissyuse(
  data = lissyrtools::get_countries_lis(),
  vars = variables_owid
) # 152.78 sec elapsed / 172.73
toc()


# 2) Market Income  ---------------------------------------------------------------
data_with_mi <- purrr::map(
  full_data_hhd,
  ~ .x %>%
    mutate(
      mi = rowSums(across(c("hifactor", "hiprivate", "hi33")), na.rm = TRUE)
    ) %>%
    mutate(
      mi = if (
        mean(hifactor, na.rm = TRUE) > 0 &&
          mean(hiprivate, na.rm = TRUE) > 0 &&
          mean(hi33, na.rm = TRUE) > 0
      ) {
        mi
      } else {
        NA
      },
      mi = if_else(
        grossnet < 200,
        mi,
        NA
      )
    )
)

print(paste0("Creation of Market Income - mi - variable, finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))




# 3) Preparation of the data -----------------------------------------


# A) Without PPP adjustment (all LIS datasets, including Taiwan and ru22 for example which don't have PPP values)
prep_data <- purrr::map(
  data_with_mi,
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: "type_2" ?
  purrr::map( ~ .x %>% mutate(dhi_pc = dhi / nhhmem)) %>% 
  purrr::map( ~ .x %>% mutate(mi_pc = mi / nhhmem)) %>% 
  apply_sqrt_equivalisation("dhi") %>% 
  apply_sqrt_equivalisation("mi") %>% 
  map(~ .x %>% select(-hifactor, -hi33, -hiprivate, -currency, -grossnet, -hwgt, -hwgta))

print(paste0("Preparation of the data without PPP adjustment, finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))

# B) With PPP adjustment

dnames_in_LIS <- lissyrtools::datasets %>% filter(database == "LIS") %>% select(dname) %>% pull() %>% unique()
ccyy_with_PPPs <- lissyrtools::deflators %>% mutate(ccyy = str_c(iso2, str_sub(year,3,4))) %>% select(ccyy) %>% pull() %>% unique()
intersection <- intersect(dnames_in_LIS, ccyy_with_PPPs)
to_remove_when_data_ppp_adjusted  <-  str_c(dnames_in_LIS[!(dnames_in_LIS %in% intersection)], "h")


prep_data_ppp_adj <- purrr::map(
  data_with_mi[!(names(data_with_mi) %in% to_remove_when_data_ppp_adjusted)],
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_ppp_adjustment("dhi", transformation = "lisppp", database = "lis", base_year_ppp = 2021) %>%
  apply_ppp_adjustment("mi", transformation = "lisppp", database = "lis", base_year_ppp = 2021) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: "type_2" ?
  purrr::map( ~ .x %>% mutate(dhi_pc = dhi / nhhmem)) %>% 
  purrr::map( ~ .x %>% mutate(mi_pc = mi / nhhmem)) %>% 
  apply_sqrt_equivalisation("dhi") %>%
  apply_sqrt_equivalisation("mi") %>% 
  map(~ .x %>% select(-hifactor, -hi33, -hiprivate, -currency, -grossnet, -hwgt, -hwgta))


print(paste0("Preparation of the data with PPP adjustment, finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


# 4) Remake data lists in 1) and 2) without dname's that have NA in market income 

study_missing_mi <- imap(prep_data, ~ .x %>% 
  dplyr::summarize(
    var = sum(is.na(mi)),
    obs = n()
) %>% 
  mutate(dataset = .y)
) %>% 
  bind_rows()

dnames_with_missing_mi <- study_missing_mi %>% filter(var > 0) %>% select(dataset) %>% pull() %>% unique()

# A) 
prep_data_mi <- prep_data[!(names(prep_data) %in% dnames_with_missing_mi)]
print(paste0("Preparation of the data with (NO) PPP adjustment, and without datasets with missing Market Income, finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))

# B) 
prep_data_ppp_adj_mi <- prep_data_ppp_adj[!(names(prep_data_ppp_adj) %in% dnames_with_missing_mi)]
print(paste0("Preparation of the data with PPP adjustment, and without datasets with missing Market Income, finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))
