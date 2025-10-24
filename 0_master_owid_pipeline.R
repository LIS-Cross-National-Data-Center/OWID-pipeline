# OWID Pipeline 

# Gonçalo Marques 
# 21st October 2025


# libraries ---------------------------------------------------------------

library(tidyverse)
library(lissyrtools)
library(assertthat)
library(tictoc)

# check if the package version is the most updated one
assertthat::assert_that(
  packageVersion("lissyrtools") == lissyrtools::check_github_version()
) # if TRUE we are sure to be working with the most updated set of countries and datasets as well.

# Paths 
source("path_definer.R")

# Import auxilliary functions
source("auxilliary_functions.R")


# Loading the data  ---------------------------------------------------------------
variables_owid <- c("dhi", "hifactor", "hiprivate", "hi33", "grossnet")

tic()
full_data_hhd <- lissyuse(
  data = lissyrtools::get_countries_lis(),
  vars = variables_owid
) # 152.78 sec elapsed / 172.73
toc()


# Market Income  ---------------------------------------------------------------
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

# Preparation of the data -----------------------------------------


# 1. Without PPP adjustment (including Taiwan and ru22 for example)
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
  apply_sqrt_equivalisation("mi") 

# 2. With PPP adjustment
prep_data_ppp_adj <- purrr::map(
  data_with_mi[(str_sub(names(data_with_mi),1,2) != "tw") & (str_sub(names(data_with_mi),1,4) != "ru22")],
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_ppp_adjustment("dhi", transformation = "lisppp", database = "lis", base_year_ppp = 2017) %>%
  apply_ppp_adjustment("mi", transformation = "lisppp", database = "lis", base_year_ppp = 2017) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: "type_2" ?
  purrr::map( ~ .x %>% mutate(dhi_pc = dhi / nhhmem)) %>% 
  purrr::map( ~ .x %>% mutate(mi_pc = mi / nhhmem)) %>% 
  apply_sqrt_equivalisation("dhi") %>%
  apply_sqrt_equivalisation("mi") 

# 3. With PPP adjustment (new 2021 PPP's)
prep_data_ppp_adj_2021 <- purrr::map(
  data_with_mi[(str_sub(names(data_with_mi),1,2) != "tw") & (str_sub(names(data_with_mi),1,4) != "ru22")],
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_ppp_adjustment("dhi", transformation = "lisppp", database = "lis", base_year_ppp = 2021) %>%
  apply_ppp_adjustment("mi", transformation = "lisppp", database = "lis", base_year_ppp = 2021) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: type = "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_4") %>% # before and in Stata: type = "type_2" ?
  purrr::map( ~ .x %>% mutate(dhi_pc = dhi / nhhmem)) %>% 
  purrr::map( ~ .x %>% mutate(mi_pc = mi / nhhmem)) %>% 
  apply_sqrt_equivalisation("dhi") %>%
  apply_sqrt_equivalisation("mi") 






# Computation of aggregate figures ------------------------------------------------


source("1_dhi_eqv.R")
source("2_dhi_pc.R")
source("3_mi_eqv.R")
source("4_mi_pc.R")
