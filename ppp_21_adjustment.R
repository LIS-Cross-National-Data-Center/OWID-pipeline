# 2. With PPP adjustment (including Taiwan and ru22 for example)
prep_data_ppp_adj <- purrr::map(
  data_with_mi[(str_sub(names(data_with_mi),1,2) != "tw") & (str_sub(names(data_with_mi),1,4) != "ru22")],
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_ppp_adjustment("dhi", transformation = "lisppp", database = "lis", base_year_ppp = 2021) %>%
  apply_ppp_adjustment("mi", transformation = "lisppp", database = "lis", base_year_ppp = 2021) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_4") %>% # type = "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_4") %>% # type = "type_2" ?
  apply_sqrt_equivalisation("dhi") %>%
  apply_sqrt_equivalisation("mi") 
# in terms of the preparation, we are on a very good path of alignment with OWID figures computed in Stata.



average_21 <- prep_data_ppp_adj %>%
  run_weighted_mean("dhi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(indicator = "Average", variable = "dhi", equiv = "square root")

median_21 <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = c(0.5)) %>%
  structure_to_plot() %>%
mutate(indicator = "Median", variable = "dhi", equiv = "square root")

deciles_21 <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = seq(0.1, 0.9, 0.1)) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("d_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)



deciles_mean_21 <- prep_data_ppp_adj %>%
  run_weighted_percentiles(
    "dhi",
    "new_wgt",
    probs = seq(0.1, 0.9, 0.1),
    average = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("decile_averages_p_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category) 


percentiles_21 <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = seq(0.01, 0.99, 0.01)) %>%
  structure_to_plot() %>% 
  mutate( 
    indicator = str_c("p_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)


percentiles_mean_21 <- prep_data_ppp_adj %>%
  run_weighted_percentiles(
    "dhi",
    "new_wgt",
    probs = seq(0.01, 0.99, 0.01),
    average = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("percentile_averages_p_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)


incomes_across_distribution_21 <- list(
  average_21,
  median_21,
  deciles_21,
  deciles_mean_21,
  percentiles_21,
  percentiles_mean_21
) %>%
  bind_rows()

write_csv(incomes_across_distribution, "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\incomes_across_distribution_21.csv")




  # Poverty (absolute) thresholds as daily monetary threshold in international dollars at 2017 PPPs -----------------------

poverty_lines <- c(
  2.15,
  3.65,
  6.85,
  1,
  2,
  5,
  10,
  20,
  30,
  40
)

results <- list()
for (line in poverty_lines) {
  a <- run_weighted_absolute_poverty(
    data_list = prep_data_ppp_adj,
    var_name = "dhi",
    wgt_name = "new_wgt",
    daily_poverty_line = line
  ) %>%
    structure_to_plot() %>%
    mutate(
      indicator = str_c("absolute_poverty_rate_", line),
      variable = "dhi",
      equiv = "square root"
    )
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_poverty_rate: ", line))
}
absolute_poverty_rate_21 <- bind_rows(results)



results <- list()
for (line in poverty_lines) {
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj,
  var_name = "dhi",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = FALSE
) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("average_poverty_shortfall_abs_", line),
    variable = "dhi",
    equiv = "square root"
  )
  results[[as.character(line)]] <- a
  print(paste0("Done average_poverty_shortfall_abs: ", line))
}
average_poverty_shortfall_abs_21 <- bind_rows(results)


results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj,
  var_name = "dhi",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = TRUE
) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("percentage_poverty_shortfall_abs_", line),
    variable = "dhi",
    equiv = "square root"
  )
  results[[as.character(line)]] <- a
  print(paste0("Done percentage_poverty_shortfall_abs: ", line))
}
percentage_poverty_shortfall_abs_21 <- bind_rows(results)



results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_gap_index(
  prep_data_ppp_adj,
  var_name = "dhi",
  wgt_name = "new_wgt",
  daily_poverty_line = line
) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("absolute_poverty_gap_index_", line),
    variable = "dhi",
    equiv = "square root"
  )
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_poverty_gap_index: ", line))
}
absolute_poverty_gap_index_21 <- bind_rows(results)


# Bind fourth set of indicators

absolute_poverty_figures <- list(
  absolute_poverty_rate_21,
  average_poverty_shortfall_abs_21,
  percentage_poverty_shortfall_abs_21,
  absolute_poverty_gap_index_21
) %>%
  bind_rows()

write_csv(absolute_poverty_figures, "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\absolute_poverty_21.csv")