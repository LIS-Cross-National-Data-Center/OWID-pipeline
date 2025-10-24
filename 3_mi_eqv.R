
# mi - eqv 

prep_data_orig_dhi <- prep_data
study_missing_na <- imap(prep_data_orig_dhi, ~ .x %>% 
  dplyr::summarize(
    var = sum(is.na(mi)),
    obs = n()
  
) %>% 
  mutate(dataset = .y)
) %>% 
  bind_rows()


dnames_with_missing_na <- study_missing_na %>% filter(var > 0 ) %>% select(dataset) %>% pull() %>% unique()


prep_data <- prep_data[!(names(prep_data) %in% dnames_with_missing_na)]
prep_data_ppp_adj <- prep_data_ppp_adj[!(names(prep_data_ppp_adj) %in% dnames_with_missing_na)]
prep_data_ppp_adj_2021 <- prep_data_ppp_adj_2021[!(names(prep_data_ppp_adj_2021) %in% dnames_with_missing_na)]

# 1) Inequalities  --------------------------------------
gini <- prep_data %>% #list
  run_weighted_gini("mi", "new_wgt") %>% # dedicated function, that only asks for a variable and a weight
  structure_to_plot() %>% # Function to restructure a list into a tidy data frame
  mutate(indicator = "Gini Index") # addition of extra columns

share_richest_10 <- prep_data %>%
  run_weighted_percentiles("mi", "new_wgt", probs = c(0.9), share = TRUE) %>%
  structure_to_plot() %>%
  filter(category == "90-100%") %>%
  select(-category) %>%
  mutate(indicator = "Share Top 10")

share_poorest_50 <- prep_data %>%
  run_weighted_percentiles("mi", "new_wgt", probs = c(0.5), share = TRUE) %>%
  structure_to_plot() %>%
  filter(category == "0-50%") %>%
  select(-category) %>%
  mutate(indicator = "Share Bottom 50")

palma_ratio <- prep_data %>%
  run_weighted_percentiles(
    "mi",
    "new_wgt",
    probs = c(0.4, 0.9),
    share = TRUE
  ) %>%
  structure_to_plot() %>%
  filter(category %in% c("0-40%", "90-100%")) %>%
  pivot_wider(names_from = category, values_from = value) %>%
  mutate(value = `90-100%` / `0-40%`) %>%
  select(-`0-40%`, -`90-100%`) %>%
  mutate(indicator = "Palma Ratio")

share_below_50_median <- prep_data %>%
  share_below_half_median("mi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(indicator = "Share below half median")

ratio_90_10 <- prep_data %>%
  run_weighted_ratios(
    "mi",
    "new_wgt",
    upper_percentile = 0.9,
    lower_percentile = 0.1
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = "Ratio p90_p10")

ratio_90_50 <- prep_data %>%
  run_weighted_ratios(
    "mi",
    "new_wgt",
    upper_percentile = 0.9,
    lower_percentile = 0.5
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = "Ratio p90_p50")

ratio_50_10 <- prep_data %>%
  run_weighted_ratios(
    "mi",
    "new_wgt",
    upper_percentile = 0.5,
    lower_percentile = 0.1
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = "Ratio p50_p10")



# Bind first set of indicators 
inequality_data <- list(
  gini,
  share_richest_10,
  share_poorest_50,
  palma_ratio,
  share_below_50_median,
  ratio_90_10,
  ratio_90_50,
  ratio_50_10
) %>%
  bind_rows() %>% mutate(variable = "mi", equiv = "square root", year_ppp = 2017)


write_csv(inequality_data, paste0(output_path_int, "inequality_mi_eqv.csv"))





# 2) Incomes across the Distribution  --------------------------------------
average <- prep_data_ppp_adj %>%
  run_weighted_mean("mi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(indicator = "Average")

median <- prep_data_ppp_adj %>%
  run_weighted_percentiles("mi", "new_wgt", probs = c(0.5)) %>%
  structure_to_plot() %>%
  mutate(indicator = "Median")

deciles <- prep_data_ppp_adj %>%
  run_weighted_percentiles("mi", "new_wgt", probs = seq(0.1, 0.9, 0.1)) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("d_", str_sub(category, 1, -2))) %>%
  select(-category)

deciles_shares <- prep_data %>%
  run_weighted_percentiles(
    "mi",
    "new_wgt",
    probs = seq(0.1, 0.9, 0.1),
    share = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("decile_shares_p_", str_sub(category, 1, -2))) %>%
  select(-category)

deciles_mean <- prep_data_ppp_adj %>%
  run_weighted_percentiles(
    "mi",
    "new_wgt",
    probs = seq(0.1, 0.9, 0.1),
    average = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("decile_averages_p_", str_sub(category, 1, -2))) %>%
  select(-category) 


percentiles <- prep_data_ppp_adj %>%
  run_weighted_percentiles("mi", "new_wgt", probs = seq(0.01, 0.99, 0.01)) %>%
  structure_to_plot() %>% 
  mutate(indicator = str_c("p_", str_sub(category, 1, -2))) %>%
  select(-category)

percentiles_shares <- prep_data %>%
  run_weighted_percentiles(
    "mi",
    "new_wgt",
    probs = seq(0.01, 0.99, 0.01),
    share = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("percentile_shares_p_", str_sub(category, 1, -2))) %>%
  select(-category)

percentiles_mean <- prep_data_ppp_adj %>%
  run_weighted_percentiles(
    "mi",
    "new_wgt",
    probs = seq(0.01, 0.99, 0.01),
    average = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("percentile_averages_p_", str_sub(category, 1, -2))) %>%
  select(-category)



# Bind second set of indicators

incomes_across_distribution <- list(
  average,
  median,
  deciles,
  deciles_shares,
  deciles_mean,
  percentiles,
  percentiles_shares,
  percentiles_mean
) %>%
  bind_rows() %>% mutate(variable = "mi", equiv = "square root", year_ppp = 2017)

write_csv(incomes_across_distribution, paste0(output_path_int, "incomes_across_distribution_mi_eqv.csv"))





# 3) Poverty (relative) thresholds as percentage of the median  --------------------------------

thresholds <- c(
  0.4,
  0.5,
  0.6
)

# Number of poor 
results <- list()
for (line in thresholds) {
  a <- number_poor_relative(    
    prep_data,
    var_name = "mi",
    wgt_name = "new_wgt",
    times_median = line
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("number_poor_relative_", line))
  results[[as.character(line)]] <- a
}
number_poor_relative_res <- bind_rows(results)


# Poverty Rate 
results <- list()
for (line in thresholds) {
  a <- run_weighted_relative_poverty(
    prep_data,
    var_name = "mi",
    wgt_name = "new_wgt",
    times_median = line
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("relative_poverty_rate_", line))
  results[[as.character(line)]] <- a
}
relative_poverty_rate <- bind_rows(results)



# average_poverty_shortfall_relt_to_median
results <- list()
for (line in thresholds) {
  a <- run_weighted_poverty_shortfall(
    prep_data,
    var_name = "mi",
    wgt_name = "new_wgt",
    times_median = line,
    percent = FALSE
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("average_poverty_shortfall_relt_to_median_", line))
  results[[as.character(line)]] <- a
}
average_poverty_shortfall_relt_to_median <- bind_rows(results)

# percentage_poverty_shortfall_relt_to_median
results <- list()
for (line in thresholds) {
  a <- run_weighted_poverty_shortfall(
    prep_data,
    var_name = "mi",
    wgt_name = "new_wgt",
    times_median = line,
    percent = TRUE
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("percentage_poverty_shortfall_relt_to_median_", line))
  results[[as.character(line)]] <- a
}

percentage_poverty_shortfall_relt_to_median <- bind_rows(results)

# Total shortfall relative to the Median 
results <- list()
for (line in thresholds) {
  a <- total_shortfall_relative( 
    prep_data,
    var_name = "mi",
    wgt_name = "new_wgt",
    times_median = line
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("total_shortfall_relative_", line))
  results[[as.character(line)]] <- a
}

total_shortfall_relative_res <- bind_rows(results)



# relative_poverty_gap_index

results <- list()
for (line in thresholds) {
a <- run_weighted_poverty_gap_index(
  prep_data,
  var_name = "mi",
  wgt_name = "new_wgt",
  times_median = line
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("relative_poverty_gap_index_", line))
  results[[as.character(line)]] <- a
}

relative_poverty_gap_index <- bind_rows(results)


# Bind third set of indicators

relative_poverty_figures <- list(
  number_poor_relative_res,
  relative_poverty_rate,
  average_poverty_shortfall_relt_to_median,
  percentage_poverty_shortfall_relt_to_median,
  total_shortfall_relative_res,
  relative_poverty_gap_index
) %>%
  bind_rows() %>% mutate(variable = "mi", equiv = "square root", year_ppp = 2017)

write_csv(relative_poverty_figures, paste0(output_path_int, "relative_poverty_mi_eqv.csv"))



# 4) Poverty (absolute) thresholds as daily monetary threshold in international dollars at 2017 PPPs -----------------------

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


# Number of poor 
results <- list()
for (line in poverty_lines) {
  a <- number_poor_abs( 
    data_list = prep_data_ppp_adj,
    var_name = "mi",
    wgt_name = "new_wgt",
    daily_poverty_line = line
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("number_poor_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_number_of_poor: ", line))
}
number_poor_abs_res <- bind_rows(results)


# Poverty Rate Absolute
results <- list()
for (line in poverty_lines) {
  a <- run_weighted_absolute_poverty(
    data_list = prep_data_ppp_adj,
    var_name = "mi",
    wgt_name = "new_wgt",
    daily_poverty_line = line
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("absolute_poverty_rate_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_poverty_rate: ", line))
}
absolute_poverty_rate <- bind_rows(results)



results <- list()
for (line in poverty_lines) {
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = FALSE
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("average_poverty_shortfall_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done average_poverty_shortfall_abs: ", line))
}
average_poverty_shortfall_abs <- bind_rows(results)


results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = TRUE
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("percentage_poverty_shortfall_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done percentage_poverty_shortfall_abs: ", line))
}
percentage_poverty_shortfall_abs <- bind_rows(results)


# Total shortfall absolute
results <- list()
for (line in poverty_lines) { 
  a <- total_shortfall_abs( 
  prep_data_ppp_adj,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("total_shortfall_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done total_shortfall_abs: ", line))
}
total_shortfall_abs_res <- bind_rows(results)



results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_gap_index(
  prep_data_ppp_adj,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("absolute_poverty_gap_index_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_poverty_gap_index: ", line))
}
absolute_poverty_gap_index <- bind_rows(results)


# Bind fourth set of indicators

absolute_poverty_figures <- list(
  number_poor_abs_res,
  absolute_poverty_rate,
  average_poverty_shortfall_abs,
  percentage_poverty_shortfall_abs,
  total_shortfall_abs_res,
  absolute_poverty_gap_index
) %>%
  bind_rows() %>% mutate(variable = "mi", equiv = "square root", year_ppp = 2017)

write_csv(absolute_poverty_figures, paste0(output_path_int, "absolute_poverty_mi_eqv.csv"))





# 5) Incomes across the Distribution 2021 PPPs --------------------------------------
average_21 <- prep_data_ppp_adj_2021 %>%
  run_weighted_mean("mi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(indicator = "Average")

median_21 <- prep_data_ppp_adj_2021 %>%
  run_weighted_percentiles("mi", "new_wgt", probs = c(0.5)) %>%
  structure_to_plot() %>%
mutate(indicator = "Median")

deciles_21 <- prep_data_ppp_adj_2021 %>%
  run_weighted_percentiles("mi", "new_wgt", probs = seq(0.1, 0.9, 0.1)) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("d_", str_sub(category, 1, -2))) %>%
  select(-category)



deciles_mean_21 <- prep_data_ppp_adj_2021 %>%
  run_weighted_percentiles(
    "mi",
    "new_wgt",
    probs = seq(0.1, 0.9, 0.1),
    average = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("decile_averages_p_", str_sub(category, 1, -2))) %>%
  select(-category) 


percentiles_21 <- prep_data_ppp_adj_2021 %>%
  run_weighted_percentiles("mi", "new_wgt", probs = seq(0.01, 0.99, 0.01)) %>%
  structure_to_plot() %>% 
  mutate(indicator = str_c("p_", str_sub(category, 1, -2))) %>%
  select(-category)


percentiles_mean_21 <- prep_data_ppp_adj_2021 %>%
  run_weighted_percentiles(
    "mi",
    "new_wgt",
    probs = seq(0.01, 0.99, 0.01),
    average = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("percentile_averages_p_", str_sub(category, 1, -2))) %>%
  select(-category)


incomes_across_distribution_21 <- list(
  average_21,
  median_21,
  deciles_21,
  deciles_shares, # not ppp adjusteed
  deciles_mean_21,
  percentiles_21,
  percentiles_shares, # not ppp adjusteed
  percentiles_mean_21
) %>%
  bind_rows() %>% mutate(variable = "mi", equiv = "square root", year_ppp = 2021)

write_csv(incomes_across_distribution_21, paste0(output_path_int, "incomes_across_distribution_21_mi_eqv.csv"))


# 6) Poverty (absolute) thresholds as daily monetary threshold in international dollars at 2021 PPPs -----------------------

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

# Number of poor 
results <- list()
for (line in poverty_lines) {
  a <- number_poor_abs( 
    data_list = prep_data_ppp_adj_2021,
    var_name = "mi",
    wgt_name = "new_wgt",
    daily_poverty_line = line
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("number_poor_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_number_of_poor: ", line))
}
number_poor_abs_21_res <- bind_rows(results)


results <- list()
for (line in poverty_lines) {
  a <- run_weighted_absolute_poverty(
    data_list = prep_data_ppp_adj_2021,
    var_name = "mi",
    wgt_name = "new_wgt",
    daily_poverty_line = line
  ) %>%
    structure_to_plot() %>%
    mutate(indicator = str_c("absolute_poverty_rate_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_poverty_rate: ", line))
}
absolute_poverty_rate_21 <- bind_rows(results)



results <- list()
for (line in poverty_lines) {
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj_2021,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = FALSE
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("average_poverty_shortfall_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done average_poverty_shortfall_abs: ", line))
}
average_poverty_shortfall_abs_21 <- bind_rows(results)


results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj_2021,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = TRUE
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("percentage_poverty_shortfall_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done percentage_poverty_shortfall_abs: ", line))
}
percentage_poverty_shortfall_abs_21 <- bind_rows(results)

# Total shortfall absolute
results <- list()
for (line in poverty_lines) { 
  a <- total_shortfall_abs( 
  prep_data_ppp_adj_2021,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("total_shortfall_abs_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done total_shortfall_abs: ", line))
}
total_shortfall_abs_21_res <- bind_rows(results)



results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_gap_index(
  prep_data_ppp_adj_2021,
  var_name = "mi",
  wgt_name = "new_wgt",
  daily_poverty_line = line
) %>%
  structure_to_plot() %>%
  mutate(indicator = str_c("absolute_poverty_gap_index_", line))
  results[[as.character(line)]] <- a
  print(paste0("Done absolute_poverty_gap_index: ", line))
}
absolute_poverty_gap_index_21 <- bind_rows(results)


# Bind fourth set of indicators

absolute_poverty_figures_21 <- list(
  number_poor_abs_21_res,
  absolute_poverty_rate_21,
  average_poverty_shortfall_abs_21,
  percentage_poverty_shortfall_abs_21,
  total_shortfall_abs_21_res,
  absolute_poverty_gap_index_21
) %>%
  bind_rows() %>% mutate(variable = "mi", equiv = "square root", year_ppp = 2021)

write_csv(absolute_poverty_figures_21, paste0(output_path_int, "absolute_poverty_21_mi_eqv.csv"))

