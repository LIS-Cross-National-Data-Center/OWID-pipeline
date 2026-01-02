
# dhi - pc 

var_for_log = "dhi"
eqv_for_log = "pc"



print("==============================")
print(paste0("Initiated 2.2 at ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))
print("==============================")



# 1) Inequalities  --------------------------------------
gini <- prep_data %>% #list
  run_weighted_gini("dhi_pc", "new_wgt") %>% # dedicated function, that only asks for a variable and a weight
  structure_to_plot(print_columns = FALSE) %>% # Function to restructure a list into a tidy data frame
  mutate(indicator = "Gini Index") # addition of extra columns

print(paste0("Computation of Gini Index for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))

share_richest_10 <- prep_data %>%
  run_weighted_percentiles("dhi_pc", "new_wgt", probs = c(0.9), share = TRUE) %>%
  structure_to_plot(print_columns = FALSE) %>%
  filter(category == "90-100%") %>%
  select(-category) %>%
  mutate(indicator = "Share Top 10")

print(paste0("Computation of income share Richest 10% for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


share_poorest_50 <- prep_data %>%
  run_weighted_percentiles("dhi_pc", "new_wgt", probs = c(0.5), share = TRUE) %>%
  structure_to_plot(print_columns = FALSE) %>%
  filter(category == "0-50%") %>%
  select(-category) %>%
  mutate(indicator = "Share Bottom 50")

print(paste0("Computation of income share Poorest 50% for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


palma_ratio <- prep_data %>%
  run_weighted_percentiles(
    "dhi_pc",
    "new_wgt",
    probs = c(0.4, 0.9),
    share = TRUE
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  filter(category %in% c("0-40%", "90-100%")) %>%
  pivot_wider(names_from = category, values_from = value) %>%
  mutate(value = `90-100%` / `0-40%`) %>%
  select(-`0-40%`, -`90-100%`) %>%
  mutate(indicator = "Palma Ratio")

print(paste0("Computation of Palma Ratio for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))

# 23rd December 2025 email, apparently for the inequality dashboard they still use the poverty rate, not the income share.
#share_below_50_median <- prep_data %>%
#  share_below_half_median("dhi_pc", "new_wgt") %>%
#  structure_to_plot(print_columns = FALSE) %>%
#  mutate(indicator = "Share below half median")

#print(paste0("Computation of income share for hhd's below 50% Median for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))

share_below_50_median <- prep_data %>%
  run_weighted_relative_poverty("dhi_pc", "new_wgt", times_median = 0.5) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = "Share below half median")

print(paste0("Computation of population share for hhd's below 50% Median for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))

ratio_90_10 <- prep_data %>%
  run_weighted_ratios(
    "dhi_pc",
    "new_wgt",
    upper_percentile = 0.9,
    lower_percentile = 0.1
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = "Ratio p90_p10")

print(paste0("Computation of 90/10 Ratio for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



ratio_90_50 <- prep_data %>%
  run_weighted_ratios(
    "dhi_pc",
    "new_wgt",
    upper_percentile = 0.9,
    lower_percentile = 0.5
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = "Ratio p90_p50")

print(paste0("Computation of 90/50 Ratio for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


ratio_50_10 <- prep_data %>%
  run_weighted_ratios(
    "dhi_pc",
    "new_wgt",
    upper_percentile = 0.5,
    lower_percentile = 0.1
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = "Ratio p50_p10")

print(paste0("Computation of 50/10 Ratio for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))




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
  bind_rows() %>% mutate(variable = "dhi", equiv = "per capita", year_ppp = 2021)

write_csv(inequality_data, paste0(output_path_int, "inequality_dhi_pc.csv"))





# 2) Incomes across the Distribution  --------------------------------------
average <- prep_data_ppp_adj %>%
  run_weighted_mean("dhi_pc", "new_wgt") %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = "Average")

print(paste0("Computation of Average Income for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


median <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi_pc", "new_wgt", probs = c(0.5)) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = "Median")

print(paste0("Computation of Median Income for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


deciles <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi_pc", "new_wgt", probs = seq(0.1, 0.9, 0.1)) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("d_", str_sub(category, 1, -2))) %>%
  select(-category)

print(paste0("Computation of income Deciles for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


deciles_shares <- prep_data %>%
  run_weighted_percentiles(
    "dhi_pc",
    "new_wgt",
    probs = seq(0.1, 0.9, 0.1),
    share = TRUE
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("decile_shares_p_", str_sub(category, 1, -2))) %>%
  select(-category)

print(paste0("Computation of Deciles shares for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


deciles_mean <- prep_data_ppp_adj %>%
  run_weighted_percentiles(
    "dhi_pc",
    "new_wgt",
    probs = seq(0.1, 0.9, 0.1),
    average = TRUE
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("decile_averages_p_", str_sub(category, 1, -2))) %>%
  select(-category) 

print(paste0("Computation of Deciles averages for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



percentiles <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi_pc", "new_wgt", probs = seq(0.01, 0.99, 0.01)) %>%
  structure_to_plot(print_columns = FALSE) %>% 
  mutate(indicator = str_c("p_", str_sub(category, 1, -2))) %>%
  select(-category)

print(paste0("Computation of income percentiles for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


percentiles_shares <- prep_data %>%
  run_weighted_percentiles(
    "dhi_pc",
    "new_wgt",
    probs = seq(0.01, 0.99, 0.01),
    share = TRUE
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("percentile_shares_p_", str_sub(category, 1, -2))) %>%
  select(-category)

print(paste0("Computation of Percentile shares for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


percentiles_mean <- prep_data_ppp_adj %>%
  run_weighted_percentiles(
    "dhi_pc",
    "new_wgt",
    probs = seq(0.01, 0.99, 0.01),
    average = TRUE
  ) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("percentile_averages_p_", str_sub(category, 1, -2))) %>%
  select(-category)

print(paste0("Computation of Percentile averages for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))




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
  bind_rows() %>% mutate(variable = "dhi", equiv = "per capita", year_ppp = 2021)

write_csv(incomes_across_distribution, paste0(output_path_int, "incomes_across_distribution_dhi_pc.csv"))





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
    var_name = "dhi_pc",
    wgt_name = "new_wgt",
    times_median = line
  ) %>%
    structure_to_plot(print_columns = FALSE) %>%
    mutate(indicator = str_c("number_poor_relative_", line))
  results[[as.character(line)]] <- a

}
number_poor_relative_res <- bind_rows(results)
print(paste0("Computation of Number of Poor (reltv_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


# Poverty Rate 
results <- list()
for (line in thresholds) {
  a <- run_weighted_relative_poverty(
    prep_data,
    var_name = "dhi_pc",
    wgt_name = "new_wgt",
    times_median = line
  ) %>%
    structure_to_plot(print_columns = FALSE) %>%
    mutate(indicator = str_c("relative_poverty_rate_", line))
  results[[as.character(line)]] <- a
}
relative_poverty_rate <- bind_rows(results)
print(paste0("Computation of Poverty rate (reltv_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



# Average_poverty_shortfall_relt_to_median
results <- list()
for (line in thresholds) {
  a <- run_weighted_poverty_shortfall(
    prep_data_ppp_adj,
    var_name = "dhi_pc",
    wgt_name = "new_wgt",
    times_median = line,
    percent = FALSE
  ) %>%
    structure_to_plot(print_columns = FALSE) %>%
    mutate(indicator = str_c("average_poverty_shortfall_relt_to_median_", line))
  results[[as.character(line)]] <- a
}
average_poverty_shortfall_relt_to_median <- bind_rows(results)
print(paste0("Computation of Average Poverty Shortfall (reltv_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


# Percentage_poverty_shortfall_relt_to_median
results <- list()
for (line in thresholds) {
  a <- run_weighted_poverty_shortfall(
    prep_data,
    var_name = "dhi_pc",
    wgt_name = "new_wgt",
    times_median = line,
    percent = TRUE
  ) %>%
    structure_to_plot(print_columns = FALSE) %>%
    mutate(indicator = str_c("percentage_poverty_shortfall_relt_to_median_", line))
  results[[as.character(line)]] <- a
}

percentage_poverty_shortfall_relt_to_median <- bind_rows(results)
print(paste0("Computation of % Poverty Shortfall (reltv_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



# Total shortfall relative to the Median 
results <- list()
for (line in thresholds) {
  a <- total_shortfall_relative( 
    prep_data_ppp_adj,
    var_name = "dhi_pc",
    wgt_name = "new_wgt",
    times_median = line
  ) %>%
    structure_to_plot(print_columns = FALSE) %>%
    mutate(indicator = str_c("total_shortfall_relative_", line))
  results[[as.character(line)]] <- a
}

total_shortfall_relative_res <- bind_rows(results)
print(paste0("Computation of Total Shortfall (reltv_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



# relative_poverty_gap_index

results <- list()
for (line in thresholds) {
a <- run_weighted_poverty_gap_index(
  prep_data,
  var_name = "dhi_pc",
  wgt_name = "new_wgt",
  times_median = line
) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("relative_poverty_gap_index_", line))
  results[[as.character(line)]] <- a

}

relative_poverty_gap_index <- bind_rows(results)
print(paste0("Computation of Poverty Gap Index (reltv_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


# Bind third set of indicators

relative_poverty_figures <- list(
  number_poor_relative_res,
  relative_poverty_rate,
  average_poverty_shortfall_relt_to_median,
  percentage_poverty_shortfall_relt_to_median,
  total_shortfall_relative_res,
  relative_poverty_gap_index
) %>%
  bind_rows() %>% mutate(variable = "dhi", equiv = "per capita", year_ppp = 2021)

write_csv(relative_poverty_figures, paste0(output_path_int, "relative_poverty_dhi_pc.csv"))



# 4) Poverty (absolute) thresholds as daily monetary threshold in international dollars at 2021 PPPs -----------------------

poverty_lines <- c(
  3,
  4.20,
  8.30,
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
    var_name = "dhi_pc",
    wgt_name = "new_wgt",
    daily_poverty_line = line
  ) %>%
    structure_to_plot(print_columns = FALSE) %>%
    mutate(indicator = str_c("number_poor_abs_", line))
  results[[as.character(line)]] <- a
}
number_poor_abs_res <- bind_rows(results)
print(paste0("Computation of Number of Poor (abs_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


# Poverty Rate Absolute
results <- list()
for (line in poverty_lines) {
  a <- run_weighted_absolute_poverty(
    data_list = prep_data_ppp_adj,
    var_name = "dhi_pc",
    wgt_name = "new_wgt",
    daily_poverty_line = line
  ) %>%
    structure_to_plot(print_columns = FALSE) %>%
    mutate(indicator = str_c("absolute_poverty_rate_", line))
  results[[as.character(line)]] <- a
}
absolute_poverty_rate <- bind_rows(results)
print(paste0("Computation of Poverty rate (abs_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



results <- list()
for (line in poverty_lines) {
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj,
  var_name = "dhi_pc",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = FALSE
) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("average_poverty_shortfall_abs_", line))
  results[[as.character(line)]] <- a
}
average_poverty_shortfall_abs <- bind_rows(results)
print(paste0("Computation of Average Poverty Shortfall (abs_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_shortfall(
  prep_data_ppp_adj,
  var_name = "dhi_pc",
  wgt_name = "new_wgt",
  daily_poverty_line = line,
  percent = TRUE
) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("percentage_poverty_shortfall_abs_", line))
  results[[as.character(line)]] <- a
}
percentage_poverty_shortfall_abs <- bind_rows(results)
print(paste0("Computation of % Poverty Shortfall (abs_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))


# Total shortfall absolute
results <- list()
for (line in poverty_lines) { 
  a <- total_shortfall_abs( 
  prep_data_ppp_adj,
  var_name = "dhi_pc",
  wgt_name = "new_wgt",
  daily_poverty_line = line
) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("total_shortfall_abs_", line))
  results[[as.character(line)]] <- a
}
total_shortfall_abs_res <- bind_rows(results)
print(paste0("Computation of Total Shortfall (abs_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



results <- list()
for (line in poverty_lines) { 
  a <- run_weighted_poverty_gap_index(
  prep_data_ppp_adj,
  var_name = "dhi_pc",
  wgt_name = "new_wgt",
  daily_poverty_line = line
) %>%
  structure_to_plot(print_columns = FALSE) %>%
  mutate(indicator = str_c("absolute_poverty_gap_index_", line))
  results[[as.character(line)]] <- a
}
absolute_poverty_gap_index <- bind_rows(results)
print(paste0("Computation of Poverty Gap Index (abs_pvt) for ", var_for_log, " - ", eqv_for_log, " finished on ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))



# Bind fourth set of indicators

absolute_poverty_figures <- list(
  number_poor_abs_res,
  absolute_poverty_rate,
  average_poverty_shortfall_abs,
  percentage_poverty_shortfall_abs,
  total_shortfall_abs_res,
  absolute_poverty_gap_index
) %>%
  bind_rows() %>% mutate(variable = "dhi", equiv = "per capita", year_ppp = 2021)

write_csv(absolute_poverty_figures, paste0(output_path_int, "absolute_poverty_dhi_pc.csv"))


print("==============================")
print(paste0("Finished 2.2 at ", format(Sys.time(), "%d-%B-%Y %H:%M:%S")))
print("==============================")