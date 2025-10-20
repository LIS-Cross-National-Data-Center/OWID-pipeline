# libraries ---------------------------------------------------------------

library(tidyverse)
library(lissyrtools)
library(assertthat)
library(tictoc)

# check if the package version is the most updated one
assertthat::assert_that(
  packageVersion("lissyrtools") == lissyrtools::check_github_version()
) # if TRUE we are sure to be working with the most updated set of countries and datasets as well.


# Auxilliary function `share_below_half_median()` non-existing in lissyrtools ------------------------------

share_below_half_median <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  type = c("type_4", "type_2"),
  na.rm = TRUE
) {
  output_share_below_half_median <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      half_median <- 0.5 *
        lissyrtools::compute_weighted_percentiles(
          var = var,
          wgt = wgt,
          probs = 0.5,
          type = type,
          na.rm = na.rm
        )

      if (na.rm) {
        valid <- !is.na(var) & !is.na(wgt)
        var <- var[valid]
        wgt <- wgt[valid]
      }

      ord <- order(var, wgt)
      var <- var[ord]
      wgt <- wgt[ord]

      cw <- cumsum(wgt)
      cxw <- cumsum(var * wgt)

      idx <- which(var >= half_median)[1] - 1

      var_l <- (var[idx])
      var_h <- (var[idx + 1])

      # the relative position in the segment between var_l and var_h
      gamma_Y <- (half_median - var_l) / (var_h - var_l)
      target_cw <- cw[idx] + (cw[idx + 1] - cw[idx]) * gamma_Y
      weight_half_median <- target_cw - cw[idx]

      final_result <- (cxw[idx] + half_median * weight_half_median) /
        cxw[length(var)] *
        100

      return(final_result)
    }
  )

  output_share_below_half_median <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_share_below_half_median
  )
  return(output_share_below_half_median)
}


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

# Preparation of the data (with and without PPP's) --------------------

# 1. Without PPP adjustment (including Taiwan and ru22 for example)
prep_data <- purrr::map(
  data_with_mi,
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_4") %>% # type = "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_4") %>% # type = "type_2" ?
  apply_sqrt_equivalisation("dhi") %>% # purrr::map( ~ .x %>% mutate(dhi = dhi / nhhmem)) %>% 
  apply_sqrt_equivalisation("mi") # # purrr::map( ~ .x %>% mutate(mi = mi / nhhmem))

# 2. With PPP adjustment (including Taiwan and ru22 for example)
prep_data_ppp_adj <- purrr::map(
  data_with_mi[(str_sub(names(data_with_mi),1,2) != "tw") & (str_sub(names(data_with_mi),1,4) != "ru22")],
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_ppp_adjustment("dhi", transformation = "lisppp", database = "lis", base_year_ppp = 2017) %>%
  apply_ppp_adjustment("mi", transformation = "lisppp", database = "lis", base_year_ppp = 2017) %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_4") %>% # type = "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_4") %>% # type = "type_2" ?
  apply_sqrt_equivalisation("dhi") %>%
  apply_sqrt_equivalisation("mi") 
# in terms of the preparation, we are on a very good path of alignment with OWID figures computed in Stata.




# Aggregate Figures (Inequalities)  --------------------------------------
gini <- prep_data %>% #list
  run_weighted_gini("dhi", "new_wgt") %>% # dedicated function, that only asks for a variable and a weight
  structure_to_plot() %>% # Function to restructure a list into a tidy data frame
  mutate(indicator = "Gini Index", variable = "dhi", equiv = "square root") # addition of extra columns

share_richest_10 <- prep_data %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = c(0.9), share = TRUE) %>%
  structure_to_plot() %>%
  filter(category == "90-100%") %>%
  select(-category) %>%
  mutate(indicator = "Share Top 10", variable = "dhi", equiv = "square root")

share_poorest_50 <- prep_data %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = c(0.5), share = TRUE) %>%
  structure_to_plot() %>%
  filter(category == "0-50%") %>%
  select(-category) %>%
  mutate(indicator = "Share Bottom 50", variable = "dhi", equiv = "square root")

palma_ratio <- prep_data %>%
  run_weighted_percentiles(
    "dhi",
    "new_wgt",
    probs = c(0.4, 0.9),
    share = TRUE
  ) %>%
  structure_to_plot() %>%
  filter(category %in% c("0-40%", "90-100%")) %>%
  pivot_wider(names_from = category, values_from = value) %>%
  mutate(value = `90-100%` / `0-40%`) %>%
  select(-`0-40%`, -`90-100%`) %>%
  mutate(indicator = "Palma Ratio", variable = "dhi", equiv = "square root")

share_below_50_median <- prep_data %>%
  share_below_half_median("dhi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(
    indicator = "Share below half median",
    variable = "dhi",
    equiv = "square root"
  )

ratio_90_10 <- prep_data %>%
  run_weighted_ratios(
    "dhi",
    "new_wgt",
    upper_percentile = 0.9,
    lower_percentile = 0.1
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = "Ratio p90_p10", variable = "dhi", equiv = "square root")

ratio_90_50 <- prep_data %>%
  run_weighted_ratios(
    "dhi",
    "new_wgt",
    upper_percentile = 0.9,
    lower_percentile = 0.5
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = "Ratio p90_p50", variable = "dhi", equiv = "square root")

ratio_50_10 <- prep_data %>%
  run_weighted_ratios(
    "dhi",
    "new_wgt",
    upper_percentile = 0.5,
    lower_percentile = 0.1
  ) %>%
  structure_to_plot() %>%
  mutate(indicator = "Ratio p50_p10", variable = "dhi", equiv = "square root")



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
  bind_rows()

write_csv(inequality_data, "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\inequality.csv")





# Aggregate Figures (Incomes across the Distribution)  --------------------------------------
average <- prep_data_ppp_adj %>%
  run_weighted_mean("dhi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(indicator = "Average", variable = "dhi", equiv = "square root")

median <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = c(0.5)) %>%
  structure_to_plot() %>%
  mutate(indicator = "Median", variable = "dhi", equiv = "square root")

deciles <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = seq(0.1, 0.9, 0.1)) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("d_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)

deciles_shares <- prep_data %>%
  run_weighted_percentiles(
    "dhi",
    "new_wgt",
    probs = seq(0.1, 0.9, 0.1),
    share = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("decile_shares_p_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)

deciles_mean <- prep_data_ppp_adj %>%
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


percentiles <- prep_data_ppp_adj %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = seq(0.01, 0.99, 0.01)) %>%
  structure_to_plot() %>% 
  mutate( 
    indicator = str_c("p_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)

percentiles_shares <- prep_data %>%
  run_weighted_percentiles(
    "dhi",
    "new_wgt",
    probs = seq(0.01, 0.99, 0.01),
    share = TRUE
  ) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("percentile_shares_p_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)

percentiles_mean <- prep_data_ppp_adj %>%
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
  bind_rows()

write_csv(incomes_across_distribution, "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\incomes_across_distribution.csv")





# Aggregate Figures - Poverty (relative) thresholds as percentage of the median  --------------------------------

thresholds <- c(
  0.4,
  0.5,
  0.6
)


results <- list()
for (line in thresholds) {
  a <- run_weighted_relative_poverty(
    prep_data,
    var_name = "dhi",
    wgt_name = "new_wgt",
    times_median = line
  ) %>%
    structure_to_plot() %>%
    mutate(
      indicator = str_c("relative_poverty_rate_", line),
      variable = "dhi",
      equiv = "square root"
    )
  results[[as.character(line)]] <- a
}
relative_poverty_rate <- bind_rows(results)



# average_poverty_shortfall_relt_to_median

results <- list()
for (line in thresholds) {
  a <- run_weighted_poverty_shortfall(
    prep_data,
    var_name = "dhi",
    wgt_name = "new_wgt",
    times_median = line,
    percent = FALSE
  ) %>%
    structure_to_plot() %>%
    mutate(
      indicator = str_c("average_poverty_shortfall_relt_to_median_", line),
      variable = "dhi",
      equiv = "square root"
    )
  results[[as.character(line)]] <- a
}
average_poverty_shortfall_relt_to_median <- bind_rows(results)

# percentage_poverty_shortfall_relt_to_median

results <- list()
for (line in thresholds) {
  a <- run_weighted_poverty_shortfall(
    prep_data,
    var_name = "dhi",
    wgt_name = "new_wgt",
    times_median = line,
    percent = TRUE
  ) %>%
    structure_to_plot() %>%
    mutate(
      indicator = str_c("percentage_poverty_shortfall_relt_to_median_", line),
      variable = "dhi",
      equiv = "square root"
    )
  results[[as.character(line)]] <- a
}

percentage_poverty_shortfall_relt_to_median <- bind_rows(results)

# relative_poverty_gap_index

results <- list()
for (line in thresholds) {
a <- run_weighted_poverty_gap_index(
  prep_data,
  var_name = "dhi",
  wgt_name = "new_wgt",
  times_median = line
) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("relative_poverty_gap_index_", line),
    variable = "dhi",
    equiv = "square root"
  )
  results[[as.character(line)]] <- a
}

relative_poverty_gap_index <- bind_rows(results)


# Bind third set of indicators

relative_poverty_figures <- list(
  relative_poverty_rate,
  average_poverty_shortfall_relt_to_median,
  percentage_poverty_shortfall_relt_to_median,
  relative_poverty_gap_index
) %>%
  bind_rows()

write_csv(relative_poverty_figures, "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\relative_poverty.csv")



# Aggregate Figures - Poverty (absolute) thresholds as daily monetary threshold in international dollars at 2017 PPPs -----------------------

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
absolute_poverty_rate <- bind_rows(results)



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
average_poverty_shortfall_abs <- bind_rows(results)


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
percentage_poverty_shortfall_abs <- bind_rows(results)



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
absolute_poverty_gap_index <- bind_rows(results)


# Bind fourth set of indicators

absolute_poverty_figures <- list(
  absolute_poverty_rate,
  average_poverty_shortfall_abs,
  percentage_poverty_shortfall_abs,
  absolute_poverty_gap_index
) %>%
  bind_rows()

write_csv(absolute_poverty_figures, "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\absolute_poverty.csv")
