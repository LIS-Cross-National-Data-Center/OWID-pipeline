# libraries ---------------------------------------------------------------

library(tidyverse)
library(lissyrtools)
library(assertthat)
library(tictoc)

# check if the package version is the most updated one
assertthat::assert_that(
  packageVersion("lissyrtools") == lissyrtools::check_github_version()
) # if TRUE we are sure to be working with the most updated set of countries and datasets as well.


# auxilliary function `share_below_half_median()` non-existing in lissyrtools ------

share_below_half_median <- function(
  data_list,
  var_name,
  wgt_name,
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


# loading the data  ---------------------------------------------------------------
variables_owid <- c("dhi", "hifactor", "hiprivate", "hi33", "grossnet")

tic()
full_data_hhd <- lissyuse(
  data = lissyrtools::get_countries_lis(),
  vars = variables_owid
) # 152.78 sec elapsed / 172.73
toc()

# prel analysis of how mi (market income) is conditioned according to OWID.  ---------------------------------------------------------------

# 1) the maximum values of its elements

# if `max_hifactor' > 0 & `max_hiprivate' > 0 & `max_hi33' > 0 {
#		  gen mi = hifactor + hiprivate + hi33
#	} else {
#		  gen mi = .
#}

# 2) interaction with `grossnet`

# if "`var'" == "mi" & ("`uniq_gross'" == "200" | "`uniq_gross'" == "300" | "`uniq_gross'" == "310" | "`uniq_gross'" == "320" | "`uniq_gross'" == ".") {
#			replace e`var'_b = .
#	}

check_mi_elements_max_and_grossnet <- map2(
  full_data_hhd,
  stringr::str_sub(names(full_data_hhd), 1, 4),
  ~ .x %>%
    summarise(
      dname = .y,
      max_hifactor = max(hifactor, na.rm = TRUE),
      max_hiprivate = max(hiprivate, na.rm = TRUE),
      max_hi33 = max(hi33, na.rm = TRUE),
      grossnet = mean(grossnet)
    )
) %>%
  list_rbind()

# check dname (rows) where at least one of the columns (maximums) is equal to zero
check_mi_elements_max_and_grossnet %>%
  mutate(
    has_at_least_one_zero = if_else(
      (max_hifactor == 0 | max_hiprivate == 0 | max_hi33 == 0),
      1,
      0
    )
  ) %>%
  filter(
    (has_at_least_one_zero == 1 | grossnet >= 200)
  ) %>%
  View()

# assess which dname have them all == 0 (more out of curiosity)
check_mi_elements_max_and_grossnet %>%
  mutate(sum_of_maxs = max_hifactor + max_hiprivate + max_hi33) %>%
  filter(sum_of_maxs == 0) # "us63" "us64" "us65" "us66" #


# also no Taiwan in the data

data_no_tw <- full_data_hhd[
  stringr::str_sub(names(full_data_hhd), 1, 2) != "tw"
]

data_with_mi <- purrr::map(
  data_no_tw,
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


prep_data <- purrr::map(
  data_with_mi,
  ~ .x %>%
    filter(!is.na(dhi)) %>%
    mutate(new_wgt = hwgt * nhhmem)
) %>%
  apply_ppp_adjustment("dhi", transformation = "lisppp", database = "lis") %>%
  apply_ppp_adjustment("mi", transformation = "lisppp", database = "lis") %>%
  apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3, type = "type_2") %>% # type = "type_2" ?
  apply_iqr_top_bottom_coding("mi", "hwgt", times = 3, type = "type_2") %>% # type = "type_2" ?
  apply_sqrt_equivalisation("dhi") %>%
  apply_sqrt_equivalisation("mi") 
# in terms of the preparation, we are on a very good path of alignment with OWID figures computed in Stata.

# Let us comapre the equivalized dhi values with the ones in Stata (using both `summarize` and `collapse` commands)

output_comparisson <- prep_data %>%
  run_weighted_mean("dhi", "new_wgt") %>%
  structure_to_plot() %>%
  rename(ey_R = value) %>%
  arrange(dname) %>%
  left_join(
    haven::read_dta(
      "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\means_summarize.dta"
    ),
    by = c("cname", "year", "dname")
  ) %>%
  rename(ey_summarize = ey) %>%
  left_join(
    haven::read_dta(
      "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\means_collapse.dta"
    ),
    by = c("cname", "year", "dname")
  ) %>%
  rename(ey_collapse = ey) %>%
  filter(dname != "ru22") %>%
  mutate(
    abs_dif_R_summarize = abs(ey_R - ey_summarize),
    abs_dif_R_collapse = abs(ey_R - ey_collapse),
    abs_dif_sumrz_collapse = abs(ey_summarize - ey_collapse)
  )

# Export
output_comparisson %>%
  readr::write_csv(
    "S:\\Staff\\Goncalo\\2025\\pipeline_OWID\\outputs\\comparisson_output_R_sum_collapse.csv"
  )

# error analysis
output_comparisson %>%
  summarise(
    mean_error_R_sum = mean(abs_dif_R_summarize),
    mean_error_R_collapse = mean(abs_dif_R_collapse),
    max_error_R_sum = max(abs_dif_R_summarize),
    max_error_R_collapse = max(abs_dif_R_collapse)
  ) # lissyrtools::run_weighted_percentiles (type = "type_2") is more aligned with `collapse`. `collapse` aligns better in practise with definition 2 from Hyndman & Fan 1996.

output_comparisson %>%
  filter(abs_dif_sumrz_collapse != 0) %>%
  select(-starts_with("abs_dif_R"))

#'# A tibble: 4 × 7
#'  cname          year dname   ey_R ey_summarize ey_collapse abs_dif_sumrz_collapse
#'1 Brazil         2015 br15  10439.       10439.      10439.                0.00293
#'2 Japan          2010 jp10  30113.       30113.      30113.                0.00195
#'3 Mexico         2005 mx05   8402.        8402.       8402.                0.146
#'4 United States  1969 us69  30469.       30469.      30469.                0.00195

# If we opt to proceed with type 2, (I honestly wouldn't for consistency with the method that we are going to use when computing the shares and averages by percentile/decile etc...)
# Future misalignments can still have their origin in the differences betweem the results of `summarize , det` (to get the percentiles in Stata) and the `collapse` command.
# This might occur in the top-bottom coding stage, during the preparation, or during the computation of the aggreagte figure itself.

# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------

# Using prep_data without "ru22h"
prep_data <- prep_data[!(names(prep_data)) == "ru22h"]

# Inequality

# 1) Gini

gini <- prep_data %>%
  run_weighted_gini("dhi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(indicator = "Gini Index", variable = "dhi", equiv = "square root")

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

share_below_50_median <- prep_data[1:4] %>%
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


average <- prep_data %>%
  run_weighted_mean("dhi", "new_wgt") %>%
  structure_to_plot() %>%
  mutate(indicator = "Average", variable = "dhi", equiv = "square root")

median <- prep_data %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = c(0.5)) %>%
  structure_to_plot() %>%
  mutate(indicator = "Median", variable = "dhi", equiv = "square root")

deciles <- prep_data[1:4] %>%
  run_weighted_percentiles("dhi", "new_wgt", probs = seq(0.1, 0.9, 0.1)) %>%
  structure_to_plot() %>%
  mutate(
    indicator = str_c("d_", str_sub(category, 1, -2)),
    variable = "dhi",
    equiv = "square root"
  ) %>%
  select(-category)

deciles_shares <- prep_data[1:4] %>%
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
  )

deciles_mean <- prep_data[1:4] %>%
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
  )


percentiles <- prep_data %>%
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
  )

percentiles_mean <- prep_data %>%
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
  )


# Poverty (relative) thresholds as percentage of the median  --------------------------------

thresholds <- c(
  0.4,
  0.5,
  0.6
)


results <- list()
for (line in thresholds) {
  a <- run_weighted_relative_poverty(
    prep_data[1:3],
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
run_weighted_poverty_shortfall(
  prep_data,
  var_name = "dhi",
  wgt_name = "new_wgt",
  times_median = line,
  percent = FALSE
) %>%
  mutate(
    indicator = str_c("average_poverty_shortfall_relt_to_median_", line),
    variable = "dhi",
    equiv = "square root"
  )

# percentage_poverty_shortfall_relt_to_median
run_weighted_poverty_shortfall(
  prep_data,
  var_name = "dhi",
  wgt_name = "new_wgt",
  times_median = line,
  percent = TRUE
) %>%
  mutate(
    indicator = str_c("percentage_poverty_shortfall_relt_to_median_", line),
    variable = "dhi",
    equiv = "square root"
  )

# relative_poverty_gap_index
run_weighted_poverty_gap_index(
  prep_data,
  var_name = "dhi",
  wgt_name = "new_wgt",
  times_median = line
) %>%
  mutate(
    indicator = str_c("relative_poverty_gap_index_", line),
    variable = "dhi",
    equiv = "square root"
  )


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
    data_list = prep_data[1:3],
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
}
final_df <- bind_rows(results)
print(final_df)


run_weighted_absolute_poverty(
  prep_data,
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

run_weighted_poverty_shortfall(
  prep_data,
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
run_weighted_poverty_shortfall(
  prep_data,
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


run_weighted_poverty_gap_index(
  prep_data,
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




all_relative_poverty_metrics
all_absolute_poverty_metrics