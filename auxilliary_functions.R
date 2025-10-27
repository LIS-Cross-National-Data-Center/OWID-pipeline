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

# Auxilliary function `number_poor_abs()` non-existing in lissyrtools ------------------------------
number_poor_abs <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  daily_poverty_line = 2.15,
  days_in_year = 365,
  na.rm = TRUE
) {

#   DISCLAIMER: the figures produced by this function might be slightly underestimated, we are computing population figures on a dataset where 
# households with missing 'dhi' were already deleted from the sample.  
  
  
  annual_poverty_line <- daily_poverty_line * days_in_year

  output_number_poors <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      df <- .x
      df$below_poverty <- ifelse(df[[var_name]] < annual_poverty_line, 1, 0)
      number_poors <- sum(df$below_poverty * .x[["hpopwgt"]] * .x[["nhhmem"]], na.rm = na.rm)
      return(number_poors)
    }
  )

  output_number_poors <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_number_poors
  )

  return(output_number_poors)
}

# Auxilliary function `total_shortfall_abs()` non-existing in lissyrtools ------------------------------
total_shortfall_abs <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  daily_poverty_line  = NULL,
  type = c("type_4", "type_2"),
  na.rm = TRUE
) {

#   DISCLAIMER: the figures produced by this function might be slightly underestimated, we are computing population figures on a dataset where 
# households with missing 'dhi' were already deleted from the sample.  

  output_run_poverty_shortfall_abs <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      poverty_line <- daily_poverty_line  * 365

      df <- .x
      df$below_poverty <- ifelse(df[[var_name]] < poverty_line, 1, 0)
      weighted_shortfall_abs <- sum(
        df$below_poverty * df$hpopwgt * df$nhhmem * (poverty_line - df[[var_name]])
      )
        return(weighted_shortfall_abs)
      }
  )

  output_run_poverty_shortfall_abs <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_run_poverty_shortfall_abs
  )
  return(output_run_poverty_shortfall_abs)
}

# Auxilliary function `number_poor_relative()` non-existing in lissyrtools ------------------------------
number_poor_relative <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  times_median = 0.5,
  type = c("type_4", "type_2"),
  na.rm = TRUE
) {

#   DISCLAIMER: the figures produced by this function might be slightly underestimated, we are computing population figures on a dataset where 
# households with missing 'dhi' were already deleted from the sample.  

  output_run_relative_poverty <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      poverty_line <- times_median *
        lissyrtools::compute_weighted_percentiles(
          var = var,
          wgt = wgt,
          probs = 0.5,
          type = type,
          na.rm = na.rm
        )

      df <- .x
      df$below_poverty <- ifelse(df[[var_name]] < poverty_line, 1, 0)
      number_poors <- sum(df$below_poverty * .x[["hpopwgt"]] * .x[["nhhmem"]], na.rm = na.rm)
      return(number_poors)
    }
  )
  output_run_relative_poverty <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_run_relative_poverty
  )
  return(output_run_relative_poverty)
}

# Auxilliary function `total_shortfall_relative()` non-existing in lissyrtools ------------------------------
total_shortfall_relative <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  times_median = 0.5,
  type = c("type_4", "type_2"),
  na.rm = TRUE
) {

#   DISCLAIMER: the figures produced by this function might be slightly underestimated, we are computing population figures on a dataset where 
# households with missing 'dhi' were already deleted from the sample.  

output_run_poverty_shortfall_relative <- purrr::imap(
    data_list,
    ~ {
      var <- .x[[var_name]]
      wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else rep(1, length(var))

      poverty_line <- times_median * lissyrtools::compute_weighted_percentiles(
            var = var,
            wgt = wgt,
            probs = 0.5,
            type = type,
            na.rm = na.rm
          )

      df <- .x
      df$below_poverty <- ifelse(df[[var_name]] < poverty_line, 1, 0)
      weighted_shortfall_relative <- sum(
        df$below_poverty * df$hpopwgt * df$nhhmem * (poverty_line - df[[var_name]])
      )
        return(weighted_shortfall_relative)
      }

  )

  output_run_poverty_shortfall_relative <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
    output_run_poverty_shortfall_relative
  )
  return(output_run_poverty_shortfall_relative)
}