#R/globals.R

#' @importFrom rlang .data
utils::globalVariables(c(
  #General variables
  "statistic", "p.value", "term", "estimate", "b", "row_type", "var_type",

  #anova_tbl variables
  "mean_val", "sd_val", "error", "lower", "upper", "Fstatistic", "p.value_1",

  #chisq_tbl & mcnemar_tbl variables
  "n_total", "chi_stat", "p_val_custom", "chisq_stats", "p_final", "n_row", "n_row_total",

  #paired_ttest_tbl variables
  "pre", "post", "Group_Temp", "Value_Temp", "conf.low", "conf.high", "pairedstat",

  #diagnostic_analysis variables
  "one_spec", "sen", "p.value_4"
))
