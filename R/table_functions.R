# descriptive table ----

#' Descriptive statistic test
#'
#' @description A descriptive table describing demographic or simple variable
#' @param data Data frame
#' @param group_var Grouping variable (optional)
#' @param included_var Variable to be included from the data frame
#' @param continuous_vars Variables to be presented as continuous (mean and SD)
#' @param non_normal_vars Variables to be presented as continuous but reported as median and IQR
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A descriptive table
#' @importFrom rlang .data .env
#' @examples
#' descriptive_tbl(
#'   data = mjms_data,
#'   group_var = "Treatment_Group",
#'   included_var = c("Age", "BMI", "Sex", "Smoker"),
#'   non_normal_vars = c("Age"),
#'   table_caption = "Baseline Patient Characteristics",
#'   abbreviation = "BMI = Body Mass Index, SBP = Systolic Blood Pressure"
#' )
#' @export
descriptive_tbl <- function(data, group_var = NULL, included_var, continuous_vars = NULL,
                            non_normal_vars = NULL,
                            table_caption = "Patient Demographics",
                            abbreviation = NULL){

  if (!is.null(group_var) && !group_var %in% names(data)) {
    stop("The grouping variable was not found in the dataset.", call. = FALSE)
  }

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {

      # 1. Base lists: all_continuous() MUST come first
      stat_list <- list(
        gtsummary::all_continuous() ~ "{mean} ({sd})",
        gtsummary::all_categorical() ~ "{n} ({p})"
      )

      digits_list <- list(
        gtsummary::all_continuous() ~ c(1, 1),
        gtsummary::all_categorical() ~ c(0, 1)
      )

      # 2. Append non-normal overrides so they evaluate LAST and override all_continuous
      if (!is.null(non_normal_vars)) {
        stat_list <- c(
          stat_list,
          list(dplyr::all_of(non_normal_vars) ~ "{median} ({IQR})")
        )
        # Note: {median} and {IQR} are two numbers, so the base c(1,1) from
        # all_continuous() works perfectly and does not need to be overridden.
      }

      # 3. Define the columns to select
      cols_to_select <- included_var
      if (!is.null(group_var)) {
        cols_to_select <- c(group_var, cols_to_select)
      }

      tbl_desc <- data |>
        dplyr::select(dplyr::all_of(cols_to_select))|>
        gtsummary::tbl_summary(
          by = if (!is.null(group_var)) dplyr::all_of(group_var) else NULL,
          missing_text = "Missing",
          statistic = stat_list,
          digits = digits_list
        )

      # 4. Conditionally add overall and format headers
      if (!is.null(group_var)) {
        tbl_desc <- tbl_desc |>
          gtsummary::add_overall(last = TRUE) |>
          gtsummary::modify_header(
            label = "**Variables**",
            stat_0 = "**Total**\n_n_ (%)",
            gtsummary::all_stat_cols(stat_0 = FALSE) ~ "**{level}** \n(\n_n_ = {n})"
          )
      } else {
        tbl_desc <- tbl_desc |>
          gtsummary::modify_header(
            label = "**Variables**",
            stat_0 = "**Total**\n_n_ (%)"
          )
      }

      # 5. Process caption and clear old footnotes
      final_desc <- tbl_desc |>
        gtsummary::modify_caption(paste0(" **Table :** ", table_caption, " (_n_ = {N})")) |>
        gtsummary::remove_footnote_header()

      # 6. Apply Footnotes
      if (is.null(non_normal_vars)) {
        final_desc <- final_desc |>
          gtsummary::modify_footnote_body(
            footnote = "Mean (SD)",
            columns = "label",
            rows = .data$var_type == "continuous" & .data$row_type == "label"
          )
      } else {
        final_desc <- final_desc |>
          gtsummary::modify_footnote_body(
            footnote = "Mean (SD)",
            columns = "label",
            rows = .data$var_type == "continuous" & !(.data$variable %in% .env$non_normal_vars) & .data$row_type == "label"
          ) |>
          gtsummary::modify_footnote_body(
            footnote = "Median (IQR)",
            columns = "label",
            rows = .data$var_type == "continuous" & (.data$variable %in% .env$non_normal_vars) & .data$row_type == "label"
          )
      }

      # 7. GT conversions and source notes
      final_desc <- final_desc |>
        gtsummary::as_gt() |>
        gt::opt_footnote_marks(marks = "letters")

      if(!is.null(abbreviation)){
        final_desc <- final_desc |>
          gt::tab_source_note(source_note = paste0("Abbreviation: ", abbreviation))
      }

      return(final_desc)
    }
  )
}


# independent t-test ----

#' Independent t-test
#'
#' @description An Independent t-test output table
#' @param data Data frame
#' @param outcome_var Continuous output variable
#' @param group_var Grouping variable consisting of two levels
#' @param equal_var TRUE(t-test) FALSE(Welch t-test)
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return An independent t-test output table
#' @importFrom stats t.test as.formula
#' @importFrom rlang .data
#' @examples
#' ttest_tbl(
#'   data = mjms_data,
#'   outcome_var = "BMI",
#'   group_var = "Sex",
#'   equal_var = TRUE,
#'   table_caption = "Comparison of BMI by Biological Sex",
#'   abbreviation = "BMI = body mass index"
#' )
#'
#' @export
ttest_tbl <- function(data, outcome_var, group_var, equal_var = TRUE,
                      table_caption = "Comparison between groups",
                      abbreviation = NULL){

  if (!all(c(outcome_var, group_var) %in% names(data))) {
    stop("Outcome or grouping variable missing from data.", call. = FALSE)
  }

  ttest_out <- stats::t.test(
    stats::as.formula(paste(outcome_var, "~", group_var)),
    data = data,
    var.equal= equal_var
  )

  tstat <- unname(ttest_out$statistic)
  tdf <- unname(ttest_out$parameter)
  tstat_format <- if (equal_var) "%.3f (%.0f)" else "%.3f (%.2f)"

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      ttest_tbl <- data |>
        dplyr::select(dplyr::all_of(c(outcome_var, group_var)))|>
        gtsummary::tbl_summary(
          by = dplyr::all_of(group_var),
          type = list(gtsummary::all_continuous() ~ "continuous"),
          statistic = list(gtsummary::all_continuous() ~ "{mean} ({sd})"),
          digits = list(gtsummary::all_continuous() ~ c(2,2))
        )|>
        gtsummary::add_difference(
          estimate_fun = list(gtsummary::all_continuous() ~ gtsummary::label_style_number(digits = c(2,2))),
          test = gtsummary::all_continuous() ~ "t.test",
          test.args = gtsummary::all_continuous() ~ list(var.equal = equal_var),
          pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
        )|>
        gtsummary::modify_spanning_header(c("stat_1","stat_2") ~ "**Mean (SD)**") |>
        gtsummary::modify_post_fmt_fun(
          fmt_fun = ~ gsub(",?\\s*Mean \\(SD\\)", "", .),
          columns = "label"
        )|>
        gtsummary::modify_post_fmt_fun(
          fmt_fun = ~ gsub (" to ", ", ", .),
          columns = "estimate"
        )|>
        gtsummary::modify_header(
          label = "**Variable**",
          stat_1 = "**{level}** \n\n_n_ = {n}",
          stat_2 = "**{level}** \n\n_n_ = {n}",
          estimate = "**Mean   \ndifference   \n(****95%  CI****)**",
          p.value = "_P_**-value**"
        )|>
        gtsummary::modify_table_body(
          ~ .x |>
            dplyr::mutate(
              tstatistic = sprintf(tstat_format, tstat, tdf)
            )|>
            dplyr::relocate("tstatistic", .before = "p.value") |>
            dplyr::relocate("stat_2", .before = "stat_1")
        )|>
        gtsummary::modify_header(tstatistic = "**_t_-statistic  \n(df)**")|>
        gtsummary::modify_caption(paste0("**Table :** ", table_caption))

      footnote_text <- ifelse(equal_var, "Independent _t_-test.", "Welch's _t_-test.")

      final_ttest <- ttest_tbl |>
        gtsummary::remove_footnote_header()|>
        gtsummary::remove_abbreviation("CI = Confidence Interval")|>
        gtsummary::modify_footnote_header(
          footnote = footnote_text,
          columns = "p.value"
        )

      if (!is.null(abbreviation)) {
        final_ttest <- final_ttest |> gtsummary::modify_abbreviation(abbreviation)
      }

      final_ttest <- final_ttest|>
        gtsummary::as_gt()|>
        gt::opt_footnote_marks(marks = "letters")

      return(final_ttest)
    }
  )
}

# paired t-test ----

#' Paired t-test
#'
#' @description A Paired t-test output table
#' @param data Data frame
#' @param id_var ID variable or unique identifier
#' @param pre_var Pre variable continuous
#' @param post_var Post variable continuous
#' @param outcome_var Continuous outcome variable
#' @param variable_label New label for outcome variable
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A Paired t-test output table
#' @importFrom rlang .data as_label enquo
#' @importFrom stats t.test
#' @examples
#' paired_ttest_tbl(
#'   data = mjms_data,
#'   id_var = "PatientID",
#'   pre_var = "SBP_Left_Arm",
#'   post_var = "SBP_Right_Arm",
#'   outcome_var = "SBP_Right_Arm",
#'   variable_label = "Systolic Blood Pressure (mmHg)",
#'   table_caption = "Comparison of SBP between Left and Right Arms"
#' )
#'
#' @export
paired_ttest_tbl <- function(data, id_var, pre_var, post_var, outcome_var = NULL, variable_label = NULL,
                             table_caption = "Comparison between paired group",
                             abbreviation = NULL) {

  if (!all(c(id_var, pre_var, post_var) %in% names(data))) {
    stop("ID, Pre, or Post variables missing from dataset.", call. = FALSE)
  }

  col1_name <- pre_var
  col2_name <- post_var

  if(is.null(variable_label)){
    variable_label <- col1_name
  }

  vec1 <- data[[pre_var]]
  vec2 <- data[[post_var]]

  ttest_result <- stats::t.test(vec1, vec2, paired = TRUE)
  pstat <- unname(ttest_result$statistic)
  pDf <- unname(ttest_result$parameter)

  temp_long <- data|>
    dplyr::select(id = dplyr::all_of(id_var), pre = dplyr::all_of(pre_var), post = dplyr::all_of(post_var))|>
    tidyr::pivot_longer(cols = c("pre", "post"),
                        names_to = "Group_Temp",
                        values_to = "Value_Temp")

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      paired_tbl <- temp_long |>
        gtsummary::tbl_summary(
          by = "Group_Temp",
          include = "Value_Temp",
          label = list("Value_Temp" ~ variable_label),
          statistic = "Value_Temp" ~ "{mean} ({sd})",
          digits = "Value_Temp" ~ c(2,2)
        )|>
        gtsummary::add_difference(
          group = "id",
          test = "Value_Temp" ~ "paired.t.test"
        )|>
        gtsummary::modify_column_merge(
          pattern = "{estimate} ({conf.low}, {conf.high})",
          rows = !is.na(.data$estimate)
        )|>
        gtsummary::modify_column_hide(columns = c("conf.low", "conf.high"))|>
        gtsummary::modify_post_fmt_fun(
          fmt_fun = ~ gsub(" to ", ", ", .),
          columns = "estimate"
        )|>
        gtsummary::modify_spanning_header(
          c("stat_1","stat_2") ~ "**Mean (SD)** \n_n_ = {n}"
        )|>
        gtsummary::modify_header(
          label = "**Variable**",
          stat_1 = paste0("**", col1_name, "**"),
          stat_2 = paste0("**", col2_name, "**"),
          estimate = "**Mean  \ndifference  \n(****95% CI****)**",
          p.value = "_P_**-value**"
        )|>
        gtsummary::modify_caption(paste0("**Table :** ", table_caption))

      paired_tbl <- paired_tbl |>
        gtsummary::modify_table_body(
          ~ .x |>
            dplyr::mutate(
              pairedstat = sprintf("%.3f (%.0f)", pstat, pDf)
            )|>
            dplyr::relocate("pairedstat", .before = "p.value")|>
            dplyr::relocate("stat_2", .before = "stat_1")
        )|>
        gtsummary::modify_header(pairedstat = "**_t_-statistic  \n(df)**")

      paired_final <- paired_tbl |>
        gtsummary::remove_footnote_header()|>
        gtsummary::remove_abbreviation("CI = Confidence Interval")

      if(!is.null(abbreviation)){
        paired_final <- paired_final |> gtsummary::modify_abbreviation(abbreviation)
      }

      paired_final <- paired_final |>
        gtsummary::modify_footnote_header(
          footnote = "Paired _t_-test.",
          columns = "p.value"
        )|>
        gtsummary::as_gt()|>
        gt::opt_footnote_marks(marks = "letters")

      return(paired_final)
    }
  )
}

# one-way ANOVA----

#' One-way ANOVA analysis
#'
#' @description A One-Way ANOVA output table with post-hoc adjustment
#' @param data Data frame
#' @param outcome_var Continuous outcome variable
#' @param group_var Grouping variable with more than 2 groups
#' @param var_equal TRUE(ANOVA) FALSE(Welch ANOVA)
#' @param outcome_label New label for outcome variable
#' @param table_caption Caption for the table in string
#' @param posthoc_method P-value adjustment for post-hoc test
#' @param abbreviation Full name of abbreviated variables
#' @param show_plot TRUE(plot) FALSE(no plot)
#' @return A One-Way ANOVA output table with/without plot graph
#' @importFrom stats aov as.formula pairwise.t.test sd
#' @importFrom rlang := .data
#' @examples
#' anova_tbl(
#'   data = mjms_data,
#'   outcome_var = "Cholesterol",
#'   group_var = "Treatment_Group",
#'   outcome_label = "Serum Cholesterol",
#'   show_plot = TRUE
#' )
#'
#' @export
anova_tbl <- function(
    data, outcome_var, group_var, var_equal = TRUE, outcome_label = NULL,
    table_caption = "Comparison of means between groups",
    posthoc_method = "bonferroni", abbreviation = NULL, show_plot = FALSE){

  out_name <- outcome_var
  grp_name <- group_var

  data <- data|>
    dplyr::mutate(!!grp_name := as.factor(.data[[grp_name]]))

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      tbl_left <- data |>
        gtsummary::tbl_continuous(
          variable = dplyr::all_of(out_name),
          include = dplyr::all_of(grp_name),
          statistic = ~ "{mean} ({sd})",
          digits = ~ c(2,2)
        )|>
        gtsummary::add_p(
          test = list(gtsummary::all_continuous() ~ "oneway.test"),
          test.args = gtsummary::all_continuous() ~ list(var.equal = var_equal)
        )|>
        gtsummary::modify_column_hide("statistic")

      tbl_right <- data |>
        gtsummary::tbl_summary(include = dplyr::all_of(grp_name), statistic = ~ "{n}")|>
        gtsummary::modify_header(stat_0 = "**_n_**",label = "")

      aov_fit <- stats::aov(stats::as.formula(paste(out_name, "~", grp_name)), data = data)
      aov_tidy <- broom::tidy(aov_fit)

      ph_test <- stats::pairwise.t.test(data[[out_name]], data[[grp_name]],
                                        p.adjust.method = posthoc_method)
      ph_tidy <- broom::tidy(ph_test)|>
        dplyr::filter(.data$p.value < 0.05)

      ph_text <- if (nrow(ph_tidy) > 0){
        paste("Sig. differences (p < 0.05):", paste(ph_tidy$group1, "vs",
                                                    ph_tidy$group2, collapse = "; "))
      } else {"No sig. differences." }

      final_merge <- gtsummary::tbl_merge(
        tbls = list(tbl_left, tbl_right),
        tab_spanner = FALSE
      )|>
        gtsummary::modify_table_body(
          ~ .x|>
            dplyr::mutate(
              Fstatistic = sprintf("%.2f  \n(%d, %d)", aov_tidy$statistic[1],
                                   aov_tidy$df[1], aov_tidy$df[2]),
              Fstatistic = ifelse(dplyr::row_number() == 1, .data$Fstatistic, "")
            )|>
            dplyr::relocate("stat_0_2", .before = "stat_0_1") |>
            dplyr::relocate("Fstatistic", .before = "p.value_1")
        )|>
        gtsummary::modify_header(
          label = "**Groups**",
          stat_0_1 = paste0("**", outcome_label, "** \n**Mean (SD)**"),
          Fstatistic = "_F_**-statistic** \n(df1,df2)",
          p.value_1 = "_P_**-value**"
        )|>
        gtsummary::remove_footnote_header()|>
        gtsummary::modify_post_fmt_fun(
          fmt_fun = ~ gsub(",?\\s*n", "", .),
          columns = "label"
        )|>
        gtsummary::modify_caption(paste0("**Table :** ", table_caption))|>
        gtsummary::modify_footnote_header(
          footnote = "One-way ANOVA.",
          columns = c("Fstatistic", "p.value_1")
        )

      if (!is.null(abbreviation)) {
        final_merge <- final_merge |> gtsummary::modify_abbreviation(abbreviation)
      }
      final_merge <- final_merge|>
        gtsummary::as_gt()|>
        gt::tab_footnote(footnote = ph_text,
                         locations = gt::cells_body(columns = "p.value_1", rows = 1)
        )|>
        gt::opt_footnote_marks(marks = "letters")

      if(show_plot){
        plot_data <- data |>
          dplyr::group_by(.data[[grp_name]]) |>
          dplyr::summarise(
            n = dplyr::n(),
            mean_val = mean(.data[[out_name]], na.rm = TRUE),
            sd_val = stats::sd(.data[[out_name]], na.rm = TRUE),
            error = 1.96 * (.data$sd_val / sqrt(.data$n)),
            lower = .data$mean_val - .data$error,
            upper = .data$mean_val + .data$error
          )

        final_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[grp_name]], y = .data$mean_val)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), width = 0.1,
                        color = "blue", linewidth = 0.8) +
          ggplot2::geom_point(shape = 21, size = 4, fill = "white", color =  "black") +
          ggplot2::geom_text(ggplot2::aes(y = .data$lower - (0.05 * .data$mean_val), label = paste0("n = ", .data$n)),
                    size = 3.5) +
          ggplot2::labs(title = "Mean Plot with 95% CI", x = grp_name, y = out_name) +
          ggplot2::theme_minimal()

        return(list(table = final_merge, plot = final_plot))
      }

      return(final_merge)
    }
  )
}

# linear regression ----

#' Linear regression test
#'
#' @description A Linear regression output table for SLR or MLR
#' @param data Data frame
#' @param outcome_var Outcome variable as a string
#' @param predictor_vars Predictor variables as a character vector
#' @param var_labels New labels for variables in string
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A linear regression output table
#' @importFrom stats lm as.formula
#' @importFrom rlang .data
#' @examples
#' lm_tbl(
#'   data = mjms_data,
#'   outcome_var = "Cholesterol",
#'   predictor_vars = c("Age", "BMI", "Smoker"),
#'   var_labels = list(Age ~ "Age (Years)", BMI ~ "Body Mass Index"),
#'   table_caption = "Multiple Linear Regression for Cholesterol"
#' )
#'
#' @export
lm_tbl <- function(data, outcome_var, predictor_vars, var_labels = NULL,
                   table_caption = "Factors associated with outcome",
                   abbreviation = NULL){

  out_name <- outcome_var
  pred_names <- predictor_vars

  frmla <- stats::as.formula(paste(out_name, "~", paste(pred_names, collapse =  " + ")))
  model <- stats::lm(frmla, data = data)

  t_tbl <- broom::tidy(model) |>
    dplyr::select("term", "statistic") |>
    dplyr::mutate(tstat = sprintf("%.3f", .data$statistic)) |>
    dplyr::select("term", "tstat")

  r_squared <- summary(model)$r.squared

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      linear_tbl <- model |>
        gtsummary::tbl_regression(
          label = var_labels,
          pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 3)
        )|>
        gtsummary::modify_header(
          label = "**Factors**",
          estimate = "**Adjusted** _b_**(95% CI)**",
          p.value = "_P_**-value**"
        )|>
        gtsummary::modify_post_fmt_fun(
          fmt_fun = ~ gsub(" to ", ", ", .),
          columns = "estimate"
        )|>
        gtsummary::modify_table_body(
          ~ .x |>
            dplyr::left_join(t_tbl, by = "term") |>
            dplyr::mutate(tstat = ifelse(is.na(.data$tstat) & !is.na(.data$variable), "-", .data$tstat)) |>
            dplyr::relocate("tstat", .before = "p.value")
        )|>
        gtsummary::modify_column_unhide("tstat")|>
        gtsummary::modify_header(tstat = "**_t_-statistic**")|>
        gtsummary::modify_caption(paste0("**Table :** ", table_caption, " (_n_ = {N})."))

      linear_final <- linear_tbl |>
        gtsummary::remove_abbreviation("CI = Confidence Interval")|>
        gtsummary::modify_footnote_header(
          footnote = "Adjusted regression coefficients,",
          columns = "estimate"
        )|>
        gtsummary::modify_footnote_header(
          footnote = paste0("Multiple linear regression (R\u00b2 =",
                            sprintf("%.3f", r_squared), ")."),
          columns = "p.value"
        )|>
        gtsummary::as_gt()|>
        gt::opt_footnote_marks(marks = "letters")

      if(!is.null(abbreviation)){
        linear_final <- linear_final |> gt::tab_source_note(source_note = abbreviation)
      }

      return(linear_final)
    }
  )
}

# logistic regression ----

#' Logistic regression test
#'
#' @description A logistic regression output table
#' @param data Data frame
#' @param outcome_var Binary output variable as a string
#' @param predictor_vars Predictor variables as a character vector
#' @param ref_levels List name of reference levels (optional)
#' @param cat_vars Categorical variables that wish to see the reference variable
#' @param var_labels List name of variable labels
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A logistic regression output table
#' @importFrom stats glm binomial as.formula relevel
#' @importFrom rlang .data
#' @examples
#' logistic_tbl(
#' data = mjms_data,
#' outcome_var = "Heart_Disease",
#' predictor_vars = c("Age", "Sex", "Smoker"),
#' ref_levels = list(Smoker = "No", Sex = "Female"),
#' cat_vars = c("Sex", "Smoker"),
#' table_caption = "Risk Factors for Heart Disease"
#' )
#'
#' @export
logistic_tbl <- function(data, outcome_var, predictor_vars, ref_levels = NULL,
                         var_labels = NULL, cat_vars = NULL,
                         table_caption = "Associated factors", abbreviation = NULL){

  out_name <- outcome_var
  pred_names <- predictor_vars

  if(!is.null(ref_levels)){
    for (v in names(ref_levels)) {
      if(v %in% names(data)){
        data[[v]] <- stats::relevel(as.factor(data[[v]]), ref = ref_levels[[v]])
      }
    }
  }

  model_formula <- stats::as.formula(
    paste(out_name, "~", paste(pred_names, collapse = " + "))
  )

  model <- stats::glm(model_formula, data = data, family = stats::binomial(link = "logit"))

  beta_tbl <- broom::tidy(model)|>
    dplyr::select("term", "estimate", "statistic") |>
    dplyr::mutate(
      b = sprintf("%.2f", .data$estimate),
      zstat = sprintf("%.3f", .data$statistic)
    )|>
    dplyr::select("term", "b", "zstat")

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      logit_tbl <- model|>
        gtsummary::tbl_regression(
          exponentiate = TRUE,
          tidy_fun = broom.helpers::tidy_parameters,
          type = if(!is.null(cat_vars)) list(dplyr::all_of(cat_vars) ~ "categorical") else NULL,
          label = var_labels,
          pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 3)
        )|>
        gtsummary::modify_header(
          label = "**Factors**",
          estimate = "**Adjusted OR    \n(95% CI)**",
          p.value = "_P_**-value**"
        )|>
        gtsummary::modify_post_fmt_fun(
          fmt_fun = ~ gsub(" to ", ", ", .),
          columns = "estimate"
        )|>
        gtsummary::modify_table_body(
          ~ .x|>
            dplyr::left_join(beta_tbl, by = "term")|>
            dplyr::mutate(
              b = ifelse(is.na(.data$b) & !is.na(.data$variable), "-", .data$b),
              zstat = ifelse(is.na(.data$zstat) & !is.na(.data$variable), "-", .data$zstat)
            )|>
            dplyr::relocate("b", .before = "estimate")|>
            dplyr::relocate("zstat", .before = "p.value")
        )|>
        gtsummary::modify_column_unhide(c("b", "zstat"))|>
        gtsummary::modify_header(b = "**_b_**", zstat = "**Wald statistic**")|>
        gtsummary::modify_caption(paste0("**Table :** ", table_caption," (_n_ = {N})."))

      final_logit <- logit_tbl|>
        gtsummary::modify_footnote_header(
          footnote = "Likelihood ratio test.",
          columns = "p.value"
        )|>
        gtsummary::modify_footnote_body(
          footnote = "Reference category.",
          rows = .data$b == "-"
        )|>
        gtsummary::remove_abbreviation("CI = Confidence Interval")|>
        gtsummary::remove_abbreviation("OR = Odds Ratio")

      final_logit <- final_logit|>
        gtsummary::as_gt()|>
        gt::opt_footnote_marks(marks = "letters")

      if(!is.null(abbreviation)){
        final_logit <- final_logit |> gt::tab_source_note(source_note = abbreviation)
      }

      return(final_logit)
    }
  )
}

# mc nemar ----

#' McNemar test
#'
#' @description A MC Nemar output table
#' @param data Data frame
#' @param pre_var Pre variable
#' @param post_var Post variable
#' @param table_caption Caption for the table in string
#' @param header_pre Header for row variable
#' @param header_post Header for column variable
#' @return A Mc Nemar output table
#' @importFrom stats mcnemar.test setNames
#' @importFrom rlang .data :=
#' @examples
#' mcnemar_tbl(
#' data = mjms_data,
#' pre_var = "Symptom_Pre",
#' post_var = "Symptom_Post",
#' table_caption = "Symptom status pre- and post-treatment.",
#' header_pre = "Pre Symptom",
#' header_post = "Post Symptom"
#' )
#'
#' @export
mcnemar_tbl <- function(
    data, pre_var, post_var, table_caption = "**Association between pre and post**",
    header_pre = "Pre", header_post = "Post"){

  pre_nm <- pre_var
  post_nm <- post_var

  levs <- sort(union(unique(data[[pre_nm]]), unique(data[[post_nm]])))

  data_fixed <- data|>
    dplyr::mutate(
      !!pre_nm := factor(.data[[pre_nm]], levels = levs),
      !!post_nm := factor(.data[[post_nm]], levels = levs)
    )

  mcnemar_out <- stats::mcnemar.test(table(data[[pre_nm]], data[[post_nm]]), correct = TRUE)

  chi_val <- mcnemar_out$statistic
  df_val <- mcnemar_out$parameter
  p_val <- mcnemar_out$p.value

  row_counts <- data_fixed|>
    dplyr::count(label = as.character(.data[[pre_nm]]))|>
    dplyr::rename(n_row_total = "n")

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      mcn_tbl <- data |>
        dplyr::select(dplyr::all_of(c(pre_nm, post_nm)))|>
        gtsummary::tbl_summary(
          by = dplyr::all_of(post_nm),
          include = dplyr::all_of(pre_nm),
          label = stats::setNames(list(header_pre), pre_nm),
          type = stats::setNames(list("categorical"), pre_nm),
          percent = "cell",
          statistic = list(gtsummary::all_categorical() ~ "{n} ({p})"),
          digits = list(gtsummary::all_categorical() ~ c(0,1))
        )|>
        gtsummary::modify_table_body(
          ~ .x |>
            dplyr::left_join(row_counts, by = "label")|>
            dplyr::mutate(
              n_total = ifelse(.data$row_type == "level", as.character(.data$n_row_total), NA),
              chisq_stats = ifelse(.data$row_type == "label",
                                   sprintf("%.2f (%d)", chi_val, df_val), NA),
              p_final = ifelse(.data$row_type == "label",
                               ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val)), NA)
            )|>
            dplyr::relocate("n_total", "chisq_stats", "p_final", .after = dplyr::last_col())
        )|>
        gtsummary::modify_column_unhide(columns = c("n_total", "chisq_stats", "p_final"))|>
        gtsummary::modify_header(
          gtsummary::all_stat_cols() ~ "**{level}** \n_n_ (%)",
          label = paste0("**", header_pre, "**"),
          n_total = "**_n_**",
          chisq_stats = "\u03c7\u00b2 **-statistic** \n(df)",
          p_final = "_P_**-value**"
        )|>
        gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ paste0("**",header_post,"**"))|>
        gtsummary::modify_caption(paste0("**Table :** ", table_caption))|>
        gtsummary::remove_footnote_header()|>
        gtsummary::modify_footnote_header(
          footnote = "McNemar's Chi-squared test with continuity correction.",
          columns = "p_final"
        )|>
        gtsummary::as_gt()|>
        gt::opt_footnote_marks(marks = "letters")

      return(mcn_tbl)
    }
  )
}

# chi-square ----

#' Chi-square test
#'
#' @description A Chi-square output table
#' @param data Data frame
#' @param exposure_var Categorical exposure variable
#' @param outcome_var Categorical outcome variable
#' @param table_caption Caption for the table in string
#' @return A Chi-square output table
#' @importFrom stats chisq.test
#' @importFrom rlang .data
#' @examples
#' chisq_tbl(
#'   data = mjms_data,
#'   exposure_var = "Sex",
#'   outcome_var = "Smoker",
#'   table_caption = "Association of Gender vs. Smoking Status."
#' )
#'
#' @export
chisq_tbl <- function(
    data, exposure_var, outcome_var, table_caption = "**Association between exposure and outcome**"){

  exp_name <- exposure_var
  out_name <- outcome_var

  chi_out <- stats::chisq.test(table(data[[exp_name]], data[[out_name]]))
  chi_val <- chi_out$statistic
  df_val <- chi_out$parameter
  p_val <- chi_out$p.value

  row_counts <- data|>
    dplyr::count(.data[[exp_name]])|>
    dplyr::mutate(label = as.character(.data[[exp_name]]))|>
    dplyr::rename(n_row = "n")

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      chi_tbl <- data |>
        gtsummary::tbl_summary(
          by = dplyr::all_of(out_name),
          include = dplyr::all_of(exp_name),
          statistic = gtsummary::all_categorical() ~ "{n} ({p})",
          digits = gtsummary::all_categorical() ~ c(0,1)
        )

      chi_tbl <- chi_tbl|>
        gtsummary::modify_table_body(
          ~ .x |>
            dplyr::left_join(row_counts, by = "label") |>
            dplyr::mutate(
              n_total = ifelse(.data$row_type == "label", NA, .data$n_row),
              chi_stat = ifelse(.data$row_type == "label",
                                sprintf("%.2f (%d)", chi_val, df_val), NA),
              p_val_custom = ifelse(.data$row_type == "label",
                                    ifelse(p_val < 0.001, "< 0.001", sprintf("%.3f", p_val)), NA))|>
            dplyr::relocate("n_total", "chi_stat", "p_val_custom", .after = dplyr::last_col()))|>
        gtsummary::modify_column_unhide(columns = c("n_total", "chi_stat", "p_val_custom"))|>
        gtsummary::modify_header(
          label = "**Variable**",
          gtsummary::all_stat_cols() ~ "**{level}** \n_n_ (%)",
          n_total = "**_n_**",
          chi_stat = "\u03c7\u00b2 **-statistic** \n(df)",
          p_val_custom = "_P_**-value**"
        )|>
        gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ paste0("**", out_name,"**"))|>
        gtsummary::modify_caption(paste0("**Table :** ", table_caption))|>
        gtsummary::remove_footnote_header()|>
        gtsummary::modify_footnote_header(
          footnote = "Chi-square test for independence.",
          columns = "p_val_custom"
        )|>
        gtsummary::as_gt()|>
        gt::opt_footnote_marks(marks = "letters")

      return(chi_tbl)
    }
  )
}

# diagnostic test ----

#' Diagnostic test
#'
#' @description A diagnostic test output table with/without ROC Curve
#' @param data Data frame
#' @param status_var Target variable (Gold standard)
#' @param marker_map Define index tests/markers, friendly labels and cutoff points
#' @param show_plot TRUE(plot) FALSE(no plot)
#' @param plot_marker Specific test visualize in the ROC plot
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A diagnostic table with/without ROC plot
#' @importFrom pROC roc coords ci.auc ci.coords
#' @importFrom ggplot2 ggplot aes geom_abline geom_step scale_x_continuous scale_y_continuous labs theme_minimal
#' @importFrom rlang .data
#' @examples
#' med_marker <- list(
#' Cholesterol   = list(label = "Serum Cholesterol (Cutoff: 200 mg/dL)", cutoff = 200),
#' BMI           = list(label = "Body Mass Index (Cutoff: 25 kg/m2)", cutoff = 25),
#' SBP_Left_Arm  = list(label = "Systolic BP - Left (Cutoff: 140 mmHg)", cutoff = 140)
#' )
#' diagnostic_tbl(
#'  data = mjms_data,
#'  status_var = "Gold_Standard",
#'  marker_map = med_marker, # list of all index markers
#'  show_plot = TRUE,
#'  plot_marker = "Cholesterol", # show plot for Cholesterol
#'  table_caption = "Diagnostic Accuracy of Cardiovascular Markers.",
#'  abbreviation = c("AUC = Area Under Curve", "SBP = Systolic Blood Pressure")
#' )
#'
#' @export
diagnostic_tbl <- function(
    data, status_var, marker_map, show_plot = FALSE, plot_marker = NULL,
    table_caption = "Diagnostic performance", abbreviation = NULL){

  marker_cols <- names(marker_map)
  marker_labels <- stats::setNames(lapply(marker_map, function(x) x$label), marker_cols)

  diag_stats <- function(data_sub, variable, type){
    if (!status_var %in% names(data_sub)) return(tibble::tibble(complex = "Error: No Status"))

    status_data <- data_sub[[status_var]]
    marker_data <- data_sub[[variable]]
    cutoff <- marker_map[[variable]]$cutoff

    status_f <- factor(status_data)
    lvls <- rev(levels(status_f))

    roc_val <- pROC::roc(status_f, marker_data, quiet = TRUE, levels = lvls)

    val <- "NA"
    if (type == "sens"){
      res <- pROC::coords(roc_val, x = cutoff, input = "threshold", ret = "sensitivity", transpose = FALSE)
      ci_val <- suppressMessages(pROC::ci.coords(roc_val, x = cutoff, input = "threshold", ret = "sensitivity"))
      val <- paste0(round(res$sensitivity * 100, 1), " (",
                    round(ci_val[[1]][1] * 100, 1), ", ", round(ci_val[[1]][3] * 100, 1), ")")
    }

    if (type == "spec"){
      res <- pROC::coords(roc_val, x = cutoff, input = "threshold", ret = "specificity", transpose = FALSE)
      ci_val <- suppressMessages(pROC::ci.coords(roc_val, x = cutoff, input = "threshold", ret = "specificity"))
      val <- paste0(round(res$specificity * 100, 1), " (",
                    round(ci_val[[1]][1] * 100, 1), ", ", round(ci_val[[1]][3] * 100, 1), ")")
    }

    if (type == "auc"){
      ci_val <- as.numeric(pROC::ci.auc(roc_val))
      val <- paste0(round(ci_val[2], 2), " (", round(ci_val[1], 2), ", ", round(ci_val[3], 2), ")")
    }
    return(tibble::tibble(complex = val))
  }

  gtsummary::with_gtsummary_theme(
    gtsummary::theme_gtsummary_journal("jama"),
    {
      t_sens <- data |>
        dplyr::select(dplyr::all_of(c(status_var, marker_cols))) |>
        gtsummary::tbl_custom_summary(
          include = dplyr::all_of(marker_cols), label = marker_labels,
          stat_fns = ~ function(data, variable, ...) diag_stats(data, variable, "sens"),
          statistic = ~ "{complex}")|>
        gtsummary::modify_header(stat_0 = "**Sensitivity (%)   \n(95% CI)**")

      t_spec <- data |>
        dplyr::select(dplyr::all_of(c(status_var, marker_cols))) |>
        gtsummary::tbl_custom_summary(
          include = dplyr::all_of(marker_cols), label = marker_labels,
          stat_fns = ~ function(data, variable, ...) diag_stats(data, variable, "spec"),
          statistic = ~ "{complex}") |>
        gtsummary::modify_header(stat_0 = "**Specificity (%)   \n(95% CI)**")

      t_auc <- data |>
        dplyr::select(dplyr::all_of(c(status_var, marker_cols))) |>
        gtsummary::tbl_custom_summary(
          include = dplyr::all_of(marker_cols), label = marker_labels,
          stat_fns = ~ function(data, variable, ...) diag_stats(data, variable, "auc"),
          statistic = ~ "{complex}") |>
        gtsummary::modify_header(stat_0 = "**AUC (95% CI)**")

      t_pval <- data |>
        dplyr::select(dplyr::all_of(c(status_var, marker_cols)))|>
        gtsummary::tbl_summary(by = dplyr::all_of(status_var), label = marker_labels)|>
        gtsummary::add_p(test= list(gtsummary::all_continuous() ~ "wilcox.test"))|>
        gtsummary::modify_column_hide(dplyr::all_of(c("stat_1", "stat_2")))|>
        gtsummary::modify_header(p.value = "_P_**-value**")

      final_diag <- gtsummary::tbl_merge(
        tbls = list(t_sens, t_spec, t_auc, t_pval),
        tab_spanner = FALSE
      )|>
        gtsummary::modify_header(label = "**Variable (cutoff)**")|>
        gtsummary::modify_abbreviation(paste(abbreviation, collapse = "; "))|>
        gtsummary::remove_footnote_header()|>
        gtsummary::modify_footnote_header(footnote = "Null hypothesis: true area = 0.5.",
                                          columns = "p.value_4")|>
        gtsummary::as_gt()|>
        gt::opt_footnote_marks(marks = "letters")|>
        gt::tab_caption(caption = paste0(table_caption, " (n = ", nrow(data), ")"))

      plot_diag <- NULL
      if (show_plot){
        target_marker <- if (is.null(plot_marker)) marker_cols[1] else plot_marker
        roc_objs <- pROC::roc(data[[status_var]], data[[target_marker]], ci = TRUE, quiet = TRUE)
        plot_df <- data.frame(sen = roc_objs$sensitivities, one_spec = 1 - roc_objs$specificities)

        plot_diag <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$one_spec, y = .data$sen)) +
          ggplot2::geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "dashed") +
          ggplot2::geom_step(color = "turquoise", linewidth = 1.2) +
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
          ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
          ggplot2::labs(
            title = paste("ROC Curve:", marker_map[[target_marker]]$label),
            x = "1 - Specificity", y = "Sensitivity"
          ) +
          ggplot2::theme_minimal()
        return(list(table = final_diag, plot = plot_diag))
      }

      return(final_diag)
    }
  )
}

# Pearson correlation ----

#' Pearson's correlation
#'
#' @description A Pearson's correlation output table with/without a correlation plot
#' @param data Data frame
#' @param included_var Variable to be included from the data frame
#' @param show_plot TRUE(plot) FALSE(no plot)
#' @param plot_x Variable to be x-axis
#' @param plot_y Variable to be y-axis
#' @param abbreviation Full name of abbreviated variables
#' @param table_caption Caption for the table in string
#' @return A Pearson's Correlation output table
#' @importFrom stats sd cor cor.test
#' @importFrom rlang .data
#' @examples
#' pearson_tbl(
#'   data = mjms_data,
#'   included_var = c("Age", "BMI", "SBP_Left_Arm", "SBP_Right_Arm"),
#'   show_plot = TRUE,
#'   plot_x = "SBP_Left_Arm",
#'   plot_y = "SBP_Right_Arm",
#'   abbreviation = "BMI = body mass index, SBP = systolic blood pressure",
#'   table_caption = "Correlation between variables."
#' )
#'
#' @export
pearson_tbl <- function(
    data, included_var = NULL, show_plot = FALSE, plot_x = NULL, plot_y = NULL,
    abbreviation = NULL, table_caption =""){

  if (is.null(included_var)){
    corr_subset <- data |> dplyr::select(dplyr::where(is.numeric))
  } else {
    corr_subset <- data |> dplyr::select(dplyr::all_of(included_var)) |> dplyr::select(dplyr::where(is.numeric))
  }

  var_names <- colnames(corr_subset)
  n_var <- ncol(corr_subset)
  results <- list()

  sd_vals <- apply(corr_subset, 2, stats::sd, na.rm = TRUE)
  r_mat <- stats::cor(corr_subset, method = "pearson", use = "complete.obs")
  p_mat <- matrix(NA, n_var, n_var)

  for (i in seq_len(n_var)) {
    for (j in seq_len(n_var)) {
      p_mat[i, j] <- stats::cor.test(corr_subset[[i]], corr_subset[[j]])$p.value
    }
  }

  tabs <- matrix("", ncol = n_var, nrow = n_var)
  colnames(tabs) <- var_names

  display_names <- if(!is.null(abbreviation)){
    sapply(var_names, function(x) if(x %in% names(abbreviation)) abbreviation[[x]] else x)
  } else {
    var_names
  }
  rownames(tabs) <- display_names

  for (i in seq_len(n_var)) {
    for (j in seq_len(n_var)) {
      if(i == j){
        tabs[i, j] <- sprintf("%.3f", sd_vals[i])
      } else if(i < j){
        p_val <- p_mat[i, j]
        tabs[i, j] <- ifelse(p_val < 0.001, "< 0.001", sprintf("%.3f", p_val))
      } else {
        tabs[i, j] <- sprintf("%.3f", r_mat[i, j])
      }
    }
  }

  gt_table <- as.data.frame(tabs)|>
    tibble::rownames_to_column(var = "Variables")|>
    gt::gt(rowname_col = "Variables")|>
    gt::tab_header(title = gt::md(paste0("**", table_caption, "** (n = ", nrow(data), ")")))|>
    gt::tab_stubhead(label = "Variables")|>
    gt::cols_align(align = "center", gt::everything())

  for (i in seq_len(n_var)) {
    for (j in seq_len(n_var)) {
      target_col <- var_names[j]
      if (i == j) {
        gt_table <- gt_table|>
          gt::tab_footnote(footnote = "Standard Deviation (SD)",
                           locations = gt::cells_body(columns = dplyr::all_of(target_col), rows = i))
      } else if (i < j){
        gt_table <- gt_table|>
          gt::tab_footnote(footnote = "P-value",
                           locations = gt::cells_body(columns = dplyr::all_of(target_col), rows = i))
      } else {
        gt_table <- gt_table|>
          gt::tab_footnote(footnote = "Pearson correlation coefficient (r)",
                           locations = gt::cells_body(columns = dplyr::all_of(target_col), rows = i))
      }
    }
  }

  if (!is.null(abbreviation)){
    if (!is.null(names(abbreviation)) && all(names(abbreviation) != "")){
      abbr_text <- paste(names(abbreviation), abbreviation, sep = " = ", collapse = "; ")
    } else {
      abbr_text <- paste(abbreviation, collapse = "; ")
    }
    gt_table <- gt_table |> gt::tab_source_note(source_note = gt::md(abbr_text))
  }

  results$table <- gt_table |>
    gt::opt_footnote_marks(marks = "letters")|>
    gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_stub())|>
    gt::tab_options(
      table.font.size = gt::px(14), column_labels.font.weight = "bold",
      heading.title.font.size = gt::px(16), table.border.top.style = "none",
      table.border.bottom.color = "black", column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black", table.width = gt::pct(90)
    )|>
    gt::opt_row_striping()

  if (show_plot && !is.null(plot_x) && !is.null(plot_y)){
    ctest <- stats::cor.test(data[[plot_x]], data[[plot_y]])

    results$plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[plot_x]], y = .data[[plot_y]])) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::labs(
        title = paste("Relationship between", plot_x, "and", plot_y),
        caption = paste0("r = ", round(ctest$estimate, 2),
                         " (P ", ifelse(ctest$p.value < 0.001, "< 0.001)",
                                        paste0( "= ", round(ctest$p.value, 3), ")")))
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, face = "bold"))
  }

  return(if (show_plot) results else results$table)
}
