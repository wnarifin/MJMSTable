
# descriptive table ----

#' Descriptive statistic test
#'
#' @description A descriptive table describing demographic or simple variable
#' @param data Data frame
#' @param group_var Grouping variable
#' @param included_var Variable to be included from the data frame
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A descriptive table
#' @examples
#' descriptive_tbl(
#'   data = mjms_data,
#'   group_var = Treatment_Group,
#'   included_var = c(Age, BMI, Sex, Smoker),
#'   table_caption = "Baseline Patient Characteristics",
#'   abbreviation = "BMI = Body Mass Index, SBP = Systolic Blood Pressure"
#' )
#' @export
descriptive_tbl <- function(data, group_var, included_var,
                            table_caption = "Patient Demographics",
                            abbreviation = NULL){

 group_var_quo <- rlang::enquo(group_var)
 included_var_quo <- rlang::enquo(included_var)

 reset_gtsummary_theme()
 #theme_gtsummary_journal("jama")

 tbl_desc <- data |>
   dplyr::select(!!group_var_quo, !!included_var_quo)|>
   tbl_summary(
     by = !!group_var_quo,
     type = list(all_continuous() ~ "continuous"),
     statistic = list(
       all_continuous() ~ "{mean} ({sd})",
       all_categorical() ~ "{n} ({p})"
     ),
     digits = list(
       all_continuous() ~ c(1,1),
       all_categorical() ~ c(0,1)
     )
   )|>
   add_overall(last = T)|>
   modify_header(
     label = "**Variables**",
     stat_0 = "**Total**
              \n_n_ (%)",
     stat_1 = "**{level}**  \n(\n_n_ = {n})",
     stat_2 = "**{level}**  \n(\n_n_ = {n})"
   )|>
   modify_caption(paste0(" **Table :** ", table_caption, "(_n_ = {N})"))

 final_desc <- tbl_desc |>
   remove_footnote_header() |>
   modify_footnote_body(
     footnote = "Mean (SD)",
     columns = "label",
     rows = var_type == "continuous" & row_type == "label"
   )|>
   as_gt() |>
   gt::opt_footnote_marks(marks = "letters")
 if(!is.null(abbreviation)){
   final_desc <- final_desc |>
     tab_source_note(source_note = paste0("Abbreviation: ", abbreviation))
 }
 return(final_desc)
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
#' @examples
#' ttest_tbl(
#'   data = mjms_data,
#'   outcome_var = BMI,
#'   group_var = Sex,
#'   equal_var = TRUE,
#'   table_caption = "Comparison of BMI by Biological Sex",
#'   abbreviation = "BMI = body mass index"
#' )
#'
#' @export
ttest_tbl <- function(data, outcome_var, group_var, equal_var = TRUE,
                      table_caption = "Comparison between groups",
                      abbreviation = NULL){

  outcome_quo <- rlang::enquo(outcome_var)
  group_var_quo <- rlang::enquo(group_var)

  ttest_out <- t.test(
    stats::as.formula(
      paste(rlang::as_name(outcome_quo), "~", rlang::as_name(group_var_quo))
    ),
    data = data,
    var.equal= equal_var
  )

  tstat <- unname(ttest_out$statistic)
  tdf <- unname(ttest_out$parameter)

  #reset_gtsummary_theme()
  theme_gtsummary_journal("jama")

  ttest_tbl <- data |>
    dplyr::select(!!outcome_quo, !!group_var_quo)|>
    tbl_summary(
      by = !!group_var_quo,
      type = list(all_continuous() ~ "continuous"),
      statistic = list(all_continuous() ~ "{mean} ({sd})"),
      digits = list(all_continuous() ~ c(2,2))
    )|>
    add_difference(
      estimate_fun = list(all_continuous() ~ label_style_number(digits = c(2,2))
                          ),
  test = all_continuous() ~ "t.test"
    )|>

    modify_spanning_header(
      c("stat_1","stat_2") ~ "**Mean (SD)**"
    ) |>
    modify_post_fmt_fun(
      fmt_fun = ~ gsub(",?\\s*Mean \\(SD\\)", "", .),
      columns = "label"
    )|>
    modify_post_fmt_fun(
      fmt_fun = ~ gsub (" to ", ", ", .),
      columns = "estimate"
    )|>
    modify_header(
      label = "**Variable**",
      stat_1 = "**{level}**   \n\n_n_ = {n}",
      stat_2 = "**{level}**  \n\n_n_ = {n}",
      estimate = "**Mean   \ndifference   \n(****95%  CI****)**",
      p.value = "_P_**-value**"
    )|>

    modify_table_body(
      ~ .x |>
        dplyr::mutate(
          tstatistic = sprintf("%.2f (%.0f)", tstat, tdf)
        )|>
        dplyr::relocate(tstatistic, .before = p.value) |>
        dplyr::relocate(stat_2, .before = stat_1)
    )|>
    modify_header(
      tstatistic = "**_t_-statistic  \n(df)**"
    )|>
    modify_caption(
      paste0("**Table :** ", table_caption)
    )

  final_ttest <- ttest_tbl |>
    remove_footnote_header()|>
    remove_abbreviation("CI = Confidence Interval")|>
    modify_footnote_header(
      footnote = "Independent _t_-test.",
      columns = "p.value"
    )

    if (!is.null(abbreviation)) {
      final_ttest <- final_ttest |>
      modify_abbreviation(abbreviation)
    }

  final_ttest <- final_ttest|>
    as_gt()|>
    gt::opt_footnote_marks(marks = "letters")

  return(final_ttest)
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
#' @importFrom rlang as_label enquo
#' @importFrom stats t.test
#' @examples
#' paired_ttest_tbl(
#'   data = mjms_data,
#'   id_var = PatientID,
#'   pre_var = SBP_Left_Arm,
#'   post_var = SBP_Right_Arm,
#'   outcome_var = SBP_Right_Arm,
#'   variable_label = "Systolic Blood Pressure (mmHg)",
#'   table_caption = "Comparison of SBP between Left and Right Arms"
#' )
#'
#' @export
paired_ttest_tbl <- function(data, id_var, pre_var, post_var, outcome_var, variable_label,
                             table_caption = "Comparison between paired group",
                             abbreviation = NULL) {

  col1_name <- as_label(enquo(pre_var))
  col2_name <- as_label(enquo(post_var))

  if(is.null(variable_label)){
    variable_label <- col1_name
  }

  vec1 <- data |> pull({{pre_var}})
  vec2 <- data |> pull({{post_var}})


  ttest_result <- t.test(vec1, vec2, paired = T)
  pstat <- unname(ttest_result$statistic)
  pDf <- unname(ttest_result$parameter)

  temp_long <- data|>
    select(id ={{id_var}}, pre = {{pre_var}}, post = {{post_var}})|>
    pivot_longer(cols = c(pre, post),
                 names_to = "Group_Temp",
                 values_to = "Value_Temp")

  #reset_gtsummary_theme()
  #theme_gtsummary_journal("jama")

  paired_tbl <- temp_long |>
    tbl_summary(
      by = Group_Temp,
      include = Value_Temp,
      label = list(Value_Temp ~ variable_label),
      statistic = Value_Temp ~ "{mean} ({sd})",
      digits = Value_Temp ~ c(2,2)
    )|>
    add_difference(
      group = id,
      test = Value_Temp ~ "paired.t.test"
    )|>
    modify_column_merge(
      pattern = "{estimate} ({conf.low}, {conf.high})",
      rows = !is.na(estimate)
    )|>
    modify_column_hide(columns = c(conf.low, conf.high))|>
    modify_post_fmt_fun(
      fmt_fun = ~ gsub(" to ", ", ", .),
      columns = "estimate"
    )|>
     modify_spanning_header(
       c("stat_1","stat_2") ~ "**Mean (SD)**   \n_n_ = {n}"
     )|>
    modify_header(
      label = "**Variable**",
      stat_1 = paste0("**", col1_name, "**"),
      stat_2 = paste0("**", col2_name, "**"),
      estimate = "**Mean  \ndifference  \n(****95% CI****)**",
      p.value = "_P_**-value**"
    )|>
    modify_caption(
      paste0("**Table :** ", table_caption)
    )

  paired_tbl <- paired_tbl |>
    modify_table_body(
      ~ .x |>
        dplyr::mutate(
          pairedstat = sprintf("%2.f (%.0f)", pstat, pDf)
        )|>
        dplyr::relocate(pairedstat, .before = p.value)|>
        dplyr::relocate(stat_2, .before = stat_1)
    )|>
    modify_header(
      pairedstat = "**_t_-statistic  \n(df)**"
    )

  paired_final <- paired_tbl |>
    remove_footnote_header()|>
    remove_abbreviation("CI = Confidence Interval")

  if(!is.null(abbreviation)){
    paired_final <- paired_final |>
      modify_abbreviation(abbreviation)
  }

  paired_final <- paired_final |>
    modify_footnote_header(
      footnote = "Paired _t_-test.",
      columns = "p.value"
    )|>
    as_gt()|>
    gt::opt_footnote_marks(marks = "letters")

  return(paired_final)
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
#' @examples
#' anova_tbl(
#'   data = mjms_data,
#'   outcome_var = Cholesterol,
#'   group_var = Treatment_Group,
#'   outcome_label = "Serum Cholesterol",
#'   show_plot = TRUE
#' )
#'
#' @export
anova_tbl <- function(
    data,
    outcome_var,
    group_var,
    var_equal = TRUE,
    outcome_label = NULL,
    table_caption = "Comparison of means between groups",
    posthoc_method = "bonferroni",
    abbreviation = NULL,
    show_plot = FALSE){

  out_name <- rlang::as_name(rlang::ensym(outcome_var))
  grp_name <- rlang::as_name(rlang::ensym(group_var))

  data <- data|>
    dplyr::mutate(!!grp_name := as.factor(.data[[grp_name]]))

  reset_gtsummary_theme()
  theme_gtsummary_journal("jama")

  tbl_left <- data |>
    tbl_continuous(
      variable = all_of(out_name),
      include = all_of(grp_name),
      statistic = ~ "{mean} ({sd})",
      digits = ~ c(2,2)
      )|>
    add_p(
      test = list(all_continuous() ~ "oneway.test"),
      test.args = all_continuous() ~ list(var.equal = var_equal)
      )|>
    modify_column_hide(statistic)

  tbl_right <- data |>
    tbl_summary(include = all_of(grp_name), statistic = ~ "{n}")|>
    modify_header(stat_0 = "**_n_**",label = "")

  aov_fit <- aov(as.formula(paste(out_name, "~", grp_name)), data = data)
  aov_tidy <- broom::tidy(aov_fit)

  ph_test <- pairwise.t.test(data[[out_name]], data[[grp_name]],
                             p.adjust.method = posthoc_method)
  ph_tidy <- broom::tidy(ph_test)|>
    dplyr::filter(p.value < 0.05)
  ph_text <- if (nrow(ph_tidy) > 0){
    paste("Sig. differences (p < 0.05):", paste(ph_tidy$group1, "vs",
                                                ph_tidy$group2, collapse = "; "))
      } else {"No sig. differences." }

  final_merge <- tbl_merge(
    tbls = list(tbl_left, tbl_right),
    tab_spanner = F
  )|>
    modify_table_body(
      ~ .x|>
        dplyr::mutate(
          Fstatistic = sprintf("%.2f  \n(%d, %d)", aov_tidy$statistic[1],
                               aov_tidy$df[1], aov_tidy$df[2]),
          Fstatistic = ifelse(dplyr::row_number() == 1, Fstatistic, "")
        )|>
        dplyr::relocate(stat_0_2, .before = stat_0_1) |>
        dplyr::relocate(Fstatistic, .before = p.value_1)
    )|>

    modify_header(
      label = "**Groups**",
      stat_0_1 = paste0("**", outcome_label, "**    \n**Mean (SD)**"),
      Fstatistic = "_F_**-statistic**   \n(df1,df2)",
      p.value_1 = "_P_**-value**"
    )|>
    remove_footnote_header()|>
    modify_post_fmt_fun(
      fmt_fun = ~ gsub(",?\\s*n", "", .),
      columns = "label"
    )|>
    modify_caption(
      paste0("**Table :** ", table_caption)
    )|>
    modify_footnote_header(
      footnote = "One-way ANOVA.",
      columns = c("Fstatistic", "p.value_1")
    )

  if (!is.null(abbreviation)) {
    final_merge <- final_merge |>
      modify_abbreviation(abbreviation)
  }
    final_merge <- final_merge|>
    as_gt()|>
    tab_footnote(footnote = ph_text,
                 locations = cells_body(columns = "p.value_1", rows = 1)
                 )|>
    gt::opt_footnote_marks(marks = "letters")


    if(show_plot){
      plot_data <- data |>
        group_by(.data[[grp_name]]) |>
        summarise(
                  n = n(),
                  mean_val = mean(.data[[out_name]], na.rm = TRUE),
                  sd_val = sd(.data[[out_name]], na.rm = TRUE),
                  error = 1.96 * (sd_val / sqrt(n)),
                  lower = mean_val - error,
                  upper = mean_val + error
                  )

      final_plot <- ggplot(plot_data, aes(x = .data[[grp_name]], y = mean_val)) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1,
                      color = "blue", linewidth = 0.8) +
        geom_point(shape = 21, size = 4, fill = "white", color =  "black") +
        geom_text(aes(y = lower - (0.05 * mean_val), label = paste0("n = ", n)),
                  size = 3.5) +
        labs(title = "Mean Plot with 95% CI", x = grp_name, y = out_name) +
        theme_minimal()

      #print(final_plot)
      return(list(table = final_merge, plot = final_plot))
    }

      return(final_merge)

}

# linear regression ----

#' Linear regression test
#'
#' @description A Linear regression output table for SLR or MLR
#' @param data Data frame
#' @param outcome_var Outcome variable
#' @param predictor_vars Predictor variables
#' @param var_labels New labels for variables in string
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A linear regression output table
#' @importFrom stats lm as.formula
#' @examples
#' lm_tbl(
#'   data = mjms_data,
#'   outcome_var = Cholesterol,
#'   predictor_vars = c(Age, BMI, Smoker),
#'   var_labels = list(Age ~ "Age (Years)", BMI ~ "Body Mass Index"),
#'   table_caption = "Multiple Linear Regression for Cholesterol"
#' )
#'
#' @export
lm_tbl <- function(data, outcome_var, predictor_vars, var_labels = NULL,
                   table_caption = "Factors associated with outcome",
                   abbreviation = NULL){

  out_name <- data|>
    select({{outcome_var}})|>
    names()

  pred_names <- data|>
    select({{predictor_vars}})|>
    names()

  frmla <- as.formula(paste(out_name, "~", paste(pred_names, collapse =  " + ")))
  model <- lm(frmla, data = data)

  r_squared <- summary(model)$r.squared

  reset_gtsummary_theme()
  theme_gtsummary_journal("jama")

  linear_tbl <- model |>
    tbl_regression(
      label = var_labels
    )|>
    modify_header(
      label = "**Factors**",
      estimate = "**Adjusted** _b_**(95% CI)**",
      p.value = "_P_**-value**"
    )|>
    modify_post_fmt_fun(
      fmt_fun = ~ gsub(" to ", ", ", .),
      columns = "estimate"
    )|>
    modify_caption(
      paste0("**Table :** ", table_caption, " (_n_ = {N}).")
    )

  linear_final <- linear_tbl |>
    remove_abbreviation("CI = Confidence Interval")|>
    modify_footnote_header(
      footnote = "Adjusted regression coefficients,",
      columns = "estimate"
    )|>
    modify_footnote_header(
      footnote = paste0("Multiple linear regression (R\u00b2 =",
                        sprintf("%.3f", r_squared), ")."),
      columns = "p.value"
    )|>
    as_gt()|>
    gt::opt_footnote_marks(marks = "letters")
  if(!is.null(abbreviation)){
    linear_final <- linear_final |>
      tab_source_note(source_note = abbreviation)
  }

  return(linear_final)
}

# logistic regression ----

#' Logistic regression test
#'
#' @description A logistic regression output table
#' @param data Data frame
#' @param outcome_var Binary output variable
#' @param predictors Predictor variables
#' @param ref_levels List name of reference levels (optional)
#' @param cat_vars Categorical variables that wish to see the reference variable
#' @param var_labels List name of variable labels
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A logistic regression output table
#' @importFrom stats glm binomial as.formula relevel
#' @examples
#' logistic_tbl(
#' data = mjms_data,
#' outcome_var = Heart_Disease,
#' predictors = c(Age, Sex, Smoker),
#' ref_levels = list(Smoker = "No", Sex = "Female"),
#' cat_vars = c("Sex", "Smoker"),
#' table_caption = "Risk Factors for Heart Disease")
#'
#' @export
logistic_tbl <- function(data,
                         outcome_var,
                         predictors,
                         ref_levels = NULL,
                         var_labels = NULL,
                         cat_vars = NULL,
                         table_caption = "Associated factors",
                         abbreviation = NULL){

 outcome_quo <- rlang::enquo(outcome_var)
 predictors_quo <- rlang::enquo(predictors)

 out_name <- data|> select(!!outcome_quo)|> names()

 pred_names <- data|> select(!!predictors_quo)|> names()

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

 model <- glm(model_formula, data = data, family = binomial(link = "logit"))

 beta_tbl <- broom::tidy(model)|>
   dplyr::select(term, estimate) |>
   dplyr::mutate(b =sprintf("%.2f", estimate))|>
   dplyr::select(term, b)

 reset_gtsummary_theme()
 theme_gtsummary_journal("jama")

 logit_tbl <- model|>
   tbl_regression(
     exponentiate = TRUE,
     tidy_fun = broom.helpers::tidy_parameters,
     type = if(!is.null(cat_vars)) list(all_of(cat_vars) ~ "categorical") else NULL,
     label = var_labels
   )|>
   modify_header(
     label = "**Factors**",
     estimate = "**Adjusted OR    \n(95% CI)**",
     p.value = "_P_**-value**"
   )|>
   modify_post_fmt_fun(
     fmt_fun = ~ gsub(" to ", ", ", .),
     columns = "estimate"
   )|>
   modify_table_body(
     ~ .x|>
       dplyr::left_join(beta_tbl, by = "term")|>
       dplyr::mutate(b = ifelse(is.na(b) & !is.na(variable), "-", b))|>
       dplyr::relocate(b, .before = estimate)
   )|>
   modify_column_unhide(b)|>
   modify_header(b = "**_b_**")|>
   modify_caption(
     paste0("**Table :** ", table_caption," (_n_ = {N}).")
   )

 final_logit <- logit_tbl|>
   modify_footnote_header(
     footnote = "Likelihood ratio test.",
     columns = p.value
   )|>
   modify_footnote_body(
     footnote = "Reference category.",
     rows = b == "-"
   )|>
   remove_abbreviation("CI = Confidence Interval")|>
   remove_abbreviation("OR = Odds Ratio")

 final_logit <- final_logit|>
   as_gt()|>
   gt::opt_footnote_marks(marks = "letters")
 if(!is.null(abbreviation)){
   final_logit <- final_logit |>
     tab_source_note(source_note = abbreviation)
 }

 return(final_logit)
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
#' @examples
#' mcnemar_tbl(
#' data = mjms_data,
#' pre_var = Symptom_Pre,
#' post_var = Symptom_Post,
#' table_caption = "Symptom status pre- and post-treatment.",
#' header_pre = "Pre Symptom",
#' header_post = "Post Symptom"
#' )
#'
#' @export
mcnemar_tbl <- function(
    data,
    pre_var,
    post_var,
    table_caption = "**Association between pre and post**",
    header_pre = "Pre",
    header_post = "Post"
    ){

  pre_nm <- rlang::as_name(rlang::enquo(pre_var))
  post_nm <- rlang::as_name(rlang::enquo(post_var))

  levs <- sort(union(unique(data[[pre_nm]]), unique(data[[post_nm]])))

  data_fixed <- data|>
    dplyr::mutate(
      !!pre_nm := factor(.data[[pre_nm]], levels = levs),
      !!post_nm := factor(.data[[post_nm]], levels = levs)
    )

  mcnemar_out <- mcnemar.test(
    table(data[[pre_nm]], data[[post_nm]]),
    correct = TRUE
  )

  chi_val <- mcnemar_out$statistic
  df_val <- mcnemar_out$parameter
  p_val <- mcnemar_out$p.value

  row_counts <- data_fixed|>
    count(label = as.character(.data[[pre_nm]]))|>
    rename(n_row_total = n)

  reset_gtsummary_theme()
  #theme_gtsummary_journal("jama")

  mcn_tbl <- data |>
    select(all_of(c(pre_nm, post_nm)))|>
    tbl_summary(
      by = all_of(post_nm),
      include = all_of(pre_nm),
      label = setNames(list(header_pre), pre_nm),
      type = setNames(list("categorical"), pre_nm),
      percent = "cell",
      statistic = list(all_categorical() ~ "{n} ({p})"),
      digits = list(all_categorical() ~ c(0,1))
    )|>
    modify_table_body(
      ~ .x |>
        dplyr::left_join(row_counts, by = "label")|>
        dplyr::mutate(
          n_total = ifelse(row_type == "level", as.character(n_row_total), NA),

          chisq_stats = ifelse(row_type == "level", NA,
                         sprintf("%.2f (%d)", chi_val, df_val)),

          p_final = ifelse(row_type == "label", NA,
                           ifelse(p_val < 0.001, "<0.001", sprintf("%.3f", p_val)
          ))
        )|>
        dplyr::relocate(n_total, chisq_stats, p_final, .after = last_col())
    )|>
    modify_column_unhide(columns = c(n_total, chisq_stats, p_final))|>
    modify_header(
      all_stat_cols() ~ "**{level}**  \n_n_ (%)",
      label = paste0("**", header_pre, "**"),
      n_total = "**_n_**",
      chisq_stats = "\u03c7\u00b2 **-statistic**  \n(df)",
      p_final = "_P_**-value**"
    )|>
    modify_spanning_header(all_stat_cols() ~ paste0("**",header_post,"**"))|>
    modify_caption(paste0("**Table :** ", table_caption))|>
    remove_footnote_header()|>
    modify_footnote_header(
      footnote = "McNemar's Chi-squared test with continuity correction.",
      columns = "p_final"
    )|>
    as_gt()|>
    gt::opt_footnote_marks(marks = "letters")

  return(mcn_tbl)
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
#' @examples
#' chisq_tbl(
#'   data = mjms_data,
#'   exposure_var = Sex,
#'   outcome_var = Smoker,
#'   table_caption = "Association of Gender vs. Smoking Status."
#' )
#'
#' @export
chisq_tbl <- function(
    data,
    exposure_var,
    outcome_var,
    table_caption = "**Association between exposure and outcome**"){

  exposure_quo <- rlang::enquo(exposure_var)
  outcome_quo <- rlang::enquo(outcome_var)

  exp_name <- rlang::as_name(exposure_quo)
  out_name <- rlang::as_name(outcome_quo)

  chi_out <- chisq.test(table(data[[exp_name]], data[[out_name]]))
  chi_val <- chi_out$statistic
  df_val <- chi_out$parameter
  p_val <- chi_out$p.value

  row_counts <- data|>
    count(!!exposure_quo)|>
    mutate(label = as.character(!!exposure_quo))|>
    rename(n_row = n)

  reset_gtsummary_theme()
  theme_gtsummary_journal("jama")

  chi_tbl <- data |>
    tbl_summary(
      by = !!outcome_quo,
      include = !!exposure_quo,
      statistic = all_categorical() ~ "{n} ({p})",
      digits = all_categorical() ~ c(0,1)
    )

  chi_tbl <- chi_tbl|>
    modify_table_body(
      ~ .x |>
        left_join(row_counts, by = "label") |>
        mutate(
          n_total = ifelse(row_type == "label", NA, n_row),
          chi_stat = ifelse(row_type == "label", NA,
                            sprintf("%.2f (%d)", chi_val, df_val)),
          p_val_custom = ifelse(row_type == "label", NA,
                                ifelse(p_val < 0.001, "< 0.001",
                                       sprintf("%.3f", p_val))))|>
        relocate(n_total, chi_stat, p_val_custom, .after = last_col()))

  chi_tbl <- chi_tbl|>
    modify_column_unhide(columns = c(n_total, chi_stat, p_val_custom))|>
    modify_header(
      label = "**Variable**",
      all_stat_cols() ~ "**{level}**  \n_n_ (%)",
      n_total = "**_n_**",
      chi_stat = "\u03c7\u00b2 **-statistic**  \n(df)",
      p_val_custom = "_P_**-value**"
    )|>
    modify_spanning_header(all_stat_cols() ~ paste0("**", out_name,"**"))|>
    modify_caption(
      paste0("**Table :** ", table_caption)
    )|>
    remove_footnote_header()|>
    modify_footnote_header(
      footnote = "Chi-square test for independence.",
      columns = "p_val_custom"
    )|>
    as_gt()|>
    gt::opt_footnote_marks(marks = "letters")

  return(chi_tbl)
}

# diagnostic test ----

#' Diagnostic test
#'
#' @description A diagnostic test output table with/without ROC Curve
#' @param data Data frame
#' @param status_var Target variable (Gold standard)
#' @param marker_map Define index tests/markers, friendly labels and cutoff points for every markers. See example below for settings.
#' @param show_plot TRUE(plot) FALSE(no plot)
#' @param plot_marker Specific test visualize in the ROC plot
#' @param table_caption Caption for the table in string
#' @param abbreviation Full name of abbreviated variables
#' @return A diagnostic table with/without ROC plot
#' @importFrom pROC roc coords ci.auc
#' @importFrom ggplot2 ggplot aes geom_abline geom_step scale_x_continuous scale_y_continuous labs theme_minimal
#' @examples
#' med_marker <- list(
#'Cholesterol   = list(label = "Serum Cholesterol (Cutoff: 200 mg/dL)", cutoff = 200),
#'BMI           = list(label = "Body Mass Index (Cutoff: 25 kg/m2)", cutoff = 25),
#'SBP_Left_Arm  = list(label = "Systolic BP - Left (Cutoff: 140 mmHg)", cutoff = 140)
#')
#'diagnostic_tbl(
#'  data = mjms_data,
#'  status_var = "Gold_Standard",
#'  marker_map = med_marker, # list of all index markers
#'  show_plot = TRUE,
#'  plot_marker = "Cholesterol", # show plot for Cholesterol
#'  table_caption = "Diagnostic Accuracy of Cardiovascular Markers.",
#'  abbreviation = c("AUC = Area Under Curve", "SBP = Systolic Blood Pressure")
#')
#'
#' @export
diagnostic_tbl <- function(
    data,
    status_var,
    marker_map,
    show_plot = FALSE,
    plot_marker = NULL,
    table_caption = "Diagnostic performance",
    abbreviation = NULL){

  marker_cols <- names(marker_map)
  marker_labels <- setNames(lapply(marker_map, function(x) x$label), marker_cols)

  diag_stats <- function(data, variable, type){
    if (!status_var %in% names(data))
      return(tibble(complex = "Error: No Status"))

    status_data <- data[[status_var]]
    marker_data <- data[[variable]]
    cutoff <- marker_map[[variable]]$cutoff

    status_f <- factor(status_data)
    lvls <- rev(levels(status_f))

    roc_val <- roc(status_f, marker_data, quiet = T, levels = lvls)

    val <- "NA"
    if (type == "sens"){
      res <- coords(roc_val, x = cutoff, input = "threshold",
                    ret = "sensitivity", transpose = F)
      val <- paste0(round(res$sensitivity * 100, 1), "%")
    }

    if (type == "spec"){
      res <- coords(roc_val, x = cutoff, input = "threshold",
                    ret = "specificity", transpose = F)
      val <- paste0(round(res$specificity * 100, 1), "%")
    }

    if (type == "auc"){
      ci_val <- as.numeric(pROC::ci.auc(roc_val))
      val <-paste0(round(ci_val[2], 2), " (", round(ci_val[1], 2), ", ",
                   round(ci_val[3], 2), ")")
    }
    return(tibble(complex = val))
  }

  t_sens <- data |>
    select(all_of(c(status_var, marker_cols))) |>
     tbl_custom_summary(
       include = all_of(marker_cols),
       label = marker_labels,
       stat_fns = ~ function(data, variable, ...) diag_stats(data, variable, "sens"),
       statistic = ~ "{complex}")|>
    modify_header(stat_0 = "**Sensitivity (%)**"
    )

  t_spec <- data |>
    select(all_of(c(status_var, marker_cols))) |>
    tbl_custom_summary(
      include = all_of(marker_cols),
      label = marker_labels,
      stat_fns = ~ function(data, variable, ...) diag_stats(data, variable, "spec"),
      statistic = ~ "{complex}") |>
    modify_header(stat_0 = "**Specificity (%)**"
    )

  t_auc <- data |>
    select(all_of(c(status_var, marker_cols))) |>
    tbl_custom_summary(
      include = all_of(marker_cols),
      label = marker_labels,
      stat_fns = ~ function(data, variable, ...) diag_stats(data, variable, "auc"),
      statistic = ~ "{complex}") |>
    modify_header(stat_0 = "**AUC (95% CI)**")

  t_pval <- data |>
    select(all_of(c(status_var, marker_cols)))|>
    tbl_summary(
      by = all_of(status_var),
      label = marker_labels
    )|>
    add_p(test= list(all_continuous() ~ "wilcox.test"))|>
    modify_column_hide(all_of(c("stat_1", "stat_2")))|>
    modify_header(p.value = "_P_**-value**")

  final_diag <- tbl_merge(
    tbls = list(t_sens, t_spec, t_auc, t_pval),
    tab_spanner = FALSE
  )|>
   modify_header(label = "**Variable (cutoff)**")|>
    modify_abbreviation(paste(abbreviation, collapse = "; "))|>
    remove_footnote_header()|>
    modify_footnote_header(footnote = "Null hypothesis: true area = 0.5.",
                           columns = "p.value_4")|>
    as_gt()|>
    opt_footnote_marks(marks = "letters")|>
    tab_caption(caption = paste0(table_caption, " (n = ", nrow(data), ")"))

  plot_diag <- NULL
  if (show_plot){
    target_marker <- if (is.null(plot_marker)) marker_cols[1] else plot_marker
    roc_objs <- roc(data[[status_var]], data[[target_marker]], ci = T, quiet = T)
    plot_df <- data.frame(sen = roc_objs$sensitivities,
                          one_spec = 1 - roc_objs$specificities)

  plot_diag <- ggplot(plot_df, aes(x = one_spec, y = sen)) +
    geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "dashed") +
    geom_step(color = "turquoise", linewidth = 1.2) +
    scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
    labs(
      title = paste("ROC Curve:", marker_map[[target_marker]]$label),
      x = "1 - Specificity", y = "Sensitivity"
    ) +
    theme_minimal()

  # print(plot_diag)

  }

  return(list(table = final_diag, plot = plot_diag))
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
#' @examples
#' pearson_tbl(
#'   data = mjms_data,
#'   included_var = c(Age, BMI, SBP_Left_Arm, SBP_Right_Arm),
#'   show_plot = TRUE,
#'   plot_x = "SBP_Left_Arm",
#'   plot_y = "SBP_Right_Arm",
#'   abbreviation = "BMI = body mass index, SBP = systolic blood pressure",
#'   table_caption = "Correlation between variables."
#' )
#'
#' @export
pearson_tbl <- function(
    data,
    included_var = NULL,
    show_plot = F,
    plot_x = NULL,
    plot_y = NULL,
    abbreviation = NULL,
    table_caption ="" ){

  if (is.null(substitute(included_var))){
    corr_subset <- data |> select(where(is.numeric))
  } else {
    corr_subset <- data |> select ({{included_var}}) |> select(where(is.numeric))
  }

  var_names <- colnames(corr_subset)
  n_var <- ncol(corr_subset)
  results <- list()

  sd_vals <- apply(corr_subset, 2, sd, na.rm = T)
  r_mat <- cor(corr_subset, method = "pearson", use = "complete.obs")
  p_mat <- matrix(NA, n_var, n_var)
  for (i in 1:n_var) {
    for (j in 1:n_var) {
      p_mat[i, j] <- cor.test(corr_subset[[i]], corr_subset[[j]])$p.value
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

  for (i in 1:n_var) {
    for (j in 1:n_var) {
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
   rownames_to_column(var = "Variables")|>
   gt(rowname_col = "Variables")|>
   tab_header(title = md(paste0("**", table_caption, "** (n = ", nrow(data), ")")))|>
   tab_stubhead(label = "Variables")|>
   cols_align(align = "center", everything())

 for (i in 1:n_var) {
   for (j in 1:n_var) {
     target_col <- var_names[j]

     if (i == j) {
       gt_table <- gt_table|>
         tab_footnote(footnote = "Standard Deviation (SD)",
                      locations = cells_body(columns = all_of(target_col), rows = i)
                      )
     } else if (i < j){
       gt_table <- gt_table|>
         tab_footnote(footnote = "P-value",
                      locations = cells_body(columns = all_of(target_col), rows = i)
         )
     } else {
       gt_table <- gt_table|>
         tab_footnote(footnote = "Pearson correlation coefficient (r)",
                      locations = cells_body(columns = all_of(target_col), rows = i)
         )
     }
   }
 }


 if (!is.null(abbreviation)){
   if (!is.null(names(abbreviation)) && all(names(abbreviation) != "")){
     abbr_text <- paste(names(abbreviation), abbreviation,
                        sep = " = ", collapse = "; ")
   } else {
     abbr_text <- paste(abbreviation, collapse = "; ")
   }
   gt_table <- gt_table |>
     tab_source_note(source_note = md(abbr_text))
 }

 results$table <- gt_table |>
   opt_footnote_marks(marks = "letters")|>
   tab_style(style = cell_text(weight = "bold"),
             locations = cells_stub())|>
   tab_options(
      table.font.size = px(14),
      column_labels.font.weight = "bold",
      heading.title.font.size = px(16),
      table.border.top.style = "none",
      table.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table.width = pct(90)
    )|>
    opt_row_striping()

  if (show_plot && !is.null(plot_x) && !is.null(plot_y)){
    ctest <- cor.test(data[[plot_x]], data[[plot_y]])

    results$plot <- ggplot(data, aes(x = .data[[plot_x]], y = .data[[plot_y]])) +
      geom_point(shape = 1) +
      geom_smooth(method = "lm", se = F) +
      labs(
        title = paste("Relationship between", plot_x, "and", plot_y),
        caption = paste0("r = ", round(ctest$estimate, 2),
                         " (P ", ifelse(ctest$p.value < 0.001, "< 0.001)",
                                        paste0( "= ", round(ctest$p.value, 3), ")"
                                        )))
      ) +
      theme_classic() +
      theme(plot.caption = element_text(hjust = 0.5, face = "bold"))
  }

  return(results)
}
