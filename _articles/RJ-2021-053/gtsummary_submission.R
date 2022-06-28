library(gtsummary)
library(tidyverse)
library(gt)

# set gtsummary theme for the R journal ----------------------------------------
reset_gtsummary_theme()
my_r_journal_theme <- theme_gtsummary_compact(set_theme = FALSE)
my_r_journal_theme$`as_gt-lst:addl_cmds` <-
  my_r_journal_theme$`as_gt-lst:addl_cmds` %>%
  c(list("cols_hide" = rlang::expr(gt::tab_options(table.font.names = "palatino"))))
set_gtsummary_theme(my_r_journal_theme)

# set to TRUE to re-render all tables (gt images and latex tables)
# if you're at the R Journal, please leave this as FALSE
save_tables <- FALSE

# Data Summaries ---------------------------------------------------------------

if (save_tables)
  trial %>%
  imap_dfr(
    ~tibble(
      colname = str_glue('`{.y}`'),
      label = attr(.x, "label"),
      class = class(.x),
      values = trial %>% select(.y) %>% distinct() %>%
        arrange(!!rlang::sym(.y)) %>% slice(1:4) %>% pull(.y) %>%
        na.omit() %>% {paste0("`", ., "`", collapse = ", ")}
    ) %>%
      mutate(
        values = ifelse(length(unique(na.omit(trial[[.y]]))) > 4,
                        paste0(values, ", ..."),
                        values)
      )
  ) %>%
  gt() %>%
  fmt_markdown(everything()) %>%
  cols_align("left", everything()) %>%
  as_latex() %>%
  as.character() %>%
  str_replace(fixed("\\bottomrule"), fixed("\\bottomrule\\caption{\\label{tab:caption}Table 1. Example data frame, \\texttt{trial}}\\\\\n")) %>%
  readr::write_lines("tex_tables/tbl_trial_desc.tex")

## `tbl_summary()` -------------------------------------------------------------

tbl_summary_1 <-
  trial %>%
  select(age, grade, response, trt) %>%
  tbl_summary(by = trt)

if (save_tables)
  tbl_summary_1 %>%
  as_gt() %>%
  gtsave(file = "images/summary_basic.png")

if (save_tables)
  tibble::tribble(
    ~Argument,       ~Description,
    "`label=`",       "specify the variable labels printed in table",
    "`type=`",        "specify the variable type (e.g., continuous, categorical, etc.)",
    "`statistic=`",   "change the summary statistics presented",
    "`digits=`",      "number of digits the summary statistics will be rounded to",
    "`missing=`",     "whether to display a row with the number of missing observations",
    "`missing_text=`","text label for the missing number row",
    "`sort=`",        "change the sorting of categorical levels by frequency",
    "`percent=`",     "print column, row, or cell percentages",
    "`include=`",     "list of variables to include in summary table"
  ) %>%
  gt() %>%
  fmt_markdown(everything()) %>%
  as_latex() %>%
  as.character() %>%
  str_replace(fixed("\\bottomrule"), fixed("\\bottomrule\\caption{\\label{tab:caption}Table 2. \\texttt{tbl\\_summary()} function arguments}\\\\\n")) %>%
  readr::write_lines("tex_tables/tbl_tbl_summary_args.tex")

tbl_summary_2 <-
  trial %>%
  select(age, grade, response, trt) %>%
  tbl_summary(
    by = trt,
    type = age ~ "continuous2",
    label = age ~ "Patient Age",
    statistic = list(age ~ c("{N_nonmiss}", "{mean} ({sd})"),
                     c(grade, response) ~ "{n} / {N} ({p}%)"),
    digits = c(grade, response) ~ c(0, 0, 1),
    missing = "no"
  )

if (save_tables)
  tbl_summary_2 %>%
  as_gt() %>%
  gtsave(file = "images/summary_plus.png")

if (save_tables)
  tibble::tribble(
    ~Function,             ~Description,
    "`add_p()`",           "add *p*-values to the output comparing values across groups",
    "`add_overall()`",     "add a column with overall summary statistics",
    "`add_n()`",           "add a column with N (or N missing) for each variable",
    "`add_difference()`",  "add column for difference between two group, confidence interval, and *p*-value",
    "`add_stat_label()`",  "add label for the summary statistics shown in each row",
    "`add_stat()`",        "generic function to add a column with user-defined values",
    "`add_q()`",           "add a column of *q*-values to control for multiple comparisons"
  ) %>%
  gt(caption = "`tbl_summary()` functions to add information") %>%
  fmt_markdown(everything()) %>%
  as_latex() %>%
  as.character() %>%
  str_replace(fixed("\\bottomrule"), fixed("\\bottomrule\\caption{\\label{tab:caption}Table 3. \\texttt{tbl\\_summary()} functions to add information}\\\\\n")) %>%
  readr::write_lines("tex_tables/tbl_tbl_summary_family.tex")

tbl_summary_3 <-
  trial %>%
  select(age, grade, response, trt) %>%
  tbl_summary(by = trt) %>%
  add_overall() %>%
  add_p(test = all_continuous() ~ "t.test",
        pvalue_fun = ~style_pvalue(., digits = 2))

if (save_tables)
  tbl_summary_3 %>%
  as_gt() %>%
  gtsave(file = "images/summary_plus_plus.png")


## `tbl_svysummary()` ----------------------------------------------------------

# convert trial data frame to survey object
svy_trial <- survey::svydesign(data = trial, ids = ~ 1, weights = ~ 1)

tbl_svysummary_1 <-
  svy_trial %>%
  tbl_svysummary(by = trt, include = c(trt, age, grade)) %>%
  add_p()

if (save_tables)
  tbl_svysummary_1 %>%
  as_gt() %>%
  gtsave(file = "images/svysummary.png")

## `tbl_cross()` ---------------------------------------------------------------

tbl_cross_1 <-
  trial %>%
  tbl_cross(row = stage, col = trt, percent = "cell") %>%
  add_p(source_note = TRUE)

tbl_cross_1 %>%
  as_gt() %>%
  gtsave(file = "images/cross.png")

## `tbl_survfit()` -------------------------------------------------------------

library(survival)

tbl_survfit_1 <-
  list(survfit(Surv(ttdeath, death) ~ trt, trial),
       survfit(Surv(ttdeath, death) ~ grade, trial)) %>%
  tbl_survfit(times = c(12, 24),
              label_header = "**{time} Month**") %>%
  add_p()

if (save_tables)
  tbl_survfit_1 %>%
  as_gt() %>%
  gtsave(file = "images/survfit.png")

## Customization ---------------------------------------------------------------

if (save_tables)
  tibble::tribble(
    ~Function,                     ~Description,
    "`modify_header()`",           "update column headers",
    "`modify_footnote()`",         "update column footnote",
    "`modify_spanning_header()`",  "update spanning headers",
    "`modify_caption()`",          "update table caption/title",
    "`bold_labels()`",             "bold variable labels",
    "`bold_levels()`",             "bold variable levels",
    "`italicize_labels()`",        "italicize variable labels",
    "`italicize_levels()`",        "italicize variable levels",
    "`bold_p()`",                  "bold significant *p*-values"
  ) %>%
  gt() %>%
  fmt_markdown(everything()) %>%
  fmt_markdown(everything()) %>%
  as_latex() %>%
  as.character() %>%
  str_replace(fixed("\\bottomrule"), fixed("\\bottomrule\\caption{\\label{tab:caption}Table 4. Functions to style and modify gtsummary tables}\\\\\n")) %>%
  readr::write_lines("tex_tables/tbl_modify.tex")

tbl_custom <-
  trial %>%
  select(marker, response, trt) %>%
  tbl_summary(by = trt, missing = "no",
              statistic = list(marker ~ "{mean} ({sd})")) %>%
  add_difference() %>%
  add_n() %>%
  add_stat_label() %>%
  bold_labels() %>%
  modify_header(
    list(label ~ "**Variable**",
         all_stat_cols() ~ "**{level}**")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Randomization Assignment**") %>%
  as_gt() %>%
  gt::tab_header(
    title = gt::md("**Table 1. Treatment Differences**"),
    subtitle = gt::md("_Highly Confidential_")
  ) %>%
  gt::tab_source_note("Data updated June 26, 2015")

if (save_tables)
  tbl_custom %>%
  gtsave(file = "images/custom.png")

# Model Summaries --------------------------------------------------------------

## `tbl_regression()` ----------------------------------------------------------

tbl_regression_1 <-
  glm(response ~ age + grade, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE) %>%
  add_global_p()

if (save_tables)
  tbl_regression_1 %>%
  as_gt() %>%
  gtsave(file = "images/regression.png")

## `tbl_uvregression()` --------------------------------------------------------

tbl_uvregression_1 <-
  trial %>%
  select(response, age, grade) %>%
  tbl_uvregression(
    y = response,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(., digits = 2)
  ) %>%
  add_nevent() %>%
  add_global_p()

if (save_tables)
  tbl_uvregression_1 %>%
  as_gt() %>%
  gtsave(file = "images/uvregression.png")

# Merging and Stacking ---------------------------------------------------------

tbl1 <-
  glm(response ~ age + grade, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

tbl2 <-
  coxph(Surv(ttdeath, death) ~ age + grade, trial) %>%
  tbl_regression(exponentiate = TRUE)

tbl_merge_1 <-
  tbl_merge(
    tbls = list(tbl1, tbl2),
    tab_spanner = c("**Tumor Response**", "**Time to Death**")
  )

if (save_tables)
  tbl_merge_1 %>%
  as_gt() %>%
  gtsave(file = "images/merge.png")

# Inline Reporting -------------------------------------------------------------

# Themes -----------------------------------------------------------------------

theme_gtsummary_journal("jama")

tbl_jama <-
  glm(response ~ age + grade, trial, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)


if (save_tables)
  tbl_jama %>%
  as_gt() %>%
  gtsave(file = "images/jama.png")

# Print Engines ----------------------------------------------------------------

# Summary ----------------------------------------------------------------------
