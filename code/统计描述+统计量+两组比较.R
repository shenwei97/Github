# 正态
my_ttest3 <- function(data, variable, by, ...) {
  t.test(data[[variable]] ~ as.factor(data[[by]])) %>%
    broom::tidy() %>%
    select(statistic, p.value) 
}


dat.desc |>
  select(分组,var_nom) |> 
  mutate(分组 = paste("分组", 分组)) |>
  tbl_summary(
    by = 分组, # group variable
    missing = 'no',
    type = list(ends_with('评分')~"continuous"),
    statistic = list(any_of(var_nom) ~ "{mean} ± {sd}")
  ) |> 
  add_stat(fns = everything() ~ my_ttest3) %>%
  modify_header(
    list(
      statistic ~ "**statistic**",
      p.value ~ "**p-value**"
    )
  ) %>%
  modify_fmt_fun(
    list(
      statistic ~ function(x) round(x, digits = 3),
      p.value ~ function(x) style_pvalue(x, digits = 3)
    )
  ) |> 
  bold_p(t = 0.05) |>
  add_overall() |>
  add_n() |>
  bold_labels() |>
  add_stat_label() |>
  italicize_levels() |>
  modify_caption('**Patient Characteristics-正态变量** (N = {N})')|>
  save_flextable_as_docx('统计描述-正态')

# 非正态  
my_wilcox.test3 <- function(data, variable, by, ...) {
  wilcox.test(data[[variable]] ~ as.factor(data[[by]])) %>%
    broom::tidy() %>%
    select(statistic, p.value)
}

dat.desc |>
  select(分组,var_unnom) |> 
  mutate(分组 = paste("分组", 分组)) |>
  tbl_summary(
    by = 分组, # group variable
    missing = 'no',
    type = list(ends_with('评分')~"continuous"),
    statistic = list(any_of(var_nom) ~ "{mean} ± {sd}")
  ) |> 
  add_stat(fns = everything() ~ my_wilcox.test3) %>%
  modify_header(
    list(
      statistic ~ "**statistic**",
      p.value ~ "**p-value**"
    )
  ) %>%
  modify_fmt_fun(
    list(
      statistic ~ function(x) round(x, digits = 3),
      p.value ~ function(x) style_pvalue(x, digits = 3)
    )
  ) |> 
  bold_p(t = 0.05) |>
  add_overall() |>
  add_n() |>
  bold_labels() |>
  add_stat_label() |>
  italicize_levels() |>
  modify_caption('**Patient Characteristics-非正态变量** (N = {N})')|>
  save_flextable_as_docx('统计描述-非正态')