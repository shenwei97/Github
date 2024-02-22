# 1 数据清洗 -----------------------------------------------------------

## 1.1 导入数据 -------------------------------------------------------

dat.raw <- read_excel(here("data", "data.xlsx"))

paste_data_vars(dat.raw)

## 1.2 变量清洗 -------------------------------------------------------

data <-
  dat.raw |>
  mutate()


## 1.3 变量类型转换 ------------------------------------------------------


var_class <- select_fct_vars(data, 4)
var_fct <- strsplit(var_class, "[',']") |> unlist() |> unique()# 将结果粘贴
var_fct <-  var_fct[var_fct != ""]  # 去除空字符串
var_num <- setdiff(colnames(data), var_fct)

data <-
  data |>
  mutate(
    across(all_of(var_fct), as.factor),
    # across(var_fct, fct_inseq) # sorted by seq 1 2 3
  )


# 2 统计描述 ---------------------------------------------

library(rstatix)
zhengtai <-
  data |>
  shapiro_test(var_num)

# 正态变量
var_nom <- zhengtai |>
  filter(p >= 0.05) |>
  pull(variable)
# 非正态变量
var_unnom <- zhengtai |>
  filter(p < 0.05) |>
  pull(variable)

# 变量筛选
dat.desc <- data |> select(everything())

# one group compare
desc1 <-
  dat.desc |>
  mutate(response = paste("response", response)) |>
  tbl_summary(
    by = response, # group variable
    missing = 'no',
    # sort = list(everything() ~ 'frequency')
  ) |>
  add_p(
    test = list(any_of(var_nom) ~ "t.test"),# 正态变量进行t检验 3组改为'aov'
    # test.args = all_tests("fisher.test") ~ list(simulate.p.value = T),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_p(t = 0.05) |>
  add_overall() |>
  add_n() |>
  bold_labels() |>
  add_stat_label() |>
  italicize_levels() |>
  modify_caption("**Table1:  Patient Characteristics** (N = {N})")


# strata group variable
desc2 <-
  dat.desc |>
  select(age, grade, stage, trt, response) |>
  mutate(grade = paste("Grade", grade)) |>
  tbl_strata(
    strata = grade,
    .tbl_fun =
      ~ .x |>
        tbl_summary(
          by = response, # group variable
          missing = 'no'
        ) |>
        add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) |>
        bold_p(t = 0.05) |>
        # add_overall() |>
        add_n() |>
        bold_labels() |>
        bold_labels() |>
        italicize_levels(),
    .header = "**{strata}**, N = {n}"
  )

show_header_names(desc2)


# modify outputstyle
desc1 <-
  desc1 |>
  modify_header(
    c("label") ~ "**变量**",
    c("stat_0") ~ "**总体(N = {N})**",
    c("p.value") ~ "**P值**"
  ) |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**分组**") |>
  modify_caption("**Table1:  Patient Characteristics** (N = {N})")


desc2 <-
  desc2 |>
  modify_header(
    c("label") ~ "**变量**",
    c("p.value_1", "p.value_2", "p.value_3") ~ "**P值**"
  ) |>
  modify_caption("**Table1:  Patient Characteristics** ")
