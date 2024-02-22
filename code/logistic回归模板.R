# 目的:

# 描述:

# 作者: WEI
# 日期: 2024-02-19

# 加载包
require(tidyverse)
require(janitor)
require(here)
require(gtsummary)
require(showtext)
# 设定主题
Sys.setLanguage("en")
showtext_auto()

# 加载函数
source(here("script/00-my-functions.R"))

## -----------------------------------------------------------------------

# 单因素多因素分析 ----------------------------------------------------------------

# 单因素cox + 多因素cox代码
paste_data_vars(data)

# 单因素变量
vars_cox <- c()

# 单因素
uvreg1 <-
  data |>
  # filter(group != "C") |>   #筛选组别
  # mutate(group = as.character(group)) |> #去掉过滤掉的因子分类
  select(everything()) |>
  tbl_uvregression(
    y = 复发,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = T,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  bold_p(t = 0.10) |>
  modify_caption("**Table1:  单因素logistic**")

# 多因素

m1 <- as.formula("复发 ~ 年龄 + 异常凝血酶原")

mureg1 <-
  glm(m1,family = binomial,data = data) |>
  tbl_regression(
    exponentiate = T,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_p(t = 0.05) |>
  modify_caption("**Table2:  多因素logistic**")


reg1 <-
  tbl_merge(
    tbls = list(uvreg1, mureg1),
    tab_spanner = c("**单因素**", "**多因素**")
  ) |>
  modify_caption("**Table3:   logistic 回归分析**")