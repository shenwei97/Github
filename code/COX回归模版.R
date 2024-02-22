# 目的: 生存分析模版

# 描述:

# 作者: WEI
# 日期: 2024-02-13

# 加载包
require(tidyverse)
require(janitor)
require(here)
require(gtsummary)
require(readxl)
require(survival)
require(ggsurvfit)
# 设定主题
Sys.setLanguage("en")

# 加载函数
source("script/00-my-functions.R")

## Code-------------------------------------------------------------------

# 单因素cox + 多因素cox代码
paste_data_vars(data)

# 单因素变量
vars_cox <- c()

# 单因素
uvreg1 <-
  data |>
  # filter(group != "C") |>   #筛选组别
  # mutate(group = as.character(group)) |> #去掉过滤掉的因子分类
  select(any_of(vars_cox), time, status, group) |>
  tbl_uvregression(
    y = Surv(time, status),
    method = coxph,
    exponentiate = T,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  bold_p(t = 0.10) |>
  modify_caption("**Table1:  单因素COX**")

# 多因素

m1 <- as.formula(" Surv(time, status) ~ 年龄+WBC+CA199")

mureg1 <-
  coxph(m1, data = data) |>
  tbl_regression(
    exponentiate = T,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_p(t = 0.05) |>
  modify_caption("**Table2:  多因素COX**")


reg1 <-
  tbl_merge(
    tbls = list(uvreg1, mureg1),
    tab_spanner = c("**单因素**", "**多因素**")
  ) |>
  modify_caption("**Table3:   Cox 回归分析**")
