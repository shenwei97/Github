# 目的:KM曲线绘制

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

# KM曲线
km_plot <- function(time, status, group, title, ylab) {
  fml <- as.formula(
    paste0(
      "Surv(", time, ",", status, ") ~", group
    )
  )
  # survfit2(fml, data = data) |> print() # 给出中位生存时间数值
  survfit2(fml, data = data) |>
    ggsurvfit(linewidth = 1) +
    add_pvalue(caption = "Log-rank {p.value}", location = "annotation", size = 5)+ #添加p值
    add_censor_mark() + # 加删失标记
    add_confidence_interval() + #加置信区间
    add_quantile() + # 加中位生存时间
    add_risktable(risktable_stats = "{n.risk} ({cum.event})") +
    add_risktable_strata_symbol() +
    labs(
      title = title,
      x = "Follow-up time(month)",
      y = ylab
    ) +
    guides(
      color = guide_legend(title = "Group")
    ) +
    theme(legend.position = "bottom") +
    theme_classic()
}

km_plot(time = "time", status = "status", group = 1, title = "KM curve", ylab = "Survival Probability")


