## ---------------------------
##
## Script name: 柱状图绘制
##
## Purpose of script:
##
## Author: WEI
##
## Date Created: 2024-01-27
##
## ---------------------------
## Notes: 
## https://yuzar-blog.netlify.app/posts/2023-08-23-histogramsdensityplots/
## 柱状图适用于单个连续性变量的统计描述
##
## ---------------------------
## Load packages
require(tidyverse)
require(janitor)
require(here)
require(gtsummary)
Sys.setLanguage("en")
theme_set(theme_test()) # for nicer looking plots
library(ISLR) # for Wage dataset
## ---------------------------



# 柱状图绘制
ggplot(data = Wage, aes(x = wage)) +
  geom_histogram()

# bin改变bin的数量,bins 默认为30
a <- ggplot(Wage, aes(x = wage)) +
  geom_histogram(bins = 4)

b <- ggplot(Wage, aes(x = wage)) +
  geom_histogram(bins = 100)

library(patchwork)
a + b

# binwidth改变bin的宽度
a <- ggplot(Wage, aes(x = wage)) +
  geom_histogram(binwidth = 50)

# install.packages('plotly')
# library(plotly) # 导出图片到网站可以交互
# ggplotly(a)

a + stat_bin(
  binwidth = 50, geom = "text", color = "red",
  aes(label = after_stat(count)),
  position = position_stack(vjust = 0.5)
)

# Add central tendency, SD, IQR, CIs

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(binwidth = 50,position = 'dodge')

