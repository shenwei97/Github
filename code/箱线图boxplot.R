
library(ggpubr)
library(patchwork)
fig_boxplot <- function(data,var,group,method = NULL) {
  data |>
    ggboxplot(
      x = group, y = var, fill = group,
      palette = "npg", bxp.errorbar = T,
      # outlier.shape = NA, # 是否显示离群值
      ylab = "",
      title = group
    ) + 
    geom_pwc(
      # label = "p.adj.signif", # 不显示数值，显示***
      label = "italic(p)= {p}",
      # hide.ns = T,
      method = if (missing(method)) NULL else method #没有method，默认非参数检验
    ) +
    scale_fill_discrete(labels = c("治疗前", "治疗后")) + # 修改图例label
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(face = "bold"), # 加粗y轴ticks
      plot.title = element_text(face = "bold"),
      legend.title = element_blank()
    )
}

# 前后是两列的变量进行比较
barplot_compare <- function(data,group, yvar) {
  data %>%
    filter(组别 == group) %>%
    # filter(组别 != "C") %>%
    # mutate(组别 = as.character(组别)) %>%
    select(组别, contains(yvar)) %>%
    pivot_longer(
      cols = contains(yvar),
      names_to = yvar,
      values_to = 'value'
    ) %>%
    fig_boxplot(var = 'value', group = yvar, method = "wilcox_test")
}

# 拼图
my_barplot_compare <- function(data, yvar) {
  p1 <- barplot_compare(data, group = 'A', yvar) + labs(subtitle = 'A组')
  p2 <- barplot_compare(data, group = 'B', yvar) + labs(subtitle = 'B组')
  p1 + p2 + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
}