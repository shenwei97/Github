
library(showtext)
showtext_auto()

data.eda <-
  data.analysis |>
  select("性别", "抑郁结果", "抑郁得分", contains("质"), contains("兼")) 


library(dlookr)
plot_correlate(data.eda)




# install.packages("DataExplorer")
library(DataExplorer)

data.eda |>
  create_report()

data.eda |>
  plot_correlation(
    type = c("continuous"),
    cor_args = list(method = 'spearman')
  )

library(correlation)

data.eda |> 
  correlation(method = "auto") 

rez <- correlation(data.eda,method = "auto")

x <- cor_sort(as.matrix(rez))
layers <- visualisation_recipe(x)

plot(layers)


x <- summary(rez, redundant = TRUE, digits = 3)
plot(visualisation_recipe(x))
