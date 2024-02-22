data <- read_excel(path = "./杂物/工作簿2.xlsx", sheet = 1)
data <- read_excel(path = "./杂物/工作簿2.xlsx", sheet = 2)
data <- read_excel(path = "./杂物/工作簿2.xlsx", sheet = 3)
data <- read_excel(path = "./杂物/工作簿2.xlsx", sheet = 4)

data1 <- data |>
  pivot_longer(cols = 1:3, names_to = "name", values_to = "value") |>
  drop_na(value)
data1$value <- as.numeric(data1$value)


shapiro.test(data$A组)
shapiro.test(data$B组)
shapiro.test(data$C组)

median(data$C组,na.rm = T)
quantile(data$C组,na.rm = T)

data1 |>
  tbl_summary(by = name, ) |>
  add_stat(fns = everything() ~ my_kruskal.test) |>
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3)) |>
  modify_caption("年龄") |>
  modify_header(
    list(
      statistic ~ "**Chi-squared**"
    )
  )





data1 |>
  tbl_summary(
    by = name,
    type = list(value ~ "continuous")
  ) |>
  add_stat(fns = everything() ~ my_kruskal.test) |>
  add_p() |>
  modify_caption("初龄") |>
  modify_header(
    list(
      statistic ~ "**Chi-squared**"
    )
  )



data1 |>
  tbl_summary(by = name, ) |>
  add_stat(fns = everything() ~ my_kruskal.test) |>
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3)) |>
  modify_caption("病程") |>
  modify_header(
    list(
      statistic ~ "**Chi-squared**"
    )
  )

data1 |>
  tbl_summary(by = name, ) |>
  add_stat(fns = everything() ~ my_kruskal.test) |>
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3)) |>
  modify_caption("疗程") |>
  modify_header(
    list(
      statistic ~ "**Chi-squared**"
    )
  )



