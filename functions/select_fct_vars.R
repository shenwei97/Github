# 筛选数据集中分类小于level的变量并粘贴
paste_data_vars <- 
  function(data) {
    paste0("'", paste(data |> colnames(), collapse = "','"), "'")
  }

select_fct_vars <-
  function(data, level) {
    data |>
      summarise_all(n_distinct) |>
      select_if(~ all(. <= level)) |>
      paste_data_vars()
  }