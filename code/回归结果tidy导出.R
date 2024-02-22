multinom(
  Subgroup ~ srh + age + gender, # 主要调整自变量
  data = data) |> broom::tidy(conf.int =T) |> 
  filter(term == 'srh1') |> 
  select(1,3,7,8,6) |> 
  mutate(
    across(2:4,~exp(.x)),
    across(2:4,~round(.x,3))
  ) |> 
  mutate(
    'OR(95CI%)' = paste0(estimate,'(',conf.low,',',conf.high,')')
  )
  
