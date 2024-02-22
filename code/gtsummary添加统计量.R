## 统计描述添加统计量



# 卡方 ----------------------------------------------------------------------
my_chisq <- function(data, variable, by, ...) {
  stats::chisq.test(data |> pull(variable) ,as.factor(data |> pull(by)))  |> 
    broom::tidy() |>  select(statistic)
}



# 配对卡方 ----------------------------------------------------------------------
my_mcnemar_test <- function(data, variable, by, ...) {
  stats::mcnemar.test(data |> pull(variable) ,as.factor(data |> pull(by)))  |> 
    broom::tidy() |>  select(statistic)
}


## 统计描述
# chisq_vars <- data1 |> select(1:14,size1_class,nlr_class) |> select(-c("sex",'plt','child','liver_tranfer')) |> colnames()
data1 |> select(1:14,size1_class,nlr_class) |> 
  select(-nlr1) |> 
  mutate_if(is.numeric,as.factor) |> 
  tbl_summary(by = nlr_class) |>
  add_stat_label() |> 
  add_stat(fns = everything() ~ my_chisq) |> 
  modify_header(
    list(
      statistic ~ "**Chi-squared**"
    )) |> 
  add_p( pvalue_fun = function(x) style_pvalue(x, digits = 3)) |> bold_labels()



# kruskal.test检验 ----------------------------------------------------------


my_kruskal.test <- function(data, variable, by, ...) {
  stats::kruskal.test(data |> pull(variable) ,as.factor(data |> pull(by)))  |> 
    broom::tidy() |>  select(statistic) |> signif(digits = 3)
}

my_kruskal.test <- function(data, variable, by, ...) {
  test <- stats::kruskal.test(data |> pull(variable) ,as.factor(data |> pull(by))) 
  statistic <- signif(test$statistic ,digits = 4)
  return(statistic)
}


test <- stats::kruskal.test(data1 |> pull('value') ,as.factor(data1 |> pull('name'))) 
statistic <- signif(test$statistic ,digits = 4)


# ---------------------------------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(officer)
library(gtsummary)


## 检验控制方法

data1 |> 
  select(status,nlr1,nlr2,liver) |> 
  tbl_summary(
    by= status,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
  ) |> 
  add_p(list(c(nlr1,nlr2) ~ "t.test", 
             liver ~ 'chisq.test'),
        pvalue_fun = ~ style_pvalue(.x, digits = 3)) 


#> #BlackLivesMatter

### Effect size
my_ES_test <- function(data, variable, by, ...) {
  aovmod = aov(data[[variable]] ~ data[[by]])
  lsr::etaSquared(aovmod)[1,1]
}

### Standard Error Mean
sem <- function(x){
  sqrt(var(x, na.rm=TRUE)/sum(!is.na(x)))
}

### Pooled Standard Error
PSE <- function(data, variable, by,...) {
  s <- data %>% 
    group_by(!!sym(by)) %>% 
    summarise(s = var(!!sym(variable)), 
              n = n()) %>% 
    mutate(num = s*(n-1))
  psd <- sqrt(sum(s$num)/(sum(s$n) - nrow(s)))
  psd*sqrt(sum(1/s$n))
}

### gtsummary
iris %>%
  select(names(iris))%>% 
  tbl_summary(
    by = Species,
    statistic = all_continuous() ~ "{mean} ± {sem}"
  ) %>% 
  add_p(
    test = all_continuous() ~ "aov", 
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  add_stat(fns = all_continuous() ~ PSE) %>% 
  add_stat(fns = all_continuous() ~ my_ES_test)  %>% 
  modify_header(label = "**Size**", p.value = "**p-value**", add_stat_1 = "**PSE**", add_stat_2 = "**\U03B7\U00B2**") %>%
  modify_footnote(add_stat_1 = "Pooled Standard Error", abbreviation = FALSE) %>%
  modify_fmt_fun( c(add_stat_1, add_stat_2) ~ purrr::partial(style_sigfig, digits = 5)) %>%
  as_kable()

