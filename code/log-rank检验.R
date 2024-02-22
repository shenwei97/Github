## KM曲线表格导出
perform_logrank_test <- function(predictor, time, status, data) {
  formula <- as.formula(paste("Surv(", time, ",", status, ") ~ ", predictor))
  km <- survdiff(formula, data = data)
  fit <- survfit(formula, data = data)
  chisq <- km$chisq |> round(3)
  pvalue <- km$pvalue |> round(3)
  
  outcome1 <- summary(fit)$table |>
    as.data.frame() |>
    dplyr::select(records, median)
  outcome1$chisq <- chisq
  outcome1$pvalue <- pvalue
  # outcome1$var <- predictor
  # outcome1 <- remove_rownames(outcome1)
  return(outcome1)
}
