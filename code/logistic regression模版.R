# 1. Logistic Regression ----------------------------------------------------------------------
## load library
require(rms)
require(broom)
require(pROC)
require(rmda)
require(ggforestplot)

data <- trial
theme_gtsummary_compact()

# 1.1 univariable regression ------------------------------------------------------------------


uvreg1 <-
  data |>
  tbl_uvregression(
    method = glm,
    y = response, # 修改y
    method.args = list(family = binomial),
    exponentiate = T,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_labels() |>
  bold_p(t = 0.1)

show_header_names(uvreg1)

uvreg1 |>
  modify_caption("**Table2: Univariable regression** ")

# 1.2 Multivariate regression ------------------------------------------------------------------------------


# 全变量方程 替换y
fit <- glm(response ~ ., data = data, family = binomial())

mvreg1 <- fit |>
  tbl_regression(
    exponentiate = T,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) |>
  bold_labels() |>
  bold_p(t = 0.05)
mvreg1 |>
  modify_caption("**Table3:  Multivariate regression** ")


#合并单因素多因素
tbl_merge(
  tbls = list(uvreg1, mvreg1),
  tab_spanner = c("**Univariable regression**", "**Multivariate regression**")
)|>
  modify_caption("**Table4:  Regression analysis** ")

# 1.3 模型验证 ------------------------------------------------------------------------------------


## 1.3.1内部验证 -----------------------------------------------------------------------------------


fit1 <- lrm(response ~ ., data = data, x = TRUE, y = TRUE)
fit1

rcorrcens(response ~ predict(fit1), data = data)


calculate_c_index <- function(data) {
  Dxy <- v[rownames(v) == "Dxy", colnames(v) == "index.corrected"]
  orig_Dxy <- v[rownames(v) == "Dxy", colnames(v) == "index.orig"]
  # The c-statistic according to Dxy=2(c-0.5)
  bias_corrected_c_index <- abs(Dxy) / 2 + 0.5
  orig_c_index <- abs(orig_Dxy) / 2 + 0.5
  out <- tibble(
    "原C指数" = orig_c_index,
    "校正C指数" = bias_corrected_c_index
  )
  return(out)
}

# bootstrap
v <- validate(fit1, method = "boot", B = 1000, dxy = T)
calculate_c_index(v)

# cross-validation
v <- validate(fit1, method = "crossvalidation", B = 10, dxy = T)
calculate_c_index(v)


## 1.3.2 ROC曲线 ---------------------------------------------------------------------------------------



fig.roc <- roc(response ~ predict(fit1), data = data)
pROC::ci.auc(fig.roc)
pROC::coords(fig.roc,x='best')
1 / (1 + exp(-pROC::coords(fig.roc,x='best')[[1]])) # 发生的概率 1 / (1 + exp(-值))


pdf("output/fig_roc.pdf", width = 11.69, height = 8.27)
ggroc(fig.roc,
  legacy.axes = T, # x轴1- 特异度
  colour = "#036299", # ROC 颜色
  linetype = 1,
  linewidth = 0.8
) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black", linewidth = 0.8) +
  annotate("text", x = 0.75, y = 0.5, label = paste("AUC=", round(fig.roc$auc, 3))) +
  theme_classic() +
  labs(title = "ROC Curve")
dev.off()


## 1.3.3 校准曲线---------------------------------------------------------------------------------------


fig.cal <- calibrate(fit1, method = "boot", B = 1000)
pdf("output/fig_cal.pdf", width = 11.69, height = 8.27)
plot(fig.cal,
  xlab = "Predicted probability",
  ylab = "Observed probability",
  sub = F
)
dev.off()

pdf("fig_cal1.pdf", width = 11.69, height = 8.27)
val.prob(predict(fit1,type=c("fitted")), # 预测值
         as.numeric(as.character(data$response)),
         xlab="Predicted Probability", 
         ylab="Actual Probability", 
         lim=c(0, 1))
dev.off()

# hosmer and Lemeshow goodness of fit (GOF) test
ResourceSelection::hoslem.test(data$response, fitted(fit))

# spiegelhalter z检验
fitvalue <- if_else(
  predictrms(fit1) > pROC::coords(fig.roc,x='best')[[1]],1,0)

table(fitvalue)
table(data$response)
prop.test(table(fitvalue),table(data$response))
## 1.3.4 DCA and CIC Curse---------------------------------------------------------------------------------------


# 分类变为数值
# data <- data |>
#   mutate(年龄分类 = fct_recode(年龄分类, "0" = "<36", "1" = ">=36")) |>
#   mutate(across(all_of(var_fct), as.numeric), # 自变量必须为数值
#     诊断 = if_else(诊断 == 1, 0, 1) # 因变量必须为0 1
#   )

fig.dca <- decision_curve(
  data = data,
  fit1$sformula, # formula
  family = binomial(link = "logit"),
  thresholds = seq(0, 1, by = 0.01),
  confidence.intervals = 0.95
)

#DCA
pdf("output/fig_dca.pdf", width = 11.69, height = 8.27)
plot_decision_curve(fig.dca,
  curve.names = "Dca model", # 曲线名称
  xlab = "Threshold probability", # x轴名称
  cost.benefit.axis = FALSE, col = "#036299",
  confidence.intervals = FALSE,
  standardize = FALSE
)
dev.off()

# CIC
pdf("output/fig_cic.pdf", width = 11.69, height = 8.27)
plot_clinical_impact(fig.dca,
  population.size = 1000,
  cost.benefit.axis = T,
  n.cost.benefits = 8, col = c("red", "#036299"),
  confidence.intervals = T
)
dev.off()


## 1.3.5 nomogram---------------------------------------------------------------------------------------


# label(data$age) <-  "age of patient" # 修改列线图刻度

ddist <- datadist(data)
options(datadist = "ddist")
fig.nom <- nomogram(fit1,
  fun = function(x) 1 / (1 + exp(-x)), # or fun=plogis
  fun.at = seq(0.1, 0.9, by = 0.1)
)

pdf("output/fig_nom.pdf", width = 11.69, height = 8.27)
plot(fig.nom)
dev.off()


## 1.3.6 森林图 ---------------------------------------------------------------------


df.ggforest <- fit |>
  tidy() |>
  slice(-1)

pdf("output/fig_forest.pdf", width = 11.69, height = 8.27)
forestplot(
  df = df.ggforest,
  name = term,
  estimate = estimate,
  logodds = TRUE,
  se = std.error,
  pvalue = p.value,
  psignif = 0.05,
  xlab = "Odds ratio (95% CI)"
  # tilte = ''
)
dev.off()


