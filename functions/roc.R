# desc: roc单因素绘制，导出图片及cutoff spe sen  threshold单因素结果
# x <- 自变量 '变量1' y <- 因变量 '变量2' data即数据集
# 输出为列表
  
library(patchwork)
library(pROC)
roc_analysis <- function(x, y, data) {
  LOG <- glm(as.formula(paste0(y, "~", x)), data = data, family = binomial())
  pre <- LOG %>% predict(data, type = "response") # 方程得出的预测值
  ROC1 <- pROC::roc(data|> pull(y), levels=c("0", "1"), direction="<", pre,ci=T) # ROC曲线
  
  rocplot <- ggroc(ROC1, 
                   legacy.axes = T,    #x轴1- 特异度
                   colour = "#036299", #ROC 颜色
                   linetype = 1, 
                   linewidth = 0.8) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black", linewidth = 0.8) +
    annotate("text", x = 0.75, y = 0.5, label = paste("AUC=", round(ROC1$auc, 3))) +
    theme_classic() + labs(title = paste0("ROC Curve: ", x))
  
  library(broom)
  # MODEL OUTPUT
  fmt.output <- LOG |> tidy(conf.int=T,exponentiate=T) |> 
    mutate_if(is.numeric, ~sprintf("%.3f", .)) |> slice_tail() |> 
    mutate(p.value = ifelse(p.value < 0.001, "<0.001", p.value))
  
  
  # 提取P值
  #cutoff SP SE
  cutof <- coords(ROC1, "best") %>% round(3)
  #AUC CI
  auc.value <- ROC1 |> pROC::ci.auc()  |>  round(3)
  au <- paste0(auc.value[2],"(",auc.value[1] ,",",auc.value[3],")")
  
  cutoff.prob <- coords(ROC1, "best")[1,1] %>% as.numeric()
  cutoffvalue <- (log(cutoff.prob/(1-cutoff.prob))- summary(LOG)$coefficients[1, 1])/summary(LOG)$coefficients[2, 1] 
  # [log(cutoff/(1-cutoff)) - intercept]/cof
  
  
  outcome <- fmt.output |> bind_cols(cutof) |> 
    mutate(
      cutoff.value = cutoffvalue %>% round(2),
      AUC.CI = au
    )
  
  return(list(outcome,rocplot))
}

# ggsave(plot = roc_nlr[[2]],filename = 'roc_nlr.pdf',width = 6, height = 4.5,path = 'output/fig')


# example

# library(patchwork)
# 
# # 创建一个包含所有变量名的向量
# variables <- colnames(data)[-1]
# 
# # 用于存储 ROC 曲线图的列表
# roc_plots_list <- list()
# cutoff_table_list <- list()
# # 循环应用 roc_analysis 函数并保存图像
# for (variable in variables) {
#   # 生成变量名对应的 ROC 分析结果和绘制的 ROC 曲线图
#   result <- roc_analysis(variable, "诊断", data)
#   roc_plots_list[[variable]] <- result[[2]]  # 存储绘制的 ROC 曲线图
#   cutoff_table_list[[variable]] <-  bind_rows(result[[1]])
# }
# 
# # 将 9 张 ROC 曲线图拼接在一起
# final_plot <- wrap_plots(roc_plots_list, ncol = 3)
# final_cutoff_table <- bind_rows(cutoff_table_list)
# # 保存拼接后的图像
# ggsave(filename = "combined_roc_plots.png", final_plot, width = 15, height = 15, units = "cm")