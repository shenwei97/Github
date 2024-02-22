# 加载 MASS 包
library(MASS)

# 假设 data 是包含了 wage 和其他自变量的数据框

# 进行 Box-Cox 变换
bc_results <- boxcox(wage ~ education + experience + age + ethnicity + region + sex + 
                       occupation + sector + union, data = data)

# 找到最佳的 lambda 值
lambda <- bc_results$x[which.max(bc_results$y)]

# 输出最佳 lambda 值
lambda

# 根据找到的 lambda 值对因变量进行变换
data$wage_transformed <- (data$wage^lambda - 1) / lambda  # 进行 Box-Cox 变换

# 建立线性回归模型
model_transformed <- lm(wage_transformed ~ education + experience + age + ethnicity + region + sex + 
                          occupation + sector + union , data = data)

# 显示模型摘要
summary(model_transformed)
performance::check_model(model_transformed)