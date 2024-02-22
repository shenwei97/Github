# Git gammar

# 首先
# terminal : 输入  
# which git 
#显示git bash 安装地址

# 设置 config
library(usethis)
edit_git_config()
# 输入
# [https]
# proxy = https://127.0.0.1:7890
# 
# [user]
# name = "Fang xingwei"
# email = "1002515260@qq.com"


use_git()
# create_github_token() # 创建token

# library(gitcreds)
# gitcreds_set() # 将创建的token复制进来


use_github() # 直接链接Git repo
# 如果冲突 使用
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE) 

# git config --global https.proxy https://127.0.0.1:7890  # 设置代理 clash
