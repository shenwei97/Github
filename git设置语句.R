# Git gammar
# which git
# git --version

library(usethis)
create_github_token() # 创建token

library(gitcreds)
gitcreds_set() # 将创建的token复制进来

use_github() # 直接链接Git repo
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE) 

# git config --global https.proxy https://127.0.0.1:7890  # 设置代理 clash