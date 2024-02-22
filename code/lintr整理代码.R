library(lintr)
lint_dir(path = here())
lint(filename = "script/dat_clean.R")
lint(filename = "script/dat_desc.R")
