#desc: rms validation output change to C_index
#data: validate output from rms package
#example:

# v <- validate(fit2, method = "boot", B = 1000, dxy = T)
# calculate_c_index(v) 


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