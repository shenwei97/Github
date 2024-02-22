

paste_data_vars <- function(data){
  col_names <- data |> colnames()
  paste0("'",paste(col_names, collapse = "','"), "'")
}


