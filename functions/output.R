# 表格导出
save_flextable_as_docx <- function(table, filename) {
  library(flextable)
  # 指定保存的路径
  output_path <- paste0(here(),'/output/tbl')
  # 创建完整的文件路径
  file_path <- file.path(output_path, paste0(filename, ".docx"))
  # 保存表格为 Word 文档
  table |> as_flex_table() |> flextable::save_as_docx(path = file_path)
}



# 使用示例
desc1  |> save_flextable_as_docx("desc1")


save_ggplot <- function(plot, filename,width = 6, height = 4.5, dpi = 300) {
  library(ggplot2)
  # 指定保存的路径
  output_path <- paste0(here(),'/output/fig')
  # 创建完整的文件路径
  file_path <- file.path(output_path, paste0(filename, ".pdf"))
  # 保存图表
  ggsave(file_path, plot = plot,  units = "in",width = width, height = height, dpi = dpi)
}


save_ggplot(plot.km1$plot, "my_plot1")
ggsave(plot = last_plot(),"my_plot1.png")






