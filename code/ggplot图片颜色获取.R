library(scales)

#extract hex color codes for a plot with three elements in ggplot2 
hex <- hue_pal()(3)

#overlay hex color codes on actual colors
show_col(hex)
