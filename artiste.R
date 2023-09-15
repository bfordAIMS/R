
png2grob <- function(path_to_png) {
  library(png)
  path_to_png |> 
    png::readPNG() |> 
    grid::rasterGrob(interpolate = TRUE)
}
blank_ggplot <- function(){
  data.frame(x = 0:10/10, y = 0:10/10) |> 
    ggplot(aes(x, y)) + 
    theme_void()
}
annotate_grob <- function(plot, grob, xmin, xmax, ymin, ymax, data_scale = FALSE){
  library(ggplot2)
  if (!data_scale) {
    # Min and max values from plot axes
    min_x <- layer_scales(plot)$x$range$range[1]
    max_x <- layer_scales(plot)$x$range$range[2]
    min_y <- layer_scales(plot)$y$range$range[1]
    max_y <- layer_scales(plot)$y$range$range[2]
    x_len <- max_x - min_x
    y_len <- max_y - min_y
    # Create grob boundaries as scaled versions of min/max values
    xmin <- min_x + x_len*xmin
    xmax <- min_x + x_len*xmax
    ymin <- min_y + y_len*ymin
    ymax <- min_y + y_len*ymax
  }
  plot + annotation_custom(
    grob, xmin, xmax, ymin, ymax
  )
}
annotate_text <- function(plot, text, x, y, ..., data_scale = FALSE){
  library(ggplot2)
  if (!data_scale) {
    # Min and max values from plot axes
    min_x <- layer_scales(plot)$x$range$range[1]
    max_x <- layer_scales(plot)$x$range$range[2]
    min_y <- layer_scales(plot)$y$range$range[1]
    max_y <- layer_scales(plot)$y$range$range[2]
    len_x <- max_x - min_x
    len_y <- max_y - min_y
    # Create text position (x & y)
    x <- min_x + len_x*x
    y <- min_y + len_y*y
  }
  plot + annotate(
    "text", x, y, 
    label = text, 
    ...
  )
}