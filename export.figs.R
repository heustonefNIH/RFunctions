export.figs <- function(plot.name = NULL, plot.fig = NULL, plot.height = 800, plot.width = 800){
  png(filename = plot.name, height = plot.height, width = plot.width)
  plot(plot.fig)
  dev.off()
}
