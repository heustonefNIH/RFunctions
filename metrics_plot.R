# Function to go through table and iterage gg dotplot all columns against one column
# Requires y_var col to be numeric or will fail
# Set plot.live = TRUE if you want it to draw the plots for you as well as print them

metrics_plot <- function(batch_df, x_var, y_var, plot.live = FALSE){
  #fix strings
  if(is.character(x_var)){
    x_var <- sym(x_var)
  }
  if(is.character(y_var)){
    y_var <- sym(y_var)
  }
  
  #define output file name
  plot_file <- paste0(
    as_string(ensym(x_var)), 
    "_v_",
    y_var, 
    ".png"
  )
  
  #define y axis as log or linear
  range_ratio <- batch_df %>% 
    summarize(range_ratio = max({{y_var}})/min({{y_var}})) %>% 
    pull(range_ratio)
  if(range_ratio > 100){
    y.lim <- c(1, NA)
    y.scale <- "log10"
  }else{
    y.lim <- c(0, NA)
    y.scale <- "identity"
  }
  
  #Plot function
  p <- ggplot(batch_df, aes(x = {{ x_var }}, y = {{ y_var }})) + 
    geom_boxplot(outliers = F) + 
    geom_point(size = 5) + 
    scale_y_continuous(limits = y.lim, transform = y.scale) +
    theme_bw()
  ggsave(filename = plot_file, path = "metadata_plots/", bg = "transparent")
  if(plot.live == TRUE){
    plot(p)
    }
}
