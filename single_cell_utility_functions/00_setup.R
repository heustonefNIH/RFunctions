# Setup notes -------------------------------------------------------------

# setup.R contains scripts and utility functions necessary for single cell analysis

# Set seed ----------------------------------------------------------------

set.seed(42)

# Source initiating scripts ---------------------------------------------------------

tryCatch({
  source("00_parallelization.R")
  source("01_params.R")
}, error = function(e){
  message("Error finding scripts:", e$message)
})

# Check for organization dirs ------------------------------------------------------

if (!dir.exists(file.path(rna.dir, "pngs"))) {
  dir.create(file.path(rna.dir, "pngs"), recursive = TRUE)
}
if (!dir.exists(file.path(rna.dir, "saved_objects"))) {
  dir.create(file.path(rna.dir, "saved_objects"), recursive = TRUE)
}

# Variables based on params -----------------------------------------------

if(do.sctransform == TRUE){
  rnaProject <- paste(rnaProject, "sct", sep = "_")
}

if("SCRUBLET" %in% toupper(doublet_detction)){
  doublet_score <- "scrublet_score"
} else if ("DOUBLETFINDER" %in% toupper(doublet_detction)){
  doublet_score <- "pANN"
}

# Logging -----------------------------------------------------------------

log.msg <- function(log.file, msg, to.console = TRUE){
  if(to.console == TRUE){
    print(msg)
  }
  time.stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", time.stamp, "] ", msg, "\n")
  cat(log_entry, file = log.file, append = TRUE)
}

# Plotting themes ---------------------------------------------------------

transparent.theme <-  ggplot2::theme(
  panel.background = ggplot2::element_rect(fill='transparent'), #transparent panel bg
  plot.background = ggplot2::element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = ggplot2::element_blank(), #remove major gridlines
  panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
  legend.background = ggplot2::element_rect(fill='transparent'), #transparent legend bg
  legend.box.background = ggplot2::element_rect(fill='transparent') #transparent legend panel
)
