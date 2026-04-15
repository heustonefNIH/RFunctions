library(reticulate)

# py_install("scrublet")

runscrublet <- function(seurat.object){
	
	scr <- import("scrublet")
	counts <- t(as.matrix(GetAssayData(seurat.object, slot = "counts")))
	
	scrub <- scr$Scrublet(counts)
	results <- scrub$scrub_doublets()
	
	# Add results back to Seurat metadata
	seurat.object$scrublet_score <- results[[1]]
	seurat.object$is_doublet <- results[[2]]
	
	return(seurat.object)
}