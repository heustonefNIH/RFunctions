library(reticulate)

# py_install("scrublet")

runScrublet <- function(seurat.object){
	
	scr <- import("scrublet")
	counts <- t(as.matrix(GetAssayData(seurat.object, layer = "counts")))
	
	scrub <- scr$Scrublet(counts)
	results <- scrub$scrub_doublets()
	
	# Add results back to Seurat metadata
	seurat.object$scrublet_score <- results[[1]]
	seurat.object$is_doublet <- results[[2]]
	
	return(seurat.object)
}