# Notes -------------------------------------------------------------------

# script to run doublet finder
# based on tutorial from https://www.youtube.com/watch?v=NqvAS4HgmrE & https://github.com/kpatel427/YouTubeTutorials/blob/main/singleCell_doublets.R <Biomagician>


# Best practices ----------------------------------------------------------

# Run on filtered data
# Run on each sample separately
# Samples need to be normalized and plotted

# DoubletFinder -----------------------------------------------------------

library(DoubletFinder)
library(ggplot2)

runDoubletFinder <- function(
		seurat.object = NULL, 
		seurat.identifier = NULL,
		cluster.dims = 15, 
		sctransformed = FALSE, 
		predicted.doubletRate = 0.05, 
		logger = NULL,
		pANN.reuse = NULL, 
		show.plots = FALSE
){
	
	#Find elbow inflection point to use in runDoubletFinder & log result
	pcs_stdev <- seurat.object[["pca"]]@stdev
	cum_var <- cumsum(pcs_stdev^2) / sum(pcs_stdev^2)
	knee <- which(cum_var >= var.explained)[1]  # raise threshold to be less conservative, e.g. 0.85, 0.90
	dims_use <- 1:knee
	
	log.msg(logger, msg = paste("Using", knee, "dims to define elbow inflection point in", seurat.identifier))
	
	
	## pK Identification (no ground-truth) ---------------------------------------------------------------------------------------
	sweep.res.list <- paramSweep(seurat.object, PCs = 1:cluster.dims, sct = sctransformed)
	sweep.stats <- summarizeSweep(sweep.res.list, GT = FALSE)
	bcmvn <- find.pK(sweep.stats)
	
	if(show.plots){
		ggplot(bcmvn, aes(pK, BCmetric, group = 1)) +
			geom_point() +
			geom_line()
	}	
	pK <- bcmvn %>% # select the pK that corresponds to max bcmvn to optimize doublet detection
		filter(BCmetric == max(BCmetric)) %>%
		select(pK) 
	pK <- as.numeric(as.character(pK[[1]]))
	log.msg(logger, msg = paste("Found pK =", pK, "in", seurat.identifier))

	## Homotypic Doublet Proportion Estimate -------------------------------------------------------------------------------------
	annotations <- seurat.object@meta.data$seurat_clusters
	homotypic.prop <- modelHomotypic(annotations) 
	nExp_poi <- round(nrow(seurat.object@meta.data) * predicted.doubletRate) 
	nExp_poi.adj <- round(nExp_poi*(1-homotypic.prop))
	
	seurat.object <- doubletFinder(seurat.object,
																 PCs = 1:cluster.dims, 
																 pK = pK, 
																 nExp = nExp_poi, 
																 reuse.pANN = pANN.reuse,
																 sct = sctransformed)
	names(seurat.object@meta.data)[grep("DF.cl", names(seurat.object@meta.data))] <- "DF.classifications"
	names(seurat.object@meta.data)[grep("pANN", names(seurat.object@meta.data))] <- "pANN"
	
	return(seurat.object)
}
