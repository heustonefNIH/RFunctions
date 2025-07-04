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

runDoubletFinder <- function(seurat.object = NULL, cluster.dims = NULL, sctransformed = FALSE, predicted.doubletRate = 0.05, pANN.reuse = NULL){
	
	print("running DoubletFinder")
	if(is.null(cluster.dims)){
		cluster.dims = 20
		print("Using default cluster.dims = 20")
	} else{
		cluster.dims = cluster.dims
	}
	## pK Identification (no ground-truth) ---------------------------------------------------------------------------------------
	
	sweep.res.list <- paramSweep(seurat.object, PCs = 1:cluster.dims, sct = sctransformed)
	sweep.stats <- summarizeSweep(sweep.res.list, GT = FALSE)
	bcmvn <- find.pK(sweep.stats)
	
	pK <- bcmvn %>% # select the pK that corresponds to max bcmvn to optimize doublet detection
		filter(BCmetric == max(BCmetric)) %>%
		select(pK) 
	pK <- as.numeric(as.character(pK[[1]]))
	print(paste0("Found pK = ", pK))
	
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
