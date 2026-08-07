embedding.functions <- function(
		seurat.object,
		reduction.method,
		assay_use = "RNA",
		dims_use = 1:30,
		cluster_name = NULL,
		reduction_name = NULL,
		resolution_use = 0.5,
		n.components_use = 2L, 
		seed.use = 42L,
		exact_neighbors = FALSE
)
{
	if(is.null(cluster_name)){
		cluster_name <- paste0(reduction.method, "_clusters")
		message("Cluster metadata column: ", cluster_name)
	}
	if(is.null(reduction_name)){
		reduction_name <- paste0("umap.", reduction.method)
		message("UMAP reduction name: ", reduction_name)
	}
	
	#confirm seed used
	set.seed(seed.use)
	
	if(isTRUE(exact_neighbors)){
		seurat.object <- FindNeighbors(
			object = seurat.object, 
			dims = dims_use, 
			k.param = 20, 
			nn.method = "rann", 
			nn.eps = 0, 
			reduction = reduction.method
		)
	} else{
		seurat.object <- FindNeighbors(
			seurat.object, 
			dims = dims_use, 
			k.param = 20,
			nn.method = "annoy",
			annoy.metric = "euclidean",
			n.trees = 50,
			reduction = reduction.method
		)
		
	}
	seurat.object <- FindClusters(
		seurat.object, 
		resolution = resolution_use, 
		cluster.name = cluster_name, 
		random.seed = seed.use
	)
	
	seurat.object <- RunUMAP(
		seurat.object, 
		dims = dims_use,
		n.neighbors = 30L,
		min.dist = 0.3,
		assay = assay_use, 
		reduction.name = reduction_name,
		reduction = reduction.method, 
		n.components = n.components_use, 
		seed.use = seed.use
		)
	
	return(seurat.object)
}