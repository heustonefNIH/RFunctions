# Notes ---------------------------------------------------------

# Changing this to a sourced file. Everything is pretty much plug and play

# Unintegrated plots ----------------------------------------------------------
unintegrated.plots <- function(
		seurat.object = seurat.object, 
		cols = color.palette, 
		compare.by = compare.by
)
{
	print("Making pictures!")
	
	adip.cols <- c(
		"mast_cell" = "red",
		"adipocyte" = "orange",
		"ASPC" = "green",
		"endothelial" = "blue",
		"SMC" = "lightcoral",
		"LEC" = "dodgerblue",
		"macrophage" = "darkviolet",
		"mesothelium" = "tan",
		"dendritic_cell" = "aquamarine", 
		"endometrium" = "black",
		"t_cell" = "maroon",
		"monocyte" = "cadetblue",
		"b_cell" = "khaki4",
		"pericyte" = "mediumpurple1",
		"Unassigned" = "gray"
	)
	
	named.color.palette <- color.palette[1:length(levels(seurat.object@active.ident))]
	names(named.color.palette) <- levels(seurat.object@active.ident)
	
	# Pre integration plots ---------------------------------------------------
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object, 
			reduction = "pca", 
			cols = color.palette, 
			shuffle = T, 
			label = T, 
			label.size = 7, 
			repel = T, 
			group.by = compare.by) + 
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-pca-unintegrated.png"), 
				height = 1000, width = 1000, bg = "transparent")
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	tryCatch({
		#elbowplot
		ElbowPlot(seurat.object, ndims = 30) +
			transparent.theme
		
		png(filename = paste0("pngs/", rnaProject, "-pca-elbowPlot-unintegrated-", object.name, ".png"), 
				height = 1000, width = 1000, bg = "transparent")
		ElbowPlot(seurat.object, ndims = 30) +
			transparent.theme
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	tryCatch({
		p1 <- VizDimLoadings(seurat.object, 
												 dims = 1:10, 
												 reduction = "pca", 
												 ncol = 5)
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-vizDimLoadings.png"), 
				height = 1600, width = 1600, bg = "transparent")
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	# Plotting unintegrated UMAPS ---------------------------------------------
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object, 
			reduction = "umap.unintegrated", 
			cols = color.palette, 
			shuffle = T, 
			label = F, 
			label.size = 7, 
			repel = T, 
			group.by = "orig.ident") + 
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umaps-unintegrated-origIdent.png"), 
				height = 1000, width = 1200)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object, 
			reduction = "umap.unintegrated", 
			cols = color.palette, 
			shuffle = T, 
			label = F, 
			label.size = 7, 
			repel = T, 
			group.by = "unintegrated_clusters") + 
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umaps-unintegrated-clust.png"), 
				height = 1000, width = 1200)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
}

# Integrated plots ----------------------------------------------------------

integrated.plots <- function(
		seurat.object = seurat.object, 
		cols = color.palette, 
		compare.by = compare.by, 
		suffix = NULL
)
{
	print("Making pictures!")
	
	adip.cols <- c(
		"mast_cell" = "red",
		"adipocyte" = "orange",
		"ASPC" = "green",
		"endothelial" = "blue",
		"SMC" = "lightcoral",
		"LEC" = "dodgerblue",
		"macrophage" = "darkviolet",
		"mesothelium" = "tan",
		"dendritic_cell" = "aquamarine", 
		"endometrium" = "black",
		"t_cell" = "maroon",
		"monocyte" = "cadetblue",
		"b_cell" = "khaki4",
		"pericyte" = "mediumpurple1",
		"Unassigned" = "gray"
	)
	
	named.color.palette <- color.palette[1:length(levels(seurat.object@active.ident))]
	names(named.color.palette) <- levels(seurat.object@active.ident)
	
	# Plot integrated UMAPs -----------------------------------------------------
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object, 
			reduction = "umap.rpca", 
			cols = color.palette, 
			shuffle = T, 
			label = F, 
			label.size = 7, 
			repel = T, 
			group.by = "orig.ident") + 
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umap-origIdent", suffix, ".png"), 
				height = 1000, width = 1200)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object, 
			reduction = "umap.rpca", 
			cols = color.palette, 
			shuffle = T, 
			label = F, 
			label.size = 7, 
			repel = T, 
			group.by = compare.by) + 
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umaps-", compare.by, suffix, ".png"), 
				height = 1000, width = 1000)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object, 
			reduction = "umap.rpca", 
			cols = color.palette, 
			shuffle = T, 
			label = F, 
			label.size = 7, 
			repel = T, 
			group.by = "rpca_clusters") + 
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umaps-rc", suffix, ".png"), 
				height = 1000, width = 1000)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	
	# Visualization -----------------------------------------------------------
	
	#do some qc
	tryCatch({
		p1 <- VlnPlot(seurat.object,
									features = c(
										"nCount_RNA", "nFeature_RNA", "percent.mt", 
										"percent.ribo", doublet_score, "prediction.score.max"),
									group.by = "rpca_clusters", 
									pt.size = 0, 
									cols = color.palette, 
									ncol = 3)
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-vln-qc_by_clust", suffix, ".png"), height = 1000, width = 1000)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	
	# ClusterQC plots --------------------------------------------------------
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object, 
			reduction = "umap.rpca", 
			cols = color.palette, 
			group.by = "orig.ident", 
			shuffle = T, 
			label = T
		) + 
			DimPlot(
				seurat.object, 
				reduction = "umap.rpca", 
				cols = named.color.palette, 
				shuffle = T, 
				label = T, 
				label.size = 10
			)
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umap-origID_clusters", suffix, ".png"), 
				height = 1000, width = 2000)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	tryCatch({
		
		pct.table <- seurat.object@meta.data %>%
			dplyr::count(orig.ident, rpca_clusters, name = "pct_cells") %>%
			group_by(rpca_clusters) %>%
			mutate(prop = pct_cells / sum(pct_cells)*100) %>%
			ungroup()
		
		p1 <- ggplot(pct.table, aes(x = orig.ident, y = prop, group = orig.ident, fill = orig.ident)) +
			geom_col(position = "dodge") + 
			facet_wrap(~rpca_clusters) +
			geom_hline(yintercept = 75) +
			theme_bw() + 
			theme(axis.text.x = element_text(angle = 90, hjust = 1))
		plot(p1)
		
		png(filename = paste0("pngs/", rnaProject, "-bar-pct.ident_per_cluster", suffix, ".png"), 
				height = 1000, width = 1000)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	
	
	
	# Cluster tree plots ------------------------------------------------------
	
	tryCatch({
		data.tree <- Tool(object = seurat.object, slot = "BuildClusterTree")
		ape::plot.phylo(x = data.tree, direction = "downwards")
		
		png(filename = paste0("pngs/", rnaProject, "-clusterTree", suffix, ".png"), 
				height = 1000, width = 1000)
		ape::plot.phylo(x = data.tree, direction = "downwards")
		dev.off()
		
		rm(data.tree)
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	
	# Distribution plots ------------------------------------------------------
	
	donor_cluster_props <- seurat.object@meta.data %>%
		dplyr::count(DonorID, !!sym(compare.by), rpca_clusters, name = "n_cells") %>%
		group_by(DonorID) %>%
		mutate(prop = n_cells / sum(n_cells)) %>%
		ungroup()
	
	tryCatch({
		p1 <-	ggplot(donor_cluster_props,
								 aes(x = .data[[compare.by]], y = prop, group = DonorID, colour = DonorID)) +
			geom_point(size = 3) +
			scale_color_manual(values = color.palette) + 
			facet_wrap(~ rpca_clusters, scales = "free_y") +
			theme_bw()
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-cellProp_per_origIdent", suffix, ".png"), 
				height = 1000, width = 1000)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
}





# Plots by assigned cell type' --------------------------------------------


cell.type.plots <- function(
		seurat.object = seurat.object, 
		cols = color.palette, 
		compare.by = compare.by, 
		suffix = NULL
)
{
	print("Making pictures!")
	
	adip.cols <- c(
		"mast_cell" = "red",
		"adipocyte" = "orange",
		"ASPC" = "green",
		"endothelial" = "blue",
		"SMC" = "lightcoral",
		"LEC" = "dodgerblue",
		"macrophage" = "darkviolet",
		"mesothelium" = "tan",
		"dendritic_cell" = "aquamarine", 
		"endometrium" = "black",
		"t_cell" = "maroon",
		"monocyte" = "cadetblue",
		"b_cell" = "khaki4",
		"pericyte" = "mediumpurple1",
		"Unassigned" = "gray"
	)
	
	named.color.palette <- color.palette[1:length(levels(seurat.object@active.ident))]
	names(named.color.palette) <- levels(seurat.object@active.ident)
	
	tryCatch({
		p1 <- VlnPlot(seurat.object,
									features = c("prediction.score.max"),
									group.by = "rpca_clusters", 
									pt.size = 0, 
									cols = color.palette, 
									ncol = 3)
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-vln-pred.score.max_clust", suffix, ".png"), height = 1000, width = 1000)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object,
			reduction = "umap.rpca",
			cols = adip.cols,
			shuffle = T,
			label = F,
			label.size = 7,
			repel = T,
			group.by = "predicted.id") +
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umap-ct", suffix, ".png"), height = 1000, width = 1100)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
	
	tryCatch({
		p1 <- DimPlot(
			seurat.object,
			reduction = "umap.rpca",
			cols = adip.cols,
			shuffle = T,
			label = F,
			label.size = 7,
			repel = T,
			group.by = "predicted.id",
			split.by = "rpca_clusters",
			ncol = ceiling(sqrt(length(unique(seurat.object$predicted.id)))) + 1) +
			transparent.theme
		plot(p1)
		png(filename = paste0("pngs/", rnaProject, "-umap-pred.id_byrc", suffix, ".png"),
				height = 1100, width = 1100)
		plot(p1)
		dev.off()
	}, error = function(e){
		message("Error in plot: ", e$message)
	})
}


