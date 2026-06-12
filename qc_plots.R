# Notes -------------------------------------------------------------------

# For plotting standard QC metrics during preprocessing


# Load libraries ----------------------------------------------------------

local.functions <- paste(
	file.path(
		RFunctions, 
		c("named_palettes.R"))
)

tryCatch({
	pacman::p_load(dplyr, Seurat, patchwork, cowplot, ggplot2, qs2, install = F)
	invisible(sapply(local.functions, source))
	setwd(rna.dir)
}, error = function(e){
	message("Error in setup: ", e$message)
})

# Metric plots ---------------------------------------------------------------

qc_plots <- function(
		seurat.object,
		file.prefix,
		cols = color.palette, 
		show_plot = FALSE,
		px.width = 800,
		px.height = 800
)
{
	metadata <- seurat.object@meta.data
	# Visualize the number UMIs/transcripts per cell
	p1 <- metadata %>% 
		ggplot(aes(x=nCount_RNA)) + 
		geom_density(alpha = 0.2) + 
		scale_x_log10() + 
		theme_classic() +
		ylab("Cell density") +
		geom_vline(xintercept = 500)
	png(filename = paste0(file.prefix, "-nCount_RNA_qc.png"), width = px.width, height = px.height)
	plot(p1)
	dev.off()
	
	# Visualize the distribution of genes detected per cell via histogram
	p1 <- metadata %>% 
		ggplot(aes(x=nFeature_RNA)) + 
		geom_density(alpha = 0.2) + 
		theme_classic() +
		scale_x_log10() + 
		geom_vline(xintercept = 300)
	png(filename = paste0(file.prefix, "-nFeature_RNA_qc.png"), width = px.width, height = px.height)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the distribution of mitochondrial gene expression detected per cell
	p1 <- metadata %>% 
		ggplot(aes(x=percent.mt)) + 
		geom_density(alpha = 0.2) + 
		scale_x_log10() + 
		theme_classic() +
		geom_vline(xintercept = 0.2)
	png(filename = paste0(file.prefix, "-percent.mt_qc.png"), width = px.width, height = px.height)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the distribution of mitochondrial gene expression detected per cell
	p1 <- metadata %>% 
		ggplot(aes(x=percent.ribo)) + 
		geom_density(alpha = 0.2) + 
		scale_x_log10() + 
		theme_classic() +
		geom_vline(xintercept = 0.2)
	png(filename = paste0(file.prefix, "-percent.ribo_qc.png"), width = px.width, height = px.height)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the overall complexity of the gene expression by visualizing the genes detected per UMI
	p1 <- metadata %>%
		ggplot(aes(x=log10GenesPerUMI)) +
		geom_density(alpha = 0.2) +
		theme_classic() +
		geom_vline(xintercept = 0.8)
	png(filename = paste0(file.prefix, "-log10GenesPerUMI_qc.png"), width = px.width, height = px.height)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}

		# Visualize the correlation between genes detected and number of UMIs and determine whether strong presence of cells with low numbers of genes/UMIs
	p1 <- metadata %>% 
		ggplot(aes(x=nCount_RNA, y=nFeature_RNA)) + 
		geom_point(aes(color=percent.mt)) + 
		scale_colour_gradient(low = "gray90", high = "black") +
		stat_smooth(method=lm) +
		scale_x_log10() + 
		scale_y_log10() + 
		theme_classic() +
		geom_vline(xintercept = 500) +
		geom_hline(yintercept = 250)
	png(filename = paste0(file.prefix, "-CountFeaturePctmt_qc.png"), width = px.width, height = px.height)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}
}


# Variable Feature plots --------------------------------------------------

var.feature_plots <- function(
		seurat.object,
		file.prefix,
		file.suffix = NULL,
		top_label = 10,
		with_labels = TRUE, 
		show_plot = FALSE, 
		px.width = 800,
		px.height = 800
)
{
	if(is.null(file.suffix)){
		file.name <- paste0(file.prefix, "-varFeaturePlot.png")
	} else {
		file.name <- paste0(file.prefix, "-varFeaturePlot-", file.suffix, ".png")
	}
	#ID top genes
	top_genes <- head(VariableFeatures(seurat.object), top_label)
	
	
	# plot variable features with and without labels
	p1 <- VariableFeaturePlot(seurat.object)
	
	if(with_labels){
		p1 <- LabelPoints(plot = p1, points = top_label, repel = TRUE)
	}
	png(filename = file.name, width = px.width, height = px.eight)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}
}


# VizDimLoadings plot -----------------------------------------------------

dimloadings_plot <-  function(
		seurat.object,
		file.prefix,
		file.suffix = NULL,
		dims = 1:2,
		reduction = "pca",
		show_plot = FALSE, 
		px.width = 800,
		px.height = 1600
)
{
	if(is.null(file.suffix)){
		file.name <- paste0(file.prefix, "-dimLoadings.png")
	} else {
		file.name <- paste0(file.prefix, "-dimLoadings-", file.suffix, ".png")
	}
	
	if(dims > 1 && length(dims) == 1){
		print(paste("entered", dims, "dims; converting to", seq_len(dims)))
		dims <- seq_len(dims)
	}
	
	# plot variable features with and without labels
	p1 <- VizDimLoadings(
		seurat.object, 
		dims = dims, 
		reduction = reduction)
	
	png(filename = file.name, width = px.width, height = px.height)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}
}


# DimHeatmap plot ---------------------------------------------------------

dimheatmap_plot <-  function(
		seurat.object,
		file.prefix,
		file.suffix = NULL,
		dims = 1:10,
		cells = 500,
		balanced = TRUE,
		show_plot = FALSE,
		px.width = 800,
		px.height = 800
		
)
{
	if(is.null(file.suffix)){
		file.name <- paste0(file.prefix, "-dimLoadings.png")
	} else {
		file.name <- paste0(file.prefix, "-dimLoadings-", file.suffix, ".png")
	}
	
	if(dims > 1 && length(dims) == 1){
		print(paste("entered", dims, "dims; converting to", seq_len(dims)))
		dims <- seq_len(dims)
	}
	
	ncol.heatmap <- ceiling(sqrt(length(dims)))
	# plot variable features with and without labels
	p1 <- DimHeatmap(
		seurat.object, 
		dims = dims, 
		cells = cells,
		balanced = TRUE, 
		width = px.width,
		height = px.height
	)
	
	png(filename = file.name, width = px.width, height = px.height)
	plot(p1)
	dev.off()
	if(show_plot == TRUE){
		plot(p1)
	}
}
