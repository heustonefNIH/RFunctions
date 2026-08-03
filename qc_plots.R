# Notes -------------------------------------------------------------------

# For plotting standard QC metrics during preprocessing


# Load libraries ----------------------------------------------------------

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
		file.id = NULL,
		outdir = NULL,
		cols = color.palette, 
		show_plot = FALSE,
		px.width = 800,
		px.height = 800
)
{
	if(any(is.null(file.id), is.null(outdir))){
		warning("file.id and/or outdir not specified; not saving figures")
	} else {
		save.flag <- TRUE
	}
	metadata <- seurat.object@meta.data
	
	# Visualize the number UMIs/transcripts per cell
	plot.title <- paste0(file.id, "-nCount_RNA_qc")
	p1 <- metadata %>% 
		ggplot(aes(x=nCount_RNA)) + 
		geom_density(alpha = 0.2) + 
		scale_x_log10() + 
		theme_classic() +
		ylab("Cell density") +
		geom_vline(xintercept = 500) + 
		ggtitle(plot.title)
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the distribution of genes detected per cell via histogram
	plot.title <- paste0(file.id, "-nFeature_RNA_qc")
	p1 <- metadata %>% 
		ggplot(aes(x=nFeature_RNA)) + 
		geom_density(alpha = 0.2) + 
		theme_classic() +
		scale_x_log10() + 
		geom_vline(xintercept = 300) + 
		ggtitle(plot.title)
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the distribution of mitochondrial gene expression detected per cell
	plot.title <- paste0(file.id, "-percent.mt_qc")
	p1 <- metadata %>% 
		ggplot(aes(x=percent.mt)) + 
		geom_density(alpha = 0.2) + 
		theme_classic() +
		geom_vline(xintercept = 0.2) + 
		ggtitle(plot.title)
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the distribution of mitochondrial gene expression detected per cell
	plot.title <- paste0(file.id, "-percent.ribo_qc")
	p1 <- metadata %>% 
		ggplot(aes(x=percent.ribo)) + 
		geom_density(alpha = 0.2) + 
		theme_classic() +
		geom_vline(xintercept = 0.2) + 
		ggtitle(plot.title)
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}	
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the overall complexity of the gene expression by visualizing the genes detected per UMI
	plot.title <- paste0(file.id, "-log10GenesPerUMI_qc")
	p1 <- metadata %>%
		ggplot(aes(x=log10GenesPerUMI)) +
		geom_density(alpha = 0.2) +
		theme_classic() +
		geom_vline(xintercept = 0.8) + 
		ggtitle(plot.title)
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
	
	# Visualize the correlation between genes detected and number of UMIs and determine whether strong presence of cells with low numbers of genes/UMIs
	plot.title <- paste0(file.id, "-CountFeaturePctmt_qc")
	p1 <- metadata %>% 
		ggplot(aes(x=nCount_RNA, y=nFeature_RNA)) + 
		geom_point(aes(color=percent.mt)) + 
		scale_colour_gradient(low = "gray90", high = "black") +
		stat_smooth(method=lm) +
		scale_x_log10() + 
		scale_y_log10() + 
		theme_classic() +
		geom_vline(xintercept = 500) +
		geom_hline(yintercept = 250) + 
		ggtitle(plot.title)
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
}


# Variable Feature plots --------------------------------------------------

var.feature_plots <- function(
		seurat.object,
		file.id = NULL,
		outdir = NULL,
		top_label = 10,
		with_labels = TRUE, 
		show_plot = FALSE, 
		px.width = 800,
		px.height = 800
)
{
	if(any(is.null(file.id), is.null(outdir))){
		warning("file.id and/or outdir not specified; not saving figures")
	} else {
		save.flag <- TRUE
	}
	
	plot.title <- paste0(file.id, "-VarFeaturePlot")
	#ID top genes
	top_genes <- head(VariableFeatures(seurat.object), top_label)
	
	# plot variable features with and without labels
	p1 <- VariableFeaturePlot(seurat.object)
	
	if(with_labels){
		p1 <- LabelPoints(plot = p1, points = top_label, repel = TRUE)
	}
	p1 <- p1 +
		ggtitle(plot.title)
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.eight)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
}

# VizDimLoadings plot -----------------------------------------------------

dimloadings_plot <-  function(
		seurat.object,
		file.id = NULL,
		outdir = NULL,
		dims = 1:2,
		reduction = "pca",
		show_plot = FALSE, 
		px.width = 800,
		px.height = 1600
)
{
	if(any(is.null(file.id), is.null(outdir))){
		warning("file.id and/or outdir not specified; not saving figures")
	} else {
		save.flag <- TRUE
	}
	plot.title <- paste0(file.id, "-VizDimLoadings")
	
	if(dims > 1 && length(dims) == 1){
		print(paste("entered", dims, "dims; converting to", seq_len(dims)))
		dims <- seq_len(dims)
	}
	
	# plot variable features with and without labels
	p1 <- VizDimLoadings(
		seurat.object, 
		dims = dims, 
		reduction = reduction)
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
}


# DimHeatmap plot ---------------------------------------------------------

dimheatmap_plot <-  function(
		seurat.object,
		file.id = NULL,
		outdir = NULL,
		dims = 1:10,
		cells = 500,
		balanced = TRUE,
		show_plot = FALSE,
		px.width = 800,
		px.height = 800
		
)
{
	if(any(is.null(file.id), is.null(outdir))){
		warning("file.id and/or outdir not specified; not saving figures")
	} else {
		save.flag <- TRUE
	}
	plot.title <- paste0(file.id, "-DimHeatmap")
	
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
	
	if(save.flag){
		png(filename = file.path(outdir, paste0(plot.title, ".png")), width = px.width, height = px.height)
		plot(p1)
		dev.off()
	}
	if(show_plot == TRUE){
		plot(p1)
	}
}
