# Load setup file ---------------------------------------------------------

source("01_setup.R")

# Load libraries ----------------------------------------------------------

local.functions <- paste(
	file.path(
		RFunctions, 
		"log.msg.R")
)

tryCatch({
	pacman::p_load(dplyr, BPCells, Seurat, qs2, SingleCellExperiment, install = F)
	invisible(sapply(local.functions, source))
}, error = function(e){
	message("Error in setup: ", e$message)
})

##session info
log.mca <- paste0(rnaProject, "-MCA.log")
log.msg(log.mca, msg = paste(capture.output(sessionInfo()), collapse = "\n"), 
				overwrite = F, 
				to.console = F)

# Load MCA data ---------------------------------------------------------------

mca.h5ad <- file.path(RFunctions, "LabelTransferObjects/mca_data/MCA_BatchRemoved_Merge_dge_seurat.h5ad")
mca.md.file <- file.path(RFunctions, "LabelTransferObjects/mca_data/MCA_BatchRemoved_Merge_dge_cellinfo.csv")

mca.mtx <- BPCells::open_matrix_anndata_hdf5(mca.h5ad)
dim(mca.mtx)

mca.bp.dir <- file.path(RFunctions, "LabelTransferObjects/mca_data/BPCells/")

if(!dir.exists(file.path(RFunctions, "LabelTransferObjects/mca_data/BPCells/"))){
	BPCells::write_matrix_dir(
		mat = mca.mtx, 
		dir = mca.bp.dir
	)}

mca.mtx <- BPCells::open_matrix_dir(mca.bp.dir)


# Load mca metadata -------------------------------------------------------

mca.md <- read.csv(
	mca.md.file, 
	row.names = 1,
	stringsAsFactors = F, 
	check.names = F
)

colnames(mca.md)
head(mca.md)


# Find H5AD cell names ----------------------------------------------------

cell.overlap <- sapply(
	mca.md, 
	function(x){
		sum(as.character(x) %in% colnames(mca.mtx))
	}
)

cell.id.col <- names(which.max(cell.overlap))

rownames(mca.md) <- as.character(mca.md[[cell.id.col]])
stopifnot(
	identical(
		rownames(mca.md), 
		colnames(mca.mtx)
	)
)

# Create seurat object ----------------------------------------------------

mca.ref <- CreateSeuratObject(
	counts = mca.mtx, 
	meta.data = mca.md, 
	project = "MCA"
)

mca.ref <- SCTransform(
	mca.ref,
	assay = "RNA",
	new.assay.name = "SCT",
	variable.features.n = 3000,
	conserve.memory = TRUE,
	verbose = TRUE
)
mca.ref <- RunPCA(
	mca.ref,
	assay = "SCT",
	npcs = 50,
	verbose = TRUE
)

qs_save(
	mca.ref, 
	file = file.path(RFunctions, "LabelTransferObjects/mca_data/mca_ref_337K.qs2"), 
	nthreads = n_threads
)

# Downsample since it's such a large object -------------------------------

cells.by.type <- split(
	colnames(mca.ref),
	mca.ref@meta.data[["tissue"]]
)
cells.keep <- unlist(
	lapply(
		cells.by.type,
		function(x){
			sample(
				x, 
				size = min(length(x), 1000)
			)
		}
	), 
	use.names = FALSE
)

length(cells.keep)
mca.ref.47K <- subset(
	mca.ref, 
	cells = cells.keep
)

mca.ref.47K[["pca"]] <- NULL
DefaultAssay(mca.ref.47K) <- "RNA"
mca.ref.47K[["SCT"]] <- NULL

mca.ref.47K <- SCTransform(
	mca.ref.47K,
	assay = "RNA", 
	new.assay.name = "SCT", 
	variable.features.n = 3000, 
	conserve.memory = T, 
	verbose = T
)

mca.ref.47K <- RunPCA(
	mca.ref.47K, 
	assay = "SCT", 
	features = VariableFeatures(mca.ref.47K[["SCT"]]), 
	npcs = 50, 
	verbose = T
)




# Save mca.ref ------------------------------------------------------------

qs_save(
	mca.ref.47K, 
	file = file.path(RFunctions, "LabelTransferObjects/mca_data/mca_ref_47K.qs2"), 
	nthreads = n_threads
)

