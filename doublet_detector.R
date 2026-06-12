
doublet_detector <- function(
		seurat.object, 
		detector, 
		logger,
		sctransform = FALSE,
		sourceable.functions = RFunctions,
		return_filtered = TRUE,
		seurat.identifier = NULL,
		pANN.reuse = NULL,
		cluster.dims = 15, 
		var.explained = 0.85,
		doublet.var.thresh = 90, 
		predicted.doubletRate = 0.05, 
		show.plots = FALSE)
{
	if(detector == "DOUBLETFINDER"){
		msg <- "Running doubletfinder"
		log.msg(logger, msg = msg)
		source(file.path(sourceable.functions, "runDoubletFinder.R"))
		seurat.object <- runDoubletFinder(
			seurat.object, 
			seurat.identifier = seurat.identifier,
			cluster.dims = cluster.dims,
			logger = logger,
			sctransformed = sctransform, 
			pANN.reuse = pANN.reuse, 
			var.explained = var.explained,
			predicted.doubletRate = predicted.doubletRate, 
			show.plots = show.plots)
		
		if(return_filtered == TRUE){
			seurat.object <- subset(
				seurat.object, 
				subset = DF.classifications == "Singlet"
			)
		}
		return(seurat.object)
	} else if(detector == "SCRUBLET"){
		msg <- "Running scrublet"
		log.msg(logger, msg = msg)
		source(file.path(sourceable.functions, "runScrublet.R"))
		seurat.object <- runScrublet(
			seurat.object
		)
		if(return_filtered == TRUE){
			seurat.object <- subset(
				seurat.object, 
				subset = is_doublet == FALSE
			)
		}
		
		return(seurat.object)
	} else {
		msg <- "no doublet detector specified"
		log.msg(logger, msg = msg)
		
	}
}
