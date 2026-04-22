# Convenience script for doublet finder

doublet_detector <- function(
		seurat.object, 
		detector, 
		logger,
		sctransform = FALSE,
		sourceable.functions = "RFunctions",
		return_filtered = TRUE,
		pANN.reuse = NULL,
		doublet.var.thresh = 90, 
		predicted.doubletRate = 0.05)
{
	if(toupper(detector) == "DOUBLETFINDER"){
		msg <- "Running doubletfinder"
		pring(msg)
		log.msg(logger, msg = msg)
		source(file.path(sourceable.functions, "runDoubletFinder.R"))
		seurat.object <- runDoubletFinder(seurat.object, 
																			sctransformed = do.sctransform, 
																			predicted.doubletRate = predicted.doubletRate,
																			pANN.reuse = NULL)
		if(return_filtered == TRUE){
			seurat.object <- subset(
				seurat.object, 
				subset = DF.classifications == "Singlet"
			)
		}
		return(seurat.object)
	} else if(toupper(detector) == "SCRUBLET"){
		msg <- "Running scrublet"
		print(msg)
		log.msg(logger, msg = msg)
		source(file.path(sourceable.functions, "runScrublet.R"))
		seurat.object <- runScrublet(seurat.object)
		if(return_filtered == TRUE){
			seurat.object <- subset(
				seurat.object, 
				subset = is_doublet == FALSE
			)
		}
			return(seurat.object)
		} else {
			msg <- "no doublet detector specified"
			print(msg)
			log.msg(logger, msg = msg)
			
		}
}
