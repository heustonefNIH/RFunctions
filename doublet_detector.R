# Convenience script for doublet finder

doubet_dector <- function(
		seurat.object, 
		detector, 
		logger,
		sctransform = FALSE,
		pANN.reuse = NULL,
		doublet.var.thresh = 90, 
		predicted.doubletRate = 0.05)
{
	if(detector == "DoubletFinder"){
		msg <- "Running doubletfinder"
		pring(msg)
		log.msg(logger, msg = msg)
		source(file.path(sourcable.function, "runDoubletFinder.R"))
		seurat.object <- runDoubletFinder(seurat.object, 
																			sctransformed = do.sctransform, 
																			predicted.doubletRate = predicted.doubletRate
																			pANN.reuse = NULL)
		return(seurat.object)
	} else if(detector == "scrublet"){
		msg <- "Running scrublet"
		print(msg)
		log.msg(logger, msg = msg)
		source(file.path(sourcable.functions, "runScrublet.R"))
		seurat.object <- runScrublet(seurat.object)
		return(seurat.object)
	} else {
		msg <- "no doublet detector specified"
		print(msg)
		log.msg(logger, msg = msg)

	}
}