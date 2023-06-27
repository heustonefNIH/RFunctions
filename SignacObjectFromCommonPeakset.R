SignacObjectFromCommonPeakset <- function(sample.path, common.peakset = NULL){
  tryCatch(
    {
      sample.id <- basename(sample.path)
      frag.list <- GenerateATACFragementObjects(sample.path)
      atac.counts <- FeatureMatrix(
        fragments = frag.list[[1]],
        features = combined.peaks,
        cells = rownames(frag.list[[2]])
      )
      
      atac.assay <- CreateChromatinAssay(atac.counts, fragments = frag.obj)
      atac.obj <- CreateSeuratObject(atac.assay, assay = "ATAC", meta.data = frag.list[[2]], project = sample.id)
      
      return(atac.obj)
    }, 
    error=function(err){
      message(paste0("failed with sample ", sample.path))
      print(err)
      return(NULL)
    },
    warning=function(warn){
      message(paste0("I'm not sure how a wanring was produced with ", sample.path))
      print(warn)
      return(NULL)
    }
    
  )
}
