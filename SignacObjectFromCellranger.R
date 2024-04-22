SignacObjectFromCellranger <- function(sample.id){
  tryCatch(
    #try to do this
    {
      counts <- Read10X_h5(filename = paste0(sample.id, "/outs/filtered_peak_bc_matrix.h5"))
      chrom_assay <- CreateChromatinAssay(counts = counts, 
                                          sep = c(":", "-"),
                                          genome = genome,
                                          fragments = paste0(sample.id, "/outs/fragments.tsv.gz"), 
                                          min.cells = 10,
                                          min.features = 200
      )
      
      atac.object <- CreateSeuratObject(counts = chrom_assay,
                                        assay = "peaks", 
                                        meta.data = read.csv(file = paste0(sample.id, "/outs/singlecell.csv"), 
                                                             header = TRUE,
                                                             row.names = 1),
                                        project = samples.list[list.index]
      )
      atac.object <- AssignMetadata(metadata.df = metadata, seurat.object = atac.object)
      print(paste0("finished ", sample.id))
      return(atac.object)
    },
    #if an error occurs, print this
    error=function(err){
      message(paste0("failed with sample ", sample.id))
      print(err)
      return(NULL)
    },
    #if a warning occurs, tell me the warning
    warning=function(warn){
      message(paste0("I'm not sure how a wanring was produced with ", sample.id))
      print(warn)
      return(NULL)
    }
  )
}