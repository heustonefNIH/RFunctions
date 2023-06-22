SignacObjectFromCellranger <- function(samples.list, list.index){
  tryCatch(
    #try to do this
    {
      counts <- Read10X_h5(filename = paste0(names(samples.list)[list.index], "/outs/filtered_peak_bc_matrix.h5"))
      chrom_assay <- CreateChromatinAssay(counts = counts, 
                                          sep = c(":", "-"),
                                          genome = genome,
                                          fragments = paste0(names(samples.list)[list.index], "/outs/fragments.tsv.gz"), 
                                          min.cells = 10,
                                          min.features = 200
      )
      
      atac.object <- CreateSeuratObject(counts = chrom_assay,
                                        assay = "peaks", 
                                        meta.data = read.csv(file = paste0(names(samples.list)[list.index], "/outs/singlecell.csv"), 
                                                             header = TRUE,
                                                             row.names = 1),
                                        project = samples.list[list.index]
      )
      atac.object <- AssignMetadata(metadata.df = metadata, seurat.object = atac.object)
      print(paste0("finished ", names(samples.list)[list.index]))
      return(atac.object)
    },
    #if an error occurs, print this
    error=function(err){
      message(paste0("failed with sample ", names(samples.list)[list.index]))
      print(err)
      return(NULL)
    },
    #if a warning occurs, tell me the warning
    warning=function(warn){
      message(paste0("I'm not sure how a wanring was produced with ", names(samples.list)[list.index]))
      print(warn)
      return(NULL)
    }
  )
}