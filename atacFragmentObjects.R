GenerateATACFragementObjects <- function(frag.path){
  tryCatch(
    {
      # frag.id <- strsplit(basename(frag.path), "_")[[1]][1]
      frag.cells <- read.table(
        file = paste0(frag.path, "/outs/singlecell.csv"),
        stringsAsFactors = FALSE,
        sep = ",",
        header = TRUE,
        row.names = 1)[-1,]
      frag.cells <- frag.cells[frag.cells$passed_filters > 500,]
      tsv.path <- paste0(frag.path, "/outs/fragments.tsv.gz")
      frag.obj <- CreateFragmentObject(path = tsv.path, cells = rownames(frag.cells))
      return(frag.obj)
    }, 
    error=function(err){
      message(paste0("failed with sample ", frag.path))
      print(err)
      return(NULL)
    },
    warning=function(warn){
      message(paste0("I'm not sure how a wanring was produced with ", frag.path))
      print(warn)
      return(NULL)
    }
    
  )
}
