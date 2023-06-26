GenerateATACFragementObjects <- function(frag.path){
  tryCatch(
    {frag.id <- strsplit(i, "_")[[1]][1]
    frag.obj <- read.table(
      file = paste0(frag.path, "/outs/singlecell.csv"),
      stringsAsFactors = FALSE,
      sep = ",",
      header = TRUE,
      row.names = 1)[-1,]
    frag.obj <- frag.obj[frag.obj$passed_filters > 500,]
    frag.obj <- CreateFragmentObject(path = paste0(frag.path, "/outs/fragments.tsv.gz"),
                                     cells = frag.obj)
    
    return(frag.obj)
    } 
    error=function(err){
      message(paste0("failed with sample ", names(frag.path)))
      print(err)
      return(NULL)
    },
    warning=function(warn){
      message(paste0("I'm not sure how a wanring was produced with ", names(frag.path)))
      print(warn)
      return(NULL)
    }
    
  )
}