function(sample.path, combined.peaks){
  tryCatch(
    #try to do this
    {
      #create peak object
      sample.id <- strsplit(sample.path, "_")[[1]][1]
      sample.peaks <- read.table(
        file = paste0(names(sample.path), "/outs/peaks.bed"),
        col.names = c("chr", "start", "end")
        )
      sample.peaks <- makeGRangesFromDataFrame(sample.peaks)
      return(atac.object)
    },
    #if an error occurs, print this
    error=function(err){
      message(paste0("failed with sample ", names(frag.path)))
      print(err)
      return(NULL)
    },
    #if a warning occurs, tell me the warning
    warning=function(warn){
      message(paste0("I'm not sure how a wanring was produced with ", names(frag.path)))
      print(warn)
      return(NULL)
    }
  )
}
