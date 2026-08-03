# Logging -----------------------------------------------------------------

log.msg <- function(
		log.file, 
		msg, 
		to.console = TRUE,
		overwrite = FALSE,
		log.path = file.path(rna.dir, "logs")){
	if(to.console == TRUE){
		print(msg)
	}
	log.file <- file.path(log.path, log.file)
	time.stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
	log_entry <- paste0("[", time.stamp, "] ", msg, "\n")
	if(overwrite == FALSE){
		cat(log_entry, file = log.file, append = TRUE)
	} else if(overwrite == TRUE){
		cat(log_entry, file = log.file, append = FALSE)
	}
}
