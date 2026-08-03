# Get parameters ----------------------------------------------------------

get_workers <- function(reserve = 1) {
	ncpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
	nworkers <- as.integer(max(1, ncpus - reserve))
	message("Workers available: ", nworkers, " (", ncpus, " CPUs - ", reserve, " reserved)")
	return(nworkers)
}

# Set global caps ---------------------------------------------------------

tryCatch({ 
	RhpcBLASctl::blas_set_num_threads(1)
	RhpcBLASctl::omp_set_num_threads(1) 
	options(future.globals.maxSize = +Inf)
}, error = function(e) {
	message("Error in setting global caps:", e$message)
}
)
tryCatch({
	Sys.setenv(
		OPENBLAS_NUM_THREADS="1", 
		OMP_NUM_THREADS="1", 
		MKL_NUM_THREADS="1", 
		NUMEXPR_NUM_THREADS="1"
	)}, error = function(e){
		message("Error in setting global caps:", e$message)
	}
)

# Parallelization cleanup ---------------------------------------------------------

# Internal env to hold the cluster handle so unregister_parallel() can find it
.cl_env <- new.env(parent = emptyenv())

unregister_parallel <- function(verbose = TRUE){
	#stop doparallel cluster if you've got one
	if (requireNamespace("foreach", quietly = TRUE) &&
			foreach::getDoParRegistered() &&
			foreach::getDoParWorkers() > 1 &&
			requireNamespace("doParallel", quietly = TRUE)) {
		try(doParallel::stopImplicitCluster(), silent = TRUE)
	}
	#cluster handles are stored in .cl_env; either close it here or manually each time
	if(exists("cl", envir = .cl_env, inherits = FALSE) && exists("cl", envir = .cl_env)){
		try(parallel::stopCluster(.cl_env$cl), silent = TRUE)
		rm("cl", envir = .cl_env)
	}
	#now reset future and bioparallel and set everything back to to serial
	if (requireNamespace("future", quietly = TRUE)){
		future::plan("sequential")
	}
	if (requireNamespace("BiocParallel", quietly = TRUE)) {
		BiocParallel::register(BiocParallel::SerialParam())
	}
	message("Parallel backends unregistered.")
	invisible(NULL)
}


# Parallelization calls ---------------------------------------------------

#Biowulf is linux, so you can use FORK clusters. FORK inherits the whole env so you don't have to remmeber to pass it every little thing. Problem is that FORK doesn't work on Windows and is unstable in RStudio. On Windows and RStudio, you have to do you HAVE to use SOCK.

# Usage:
#   enable_doparallel()
#   results <- foreach(i = 1:n, .combine = "c") %dopar% { ... }
#   unregister_parallel()

enable_doparallel <- function(verbose = TRUE){
	unregister_parallel()
	n <- get_workers()
	#Don't use forking if in RSTUDIO or WINDOWS
	use_fork <- !interactive() && .Platform$OS.type == "unix"
	if(use_fork){
		cluster_handle <- parallel::makeForkCluster(n)
		if (verbose) message("FORK cluster: ", n, " workers (batch mode).")
	}
	else {
		cluster_handle <- parallel::makePSOCKcluster(n)
		if (verbose) message("PSOCK cluster: ", n, " workers (interactive/RStudio-safe).")
	}
	.cl_env$cl <- cluster_handle
	doParallel::registerDoParallel(cluster_handle)
	options(future.globals.maxSize = +Inf)
	if (verbose) message("doParallel backend registered with ", n, " workers.")
	invisible(cluster_handle)
}

# future::multisession (best for Seurat computations)
# Usage:
#   enable_multisession()
#   seurat_obj <- SCTransform(seurat_obj)
#   unregister_parallel()

enable_multisession <- function(verbose = TRUE){
	unregister_parallel(verbose = FALSE)
	if (!requireNamespace("future", quietly = TRUE)) {
		warning("Package 'future' not available; staying serial.")
		return(invisible(NULL))
	}
	n <- get_workers()
	future::plan(future::multisession, workers = n)
	options(future.globals.maxSize = +Inf)
	options(future.rng.onMisuse = "ignore")  # optional
	if (verbose) message("future::multisession registered with ", n, " workers")
}


#biocparallel::snow (best for tradeseq)
# Usage:
#   enable_snow()
#   models <- fitGAM(counts, sds = sds)
#   unregister_parallel()

enable_snow <- function(verbose = TRUE){
	unregister_parallel(verbose = FALSE)
	if (!requireNamespace("BiocParallel", quietly = TRUE)) {
		warning("Package 'BiocParallel not available; staying serial.")
		return(invisible(NULL))
	}
	n <- get_workers()
		p <- BiocParallel::SnowParam(
			workers = n,
			type = "SOCK",
			progressbar = TRUE
		)
		BiocParallel::register(p)
		if (verbose) message("BiocParallel::SnowParam registered with ", get_workers(), " workers.")
		invisible(NULL)
}
