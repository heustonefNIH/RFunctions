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

unregister_parallel <- function(){
	#stop doparallel cluster if you've got one
	if (requireNamespace("foreach", quietly = TRUE) &&
			foreach::getDoParWorkers() > 1 &&
			requireNamespace("doParallel", quietly = TRUE)) {
		doParallel::stopImplicitCluster()
	}
	#cluster handels are stored in .cl_env; either close it here or manually each time
	if(exists(".cl_env", inherits = FALSE) && exists("cl", envir = .cl_env)){
		try(parallel::stopCluster(.cl_env$cl), silent = TRUE)
		rm("cl", envir = .cl_env)
	}
	#now reset future and bioparallel
	if (requireNamespace("future", quietly = TRUE)){
		future::plan("sequential")
	}
	if (requireNamespace("BiocParallel", quietly = TRUE)) {
		BiocParallel::register(BiocParallel::SerialParam())
	}
	message("Parallel backends unregistered.")
}


# Parallelization calls ---------------------------------------------------

#Biowulf is linux, so you can use FORK clusters. FORK inherits the whole env so you don't have to remmeber to pass it every little thing. Problem is that FORK doesn't work on Windows. On Windows you HAVE to use SOCK.

# Usage:
#   enable_doparallel()
#   results <- foreach(i = 1:n, .combine = "c") %dopar% { ... }
#   unregister_parallel()

enable_doparallel <- function(){
	unregister_parallel()
	n <- get_workers()
	cl <- parallel::makeForkCluster(n)
	.cl_env$cl <- cl
	doParallel::registerDoParallel(cl)
	options(future.globals.maxSize = +Inf)
	message("doParallel (FORK) registered with ", n, " workers.")
	invisible(cl)
}

# future::multisession (best for Seurat computations)
# Usage:
#   enable_multisession()
#   seurat_obj <- SCTransform(seurat_obj)
#   unregister_parallel()

enable_multisession <- function(){
	unregister_parallel()
	if (requireNamespace("future", quietly = TRUE)) {
		future::plan(future::multisession, workers = get_workers())
		options(future.globals.maxSize = +Inf)
		message("future::multisession registered with ", get_workers(), " workers.")
		options(future.rng.onMisuse = "ignore")  # optional
	}
}


#biocparallel::snow (best for tradeseq)
# Usage:
#   enable_snow()
#   models <- fitGAM(counts, sds = sds)
#   unregister_parallel()

enable_snow <- function(){
	unregister_parallel()
	if (requireNamespace("BiocParallel", quietly = TRUE)) {
		p <- BiocParallel::SnowParam(
			workers = get_workers(),
			type = "SOCK",
			progressbar = TRUE
		)
		BiocParallel::register(p)
		message("BiocParallel::SnowParam registered with ", get_workers(), " workers.")
	}
}
