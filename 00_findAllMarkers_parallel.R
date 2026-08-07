# help from Claude Opus 4.8 Thinking 2026.07.23

FindAllMarkers_parallel <- function(
    object,
    test.use            = "wilcox_limma",
    only.pos            = FALSE,
    min.pct             = 0.1,
    logfc.threshold     = 0.25,
    max.cells.per.ident = Inf,
    reserve             = 1,
    verbose             = TRUE,
    ...
) {
    stopifnot(requireNamespace("foreach", quietly = TRUE),
              requireNamespace("parallel", quietly = TRUE),
              requireNamespace("doParallel", quietly = TRUE))

    idents <- levels(Seurat::Idents(object))
    if (length(idents) == 0)
        stop("No identities found. Did you set Idents(object)?")

    n <- get_workers(reserve = reserve)
    # don't spin up more workers than there are clusters to run
    n <- min(n, length(idents))

    # --- pick backend: PSOCK if interactive (RStudio-safe), FORK if batch ---
    use_fork <- !interactive() && .Platform$OS.type == "unix"
    if (use_fork) {
        cluster_handle <- parallel::makeForkCluster(n)
        if (verbose) message("FORK cluster: ", n, " workers (batch mode).")
    } else {
        cluster_handle <- parallel::makePSOCKcluster(n)
        if (verbose) message("PSOCK cluster: ", n, " workers (interactive/RStudio-safe).")
    }
    doParallel::registerDoParallel(cluster_handle)
    on.exit(parallel::stopCluster(cluster_handle), add = TRUE)  # always clean up

    if (verbose) message("Running '", test.use, "' across ", length(idents), " clusters.")

    # FORK inherits everything; PSOCK needs object + package shipped explicitly
    export_vars <- if (use_fork) character(0) else "object"

    markers <- foreach::foreach(
        clust = idents,
        .combine   = "rbind",
        .packages  = "Seurat",
        .export    = export_vars,
        .errorhandling = "pass"     # one bad cluster won't kill the whole run
    ) %dopar% {
        m <- Seurat::FindMarkers(
            object,
            ident.1             = clust,
            test.use            = test.use,
            only.pos            = only.pos,
            min.pct             = min.pct,
            logfc.threshold     = logfc.threshold,
            max.cells.per.ident = max.cells.per.ident,
            ...
        )
        if (nrow(m) == 0) return(NULL)
        m$gene    <- rownames(m)
        m$cluster <- clust
        m
    }

    # roc returns no p_val, so only order by it when it exists
    if ("p_val" %in% colnames(markers)) {
        markers <- markers[order(markers$cluster, markers$p_val), ]
    } else if ("myAUC" %in% colnames(markers)) {
        markers <- markers[order(markers$cluster, -markers$myAUC), ]
    }
    rownames(markers) <- NULL
    markers
}