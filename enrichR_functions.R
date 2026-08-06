# EnrichR Table -----------------------------------------------------------


enrichr_table <- function(
    markers.df, 
    cluster.list, 
    gene.prop = 1, #proportion of hits to return
    fc.threshold, 
    genelist_adjPval_threshold = 0.05
){
  if(fc.threshold >0){
    xprsn.type <- "pos"
  } else if (fc.threshold < 0){
    xprsn.type <- "neg"
  } else {
    xprsn.type <- ""
  }
  enr.markers <- markers.df %>% 
    dplyr::filter(cluster %in% cluster.list & abs(avg_log2FC) >= fc.threshold & p_val_adj <= genelist_adjPval_threshold) %>% 
    select(avg_log2FC, cluster, gene) %>% 
    group_by(cluster) %>% 
    dplyr::slice_max(avg_log2FC, prop = gene.prop)
  return(enr.markers)
}


# EnrichR Run -------------------------------------------------------------

enrichr_run <- function(
    markers.df, 
    cluster.list, 
    db.list, 
    rnaProject, 
    enrichr_adjPval_threshold = 0.05, 
    n_max = Inf,
    enrichr_dir = "", 
    fc.threshold = 1.5, 
    is.findMarkers.df = TRUE,
    id_columns = NULL, # if not findMarkers.df, provide c(gene_col_name, clust_col_name)
    genelist_adjPval_threshold = 0.05,
    gene.prop = 1, #proportion of gene hits to return
    file_suffix = NULL, 
    n_threads = 1
){
  
  if(!is.null(file_suffix)){
    file_suffix <- paste0("-", file_suffix)
  }
  
  adjPval.character <- paste0("_adjP", enrichr_adjPval_threshold)
  
  for(clust in cluster.list){
    if(isTRUE(is.findMarkers.df)){
      #create gene list
      enr.genes <- enrichr_table(
        markers.df = markers.df, 
        cluster.list = clust, 
        gene.prop = gene.prop, 
        fc.threshold = fc.threshold, 
        genelist_adjPval_threshold = genelist_adjPval_threshold
      )
      enr.genes <- enr.genes %>% 
        filter(cluster == clust) %>% 
        pull(gene)
    }  else if (!isTRUE(is.findMarkers.df)) {
      if(length(id_columns) != 2){
        stop("Must provide gene and cluster column ids")
      }
      enr.genes <- markers.df %>% 
        filter(.data[[id_columns[2]]] == clust) %>% 
        pull(.data[[id_columns[1]]])
    }
    
    #slice to max number of genes to report
    if(is.finite(n_max)){
      enr.genes <- enr.genes[seq_len(n_max)]
    }
    
    message("starting enrichR::enrichr")
    
    #run enrichr
    enr.list <- enrichr(enr.genes, db.list)
    
    #build table of enrichR results
    for(i in names(enr.list)){
      partial.df <- enr.list[[i]] %>% 
        dplyr::filter(Adjusted.P.value <= enrichr_adjPval_threshold) %>% 
        mutate(db = i, cluster = clust)
      
      #create or append to growing table
      if(i == names(enr.list)[1]){
        clutser.df <- partial.df
      } else{
        clutser.df <- rbind(clutser.df, partial.df)
      }
    }
    if(clust == cluster.list[1]){
      enr.df <- clutser.df
    } else {
      enr.df <- rbind(enr.df, clutser.df)
    }
    message(paste("finished", clust))
  }
  
  file.name <- paste0(rnaProject, "-EnrichR", adjPval.character, file_suffix)
  message(paste0("saving ", file.name, ".qs2"))
  qs_save(
    enr.df, 
    file = file.path(enrichr_dir, paste0(file.name, ".qs2")), 
    nthreads = n_threads
  )
  write.table(
    enr.df, 
    file = file.path(enrichr_dir, paste0(file.name, ".txt")), 
    col.names = T,
    row.names = F,
    sep = "\t", 
    quote = F
  )
  message("completed enrichR run")
  return(enr.df)
}


# EnrichR Graph -----------------------------------------------------------

enrichr_graph <- function(
    celltype.list, 
    enr.df, 
    plot_n = 5,     
    sort.by = "db", # one of "db" [default], "celltype", "None"
    enrichr_dir = "", 
    axis_label = "Cluster",
    file_suffix = NULL,
    
    # Plot appearance
    label_wrap_width = 42,
    base_font_size = 11,
    y_font_size = 9,
    x_font_size = 10,
    
    # Dynamic size controls, in inches
    min_plot_width = 8,
    max_plot_width = 18,
    min_plot_height = 5,
    max_plot_height = 30,
    row_height = 0.30,
    wrapped_line_height = 0.18
    
){
  # File naming -------------------------------------------------------------
  
  if(!is.null(file_suffix) && nzchar(file_suffix)){
    file_suffix <- paste0("-", file_suffix)
  }
  
  max_adjusted_p <- max(enr.df$Adjusted.P.value, na.rm = TRUE)
  enrichr_maxpval <- paste0("adjP", round(max_adjusted_p, 2))
  
  
  # Folder creation ---------------------------------------------------------
  
  if(sort.by == "db"){
    message(paste("creating db folders"))
    for(db_i in unique(enr.df$db)){
      image.dir <- file.path(file.path(rna.dir, enrichr_dir, db_i))
      if (!dir.exists(image.dir)) {
        dir.create(image.dir, recursive = TRUE)}
    }
  } else if(sort.by == "celltype"){
    message(paste("creating celltype folders"))
    for(pop in names(celltype.list)){
      image.dir <- file.path(file.path(rna.dir, enrichr_dir, pop))
      if (!dir.exists(image.dir)) {
        dir.create(image.dir, recursive = TRUE)}
    }
  } else {
    image.dir <- file.path(file.path(rna.dir, enrichr_dir))
    message(paste("saving to", image.dir))
  }
  
  
  if(is.null(names(celltype.list))){
    stop("Must have a named cluster list")
  }
  
  
  # Create plot dataframes --------------------------------------------------
  
  for(pop in names(celltype.list)){
    
    cluster_set <- celltype.list[[pop]]
    
    for(db_i in unique(enr.df$db)){
      
      if(sort.by == "db"){
        image.dir <- file.path(file.path(rna.dir, enrichr_dir, db_i))
      } else if(sort.by == "celltype"){
        image.dir <- file.path(file.path(rna.dir, enrichr_dir, pop))
      } else {
        image.dir <- file.path(file.path(rna.dir, enrichr_dir))
      }
      
      message(pop, "-", db_i)
      
      db_i.terms <- enr.df %>% 
        group_by(cluster) %>% 
        dplyr::filter(db == db_i, cluster %in% cluster_set) %>% 
        dplyr::slice_max(Combined.Score, n = plot_n, with_ties = TRUE) %>% 
        pull(Term)
      db_i.plot <- enr.df %>% 
        dplyr::filter(db == db_i, cluster %in% cluster_set, Term %in% db_i.terms)
      
      if(nrow(db_i.plot) == 0) {
        message("--No entries for", pop, "pop in", db_i)
        next
      }
      
      # Order plot dataframe ----------------------------------------------------------
      
      #remove duplicates
      db_i.plot <- db_i.plot %>% 
        dplyr::distinct(cluster, Term, .keep_all = TRUE)
      
      #preserve cluster order
      cluster_levels <- sort(unique(db_i.plot$cluster))
      
      #preserve existing term order
      term_levels <- rev(unique(db_i.plot$Term))
      
      #set plot df order
      db_i.plot <- db_i.plot %>% 
        dplyr::mutate(
          cluster = factor(cluster, levels = cluster_levels),
          Term = factor(Term, levels = term_levels)
        )
      
      # Calculate plot dimensions -----------------------------------------------
      
      plot_dimensions <- calculate_plot_dimensions(
        terms = levels(db_i.plot$Term), 
        clusters = db_i.plot$cluster, 
        wrap_width = label_wrap_width, 
        min_width = min_plot_width,
        max_width = max_plot_width,
        min_height = min_plot_height,
        max_height = max_plot_height,
        row_height = row_height,
        wrapped_line_height = wrapped_line_height
      )
      
      # Start plotting ----------------------------------------------------------
      
      subtitle_txt = paste0("(enrichR adjP<= ", round(max_adjusted_p, 2), " ", file_suffix, ")")
      
      p <- ggplot(
        db_i.plot, 
        aes(
          x = cluster, 
          y = Term
        )
      ) +
        geom_point(
          aes(
            size = -log10(pmax(Adjusted.P.value, .Machine$double.xmin)), #protects against pval = 0 
            fill = Combined.Score
          ), 
          alpha = 0.75, 
          shape = 21
        ) +
        scale_fill_viridis_c(
          name = "Combined.Score"
        ) +
        scale_size_continuous(
          name = expression(-log[10]("adjPval")), 
          range = c(2, 10)
        ) +
        scale_y_discrete(
          labels = function(x) {
            stringr::str_wrap(x, width = 40)
          }, 
          expand = expansion(add = c(0.5, 0.5))
        ) +
        scale_x_discrete(
          expand = expansion(add = 0.5)
        ) +
        labs(
          x = axis_label, 
          y = NULL, 
          title = paste0(db_i, " - ", pop), 
          subtitle = subtitle_txt
        ) +
        guides(
          fill = guide_colorbar(
            order = 1, 
            barheight = grid::unit(30, "mm")
          ), 
          size = guide_legend(
            order = 2, 
            override.aes = list(alpha = 0.8)
          )
        ) +
        theme_bw(base_size = base_font_size) +
        theme(
          panel.background = element_rect(
            fill='transparent', 
            color = NA
          ),
          plot.background = element_rect(
            fill='transparent'
            , color=NA
          ),
          legend.background = element_rect(
            fill='transparent', 
            color = NA
          ),
          legend.box.background = element_rect(
            fill='transparent', 
            color = NA
          ),
          plot.title = element_text(
            size = base_font_size + 2, 
            face = "bold", 
            margin = margin(b = 4)
          ),
          plot.subtitle = element_text(
            size = base_font_size, 
            margin = margin(b = 10)
          ),
          axis.text.y = element_text(
            size = y_font_size, 
            lineheight = 0.95, 
            margin = margin(t = 5)
          ),
          axis.text.x = element_text(
            size = x_font_size, 
            margin = margin(t = 8)
          ),
          axis.title.x = element_text(
            size = base_font_size + 1, 
            margin = margin(t = 8)
          ),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(
            color = "gray85", 
            linewidth = 0.35
          ), 
          legend.position = "right", 
          legend.title = element_text(size = base_font_size - 1), 
          legend.text = element_text(size = base_font_size - 1), 
          plot.margin = margin(
            t = 12, 
            r = 15, 
            b = 12, 
            l = 12, 
            unit = "pt"
          )
        )
      print(p)
      
      ggsave(
        filename = file.path(
          image.dir, 
          paste0(
            rnaProject, "-plot-", pop, "_", db_i, "-EnrichR", enrichr_maxpval, file_suffix, ".png"
          )
        ),
        plot = p, 
        width = plot_dimensions$width, 
        height = plot_dimensions$height, dpi = 300, bg = "transparent", 
        limitsize = FALSE
      )
    }
  }
}

calculate_plot_dimensions <- function(
    terms,
    clusters,
    wrap_width,
    min_width,
    max_width,
    min_height,
    max_height,
    row_height,
    wrapped_line_height
) {
  
  wrapped_terms <- stringr::str_wrap(terms, width = wrap_width)
  
  # Number of displayed lines in each wrapped label
  line_counts <- stringr::str_count(wrapped_terms, "\n") + 1L
  
  # Longest individual line after wrapping
  individual_lines <- unlist(strsplit(wrapped_terms, "\n", fixed = TRUE))
  longest_line <- if (length(individual_lines) > 0) {
    max(nchar(individual_lines), na.rm = TRUE)
  } else {
    0
  }
  
  n_terms <- length(terms)
  n_clusters <- length(unique(clusters))
  
  # Height includes: one unit for every term; additional space for wrapped lines; space for title, axis title, and legends
  height <- 2.7 +
    n_terms * row_height +
    sum(pmax(line_counts - 1L, 0L)) * wrapped_line_height
  
  # Width includes: plotting panel width based on number of x-axis categories; space for the longest y-axis label; space for legends
  panel_width <- max(3.5, 0.65 * n_clusters)
  label_width <- max(2.0, 0.085 * longest_line)
  legend_width <- 2.3
  
  width <- panel_width + label_width + legend_width
  
  list(
    width = max(min_width, min(width, max_width)),
    height = max(min_height, min(height, max_height)),
    wrapped_terms = wrapped_terms
  )
}


