
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
    #create gene list
    enr.genes <- enrichr_table(
      markers.df = markers.df, 
      cluster.list = clust, 
      gene.prop = gene.prop, 
      fc.threshold = fc.threshold, 
      genelist_adjPval_threshold = genelist_adjPval_threshold
    )
    enr.genes <- enr.genes %>% 
      pull(gene)
    
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
        dplyr::filter(Adjusted.P.value <= enrichr_adjPval) %>% 
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
    file_suffix = NULL
){
  
  if(!is.null(file_suffix)){
    file_suffix <- paste0("-", file_suffix)
  }
  enrichr_maxpval <- paste0("adjP", round(max(enr.df$Adjusted.P.value), 2))
  
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
      
      print(paste0(pop, "-", db_i))
      db_i.terms <- enr.df %>% 
        dplyr::filter(db == db_i, cluster %in% cluster_set) %>% 
        dplyr::slice_max(Combined.Score, n = plot_n, with_ties = TRUE) %>% 
        pull(Term)
      db_i.plot <- enr.df %>% 
        dplyr::filter(db == db_i, cluster %in% cluster_set, Term %in% db_i.terms)
      if(nrow(db_i.plot) > 0){
        if(nrow(db_i.plot) <=8){
          plot.height <- 6
        } else {
          plot.height <- 10
        }
        db_i.plot <- ggplot(db_i.plot, aes(x = factor(cluster, levels = sort(unique(cluster))), 
                                           y = Term)) +
          geom_point(aes(size = -log10(Adjusted.P.value), 
                         fill = Combined.Score), 
                     alpha = 0.75, shape = 21) +
          scale_fill_viridis_c(name = "Combined.Score") +
          scale_size_continuous(name = "-log10(Adjusted.P.value)", range = c(2, 10)) +
          xlab("Cluster ID") +
          scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
          ggtitle(label = paste0(db_i, " - ", pop), subtitle = paste("(enrichR adjP<=", enrichr_maxpval, ",", file_suffix, ")")) +
          theme_bw() +
          theme(
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent'),
            plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 8),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "gray85")
          )
        print(db_i.plot)
        
        ggsave(
          filename = file.path(image.dir, paste0(rnaProject, "-plot-", pop, "_", db_i, "-EnrichR", enrichr_maxpval, file_suffix, ".png")),
          plot = db_i.plot, width = plot.height, height = plot.height, dpi = 300, bg = "transparent"
        )
      } else {
        print(paste("--No entries for", pop, "pop in", db_i))
      }
    }
  }
}



