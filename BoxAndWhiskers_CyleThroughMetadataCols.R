
#### Figure 2A
qc.list <- c("nCount_RNA", "nFeature_RNA", "percent.mt", "pANN")
for (qc in qc.list){
  p <- ggplot(
    seurat.object@meta.data, 
    aes(
      x = rpca_clusters, 
      y = !!sym(qc), 
      fill = Disease)) +
    geom_boxplot(outlier.size = .5) + 
    theme_classic() +
    labs(y = qc)
  print(p)
  ggsave(
    filename = paste0("pngs/", rnaProject, "-boxandwhiskers-", qc, ".png"),
    height = 3,
    width = 9,
    dpi = 300
  )  
}

