# Libraries
library(Seurat)
library(dplyr)

# rnaProject <- "Obesity_scRNA-Anchored-NW-OB-allmarkers-90pctvar"
rnaProject <- "schsWAT-allmarkers-95pctvar"
# Import data

xp.table <- readRDS("schsWAT-allmarkers-95pctvar.rds")
xp.table

if(dir.exists(rnaProject)){
  print("it's there")
} else {
  dir.create(paste0(rnaProject, "_subtables"))
  print("Didn't see it, so I made it")
}


xp.table <- xp.table %>%
  mutate(.by = c(avg_log2FC, p_val_adj, cluster, gene), .keep = "used") %>%
  relocate(gene, .before = everything()) %>%
  group_by(cluster)
head(xp.table)

for(i in xp.table$cluster){
  subset.table <- xp.table[xp.table$cluster == i,]
  write.table(subset.table, file = paste0(rnaProject, "_subtables/", rnaProject, "_cl", as.character(i), ".txt"), quote = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)
}



