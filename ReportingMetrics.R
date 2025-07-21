

# Reporting metrics -------------------------------------------------------

head(seurat.object@meta.data)
raw.meta <- as.data.frame(seurat.object@meta.data)

min.quantile <- 0.025
max.quantile <- 0.99

groupby.var <- c("orig.ident")
raw.meta %>% 
  group_by_at(orig.ident) %>% 
  reframe(umi.median = median(nCount_RNA),
          umi.qsmin = quantile(nCount_RNA, min.quantile), 
          umi.qsmax = quantile(nCount_RNA, max.quantile), 
          feat.median = median(nFeature_RNA),
          feat.qsmin = quantile(nFeature_RNA, min.quantile), 
          feat.qsmin = quantile(nFeature_RNA, max.quantile), 
          mt.median = median(percent.mt),
          mt.qsmin = quantile(percent.mt, min.quantile), 
          mt.qsmax = quantile(percent.mt, max.quantile), 
  )




# Investigate seurat.object --------------------------------------------------

ggplot(seurat.object@meta.data, aes(x = nFeature_RNA, fill = orig.ident)) +
  geom_histogram(binwidth = 50) +
  scale_x_continuous(breaks = seq(100, quantile(seurat.object$nFeature_RNA, max.quantile), 100), limits = c(100, quantile(seurat.object$nFeature_RNA, max.quantile))) + 
  scale_fill_manual(values = color.palette) + 
  theme_bw()


# QC for unfiltered seurat object ----------------------------------------------------------------------

seurat.object@meta.data[sapply(seurat.object@meta.data, is.character)] <- lapply(seurat.object@meta.data[sapply(seurat.object@meta.data, is.character)], as.factor)
sapply(seurat.object@meta.data, class)

factor(seurat.object$Load)

color.palette[c(factor(seurat.object$orig.ident), factor(seurat.object$Load))]
varpairs <- unique(c(seurat.object$orig.ident, seurat.object$Load))

VlnPlot(seurat.object, features = "nFeature_RNA", pt.size = 0, group.by = "orig.ident") + 
  NoLegend() + 
  geom_boxplot(
    width = 0.07,
    notch = FALSE,
    notchwidth = 0.1,
    outlier.shape = NA,
    coef = 0) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", linewidth = 1.1) +
  geom_hline(yintercept = 2500, linetype = "dashed", color = "gray40", linewidth = 1.1) + 
  scale_color_manual(colScale)


#specify color vector to get as many colors as group levels
library(RColorBrewer)
color_pallete_function <- colorRampPalette(
  colors = color.palette,
  space = "Lab")

dat<-data.frame(trt,emmean,group)

dat$group<-as.factor(dat$group)
num_colors <- nlevels(dat$group)
diamond_color_colors <- color_pallete_function(num_colors)
diamond_color_colors

seurat.object@meta.data[sapply(seurat.object@meta.data, is.character)] <- lapply(seurat.object@meta.data[sapply(seurat.object@meta.data, is.character)], as.factor)
sapply(seurat.object@meta.data, class)

#png(filename = paste0(rnaProject, "-unfiltered-nFeature-vlnQC.png"), height = 800, width = 800)
VlnPlot(seurat.object, features = "nFeature_RNA", pt.size = 0, group.by = groupby.var) + 
  NoLegend() + 
  geom_boxplot(
    width = 0.07,
    notch = FALSE,
    notchwidth = 0.1,
    outlier.shape = NA,
    coef = 0) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", linewidth = 1.1) +
  geom_hline(yintercept = 2500, linetype = "dashed", color = "gray40", linewidth = 1.1) +
  scale_fill_manual(values=c(color.palette))
#dev.off()

#png(filename = paste0(rnaProject, "-unfiltered-percentMT-vlnQC.png"), height = 800, width = 800)
VlnPlot(seurat.object, features = "percent.mt", pt.size = 0, group.by = groupby.var) + 
  NoLegend() + 
  geom_boxplot(
    width = 0.07,
    notch = FALSE,
    notchwidth = 0.1,
    outlier.shape = NA,
    coef = 0) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "gray40", linewidth = 1.1)

#dev.off()

#png(filename = paste0(rnaProject, "-unfiltered-nCount-vlnQC.png"), height = 800, width = 800)
VlnPlot(seurat.object, features = "nCount_RNA", pt.size = 0, group.by = "orig.ident") + 
  NoLegend() + 
  geom_boxplot(
    width = 0.07,
    notch = FALSE,
    notchwidth = 0.1,
    outlier.shape = NA,
    coef = 0) +
  scale_fill_manual(values=c("white", "gray", "white", "gray"))
#dev.off()


# FeatureScatter for nCount_RNA & percent.mt
scatterp1 <- ggplot(seurat.object@meta.data[sample(x = 1:nrow(x = seurat.object@meta.data)), ], 
                    aes(x = nCount_RNA, y = percent.mt, col = orig.ident)) + 
  geom_point(shape = 1, alpha = 0.4, size = 5) +
  scale_color_manual(values = color.palette) + 
  facet_wrap(~Load) + 
  guides(color = guide_legend(override.aes = list(size = 10, alpha = 1, shape = 20))) +
  ggtitle("Unfiltered: nCount vs percent.mt") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 10)) + 
  theme_bw()
#png(filename = paste0(rnaProject, "-nCountvsPctMt-QC.png"), height = 800, width = 1600)
plot(scatterp1)
#dev.off()


# FeatureScatter for nCount_RNA & nFeature_RNA
scatterp2 <- ggplot(seurat.object@meta.data[sample(x = 1:nrow(x = seurat.object@meta.data)), ], 
                    aes(x = nCount_RNA, y = nFeature_RNA, col = orig.ident)) + 
  geom_point(shape = 1, alpha = 0.4, size = 5) +
  scale_color_manual(values = color.palette) + 
  facet_wrap(~Load) + 
  guides(color = guide_legend(override.aes = list(size = 10, alpha = 1, shape = 20))) +
  ggtitle("Unfiltered: nCount vs nFeature_RNA") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 10)) + 
  theme_bw()
#png(filename = paste0(rnaProject, "-nCountvsnFeature-QC.png"), height = 800, width = 1600)
plot(scatterp2)
#dev.off()


scatterp3 <- FeatureScatter(seurat.object, feature1 = "nCount_RNA", feature2 = "percent.mt", split.by = "orig.ident", ncol = 2) + 
  geom_hex(bins = 100) + 
  NoLegend()
#png(filename = paste0(rnaProject, "-nCountvsnPctMt-scatter-QC.png"), height = 800, width = 1600)
plot(scatterp3)
#dev.off()

Idents(seurat.object) <- "orig.ident"

#png(filename = paste0(rnaProject, "-nCountvsnFeature-QC.png"), height = 800, width = 1600)
FeatureScatter(seurat.object, feature1 = "nCount_RNA", feature2 = "nFeature_RNA", shuffle = TRUE, cols = color.palette, jitter = TRUE, pt.size = 1.5)
#dev.off()

scatterp4 <- FeatureScatter(seurat.object, feature1 = "nCount_RNA", feature2 = "nFeature_RNA", split.by = "orig.ident", ncol = 2) + 
  geom_hex(bins = 100) + 
  NoLegend()
#png(filename = paste0(rnaProject, "-nCountvsnnFeature-scatter-QC.png"), height = 800, width = 1600)
plot(scatterp4)
#dev.off()

scatterp4 <- FeatureScatter(seurat.object, feature1 = "nCount_RNA", feature2 = "nFeature_RNA", 
                            group.by = "orig.ident", pt.size = 4, cols = color.palette) + 
  geom_hex(bins = 80) 
#png(filename = paste0(rnaProject, "-nCountvsnnFeature-scatter-QC-unfiltered.png"), height = 800, width = 800)
plot(scatterp4)
#dev.off()

orig.ident.levels <- c("EH007", "EH008", "EH010", "EH009")
seurat.object$orig.ident <- factor(x = seurat.object$orig.ident, levels = orig.ident.levels)
for(i in levels(seurat.object$orig.ident)){
  scatterp4 <- FeatureScatter(subset(seurat.object, subset = orig.ident == i), feature1 = "nCount_RNA", feature2 = "nFeature_RNA", split.by = "orig.ident", ncol = 2) + 
    geom_hex(bins = 100) + 
    NoLegend()
  #png(filename = paste0(rnaProject, "-", i, "-nCountvsnnFeature-unfiltered-scatter-QC.png"), height = 800, width = 800)
  plot(scatterp4)
  #dev.off()
}




# plot mt frequences
nmtcounts_5 <- c()
for(sample in unique(seurat.object$orig.ident)){
  num.in.range_mt <- ncol(subset(seurat.object, 
                                 subset = percent.mt <=5 &
                                   orig.ident == sample))
  pct.in.range_mt <- num.in.range_mt/ncol(subset(seurat.object, 
                                                 subset = orig.ident == sample))
  pct.in.range_mt <- round(pct.in.range_mt, digits = 3)*100
  text.features <- paste0(sample, " ", pct.in.range_mt,"%")
  nmtcounts_5 <- c(nmtcounts_5, text.features)
}

nmtcounts_10 <- c()
for(sample in unique(seurat.object$orig.ident)){
  num.in.range_mt <- ncol(subset(seurat.object, 
                                 subset = percent.mt <=10 &
                                   orig.ident == sample))
  pct.in.range_mt <- num.in.range_mt/ncol(subset(seurat.object, 
                                                 subset = orig.ident == sample))
  pct.in.range_mt <- round(pct.in.range_mt, digits = 3)*100
  text.features <- paste0(sample, " ", pct.in.range_mt,"%")
  nmtcounts_10 <- c(nmtcounts_10, text.features)
}

ridgeplot1 <- RidgePlot(seurat.object, features = "percent.mt", group.by = "orig.ident", cols = color.palette) +
  geom_vline(xintercept = c(5, 10), linetype = "longdash") + 
  scale_x_continuous(limits = c(-3, 25)) + 
  ggtitle("Unfiltered: percent.mt distribution") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 14)) + 
  annotate(geom = "text", x = 1, y = seq(from = 10, by = -0.2, length.out = length(unique(seurat.object$orig.ident))), label = nmtcounts_5, vjust = 1.5) + 
  annotate(geom = "text", x = 7, y = seq(from = 10, by = -0.2, length.out = length(unique(seurat.object$orig.ident))), label = nmtcounts_10, vjust = 1.5) 
#png(filename = paste0(rnaProject, "-PercentMT-ridgeplot-QC.png"), height = 800, width = 1600)
plot(ridgeplot1)
#dev.off()


ridgeplot2 <- RidgePlot(subset(seurat.object, subset = Load == "Nuclei"), features = "percent.mt", group.by = "orig.ident", cols = color.palette) +
  geom_vline(xintercept = c(5, 10), linetype = "longdash") + 
  scale_x_continuous(limits = c(-3, 25)) + 
  ggtitle("Unfiltered: percent.mt distribution - Nuclei") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 14)) + 
  annotate(geom = "text", x = 1, y = seq(from = 7, by = -0.2, length.out = 4), label = nmtcounts_5[c(2,4,5,7)], vjust = 1.5) + 
  annotate(geom = "text", x = 7, y = seq(from = 7, by = -0.2, length.out = 4), label = nmtcounts_10[c(2,4,5,7)], vjust = 1.5)
#png(filename = paste0(rnaProject, "-PercentMTnuclei-ridgeplot-QC.png"), height = 800, width = 1600)
plot(ridgeplot2)
#dev.off()

ridgeplot3 <- RidgePlot(subset(seurat.object, subset = Load == "WholeCells"), features = "percent.mt", group.by = "orig.ident", cols = color.palette) +
  geom_vline(xintercept = c(5, 10), linetype = "longdash") + 
  scale_x_continuous(limits = c(-3, 25)) + 
  ggtitle("Unfiltered: percent.mt distribution - WholeCells") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 14)) + 
  annotate(geom = "text", x = 1, y = seq(from = 7, by = -0.2, length.out = 3), label = nmtcounts_5[c(1,3,6)], vjust = 1.5) + 
  annotate(geom = "text", x = 7, y = seq(from = 7, by = -0.2, length.out = 3), label = nmtcounts_10[c(1,3,6)], vjust = 1.5)
#png(filename = paste0(rnaProject, "-PercentMTwholecells-ridgeplot-QC.png"), height = 800, width = 1600)
plot(ridgeplot3)
#dev.off()


# plot nfeature frequencies
nfeaturecounts <- c()
for(sample in unique(seurat.object$orig.ident)){
  num.in.range_feature <- ncol(subset(seurat.object, 
                                      subset = nFeature_RNA >= 200 &
                                        nFeature_RNA <= 2500 &
                                        orig.ident == sample))
  pct.in.range_feature <- num.in.range_feature/ncol(subset(seurat.object, 
                                                           subset = orig.ident == sample))
  pct.in.range_feature <- round(pct.in.range_feature, digits = 3)*100
  text.features <- paste0(sample, " ", pct.in.range_feature,"%")
  nfeaturecounts <- c(nfeaturecounts, text.features)
}

ridgeplot4 <- RidgePlot(seurat.object, features = "nFeature_RNA", group.by = "orig.ident", cols = color.palette) +
  geom_vline(xintercept = c(200, 2500), linetype = "longdash") + 
  scale_x_continuous(limits = c(-300, 5000)) + 
  ggtitle("Unfiltered: nFeature distribution") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 14)) + 
  annotate(geom = "text", x = 3500, y = seq(from = 7.8, by = -0.2, length.out = length(nfeaturecounts)), label = nfeaturecounts, vjust = 1.5)
#png(filename = paste0(rnaProject, "-nFeatureRNA-ridgeplot-QC.png"), height = 800, width = 1600)
plot(ridgeplot4)
#dev.off()

