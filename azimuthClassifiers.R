# https://azimuth.hubmapconsortium.org/references/#Human%20-%20Pancreas

az <- list()
az$cycling.endocrine <- 
c("UBE2C", "TOP2A", "CDK1", "BIRC5", "PBK", "CDKN3", "MKI67", "CDC20", "CCNB2", "CDCA3")
az$immune.leukocyte <- 
c("ACP5", "APOE", "HLA-DRA", "TYROBP", "LAPTM5", "SDS", "FCER1G", "C1QC", "C1QB", "SRGN")
az$quiescent.stellate <- 
c("RGS5", "C11orf96", "FABP4", "CSRP2", "IL24", "ADIRF", "NDUFA4L2", "GPX3", "IGFBP4", "ESAM")
az$endothelial <- 
c("PLVAP", "RGCC", "ENG", "PECAM1", "ESM1", "SERPINE1", "CLDN5", "STC1", "MMP1", "GNG11")
az$schwann <- 
c("NGFR", "CDH19", "UCN2", "SOX10", "S100A1", "PLP1", "TSPAN11", "WNT16", "SOX2", "TFAP2A")
az$activated.stellate <- 
c("COL1A1", "COL1A2", "COL6A3", "COL3A1", "TIMP3", "TIMP1", "CTHRC1", "SFRP2", "BGN", "LUM")
az$epsilon <- 
c("BHMT", "VSTM2L", "PHGR1", "TM4SF5", "ANXA13", "ASGR1", "DEFB1", "GHRL", "COL22A1", "OLFML3")
az$pp <- 
c("PPY", "AQP3", "MEIS2", "ID2", "GPC5-AS1", "CARTPT", "PRSS23", "ETV1", "PPY2", "TUBB2A")
az$delta <- 
c("SST", "RBP4", "SERPINA1", "RGS2", "PCSK1", "SEC11C", "HHEX", "LEPR", "MDK", "LY6H")
az$ductal <- 
c("SPP1", "MMP7", "IGFBP7", "KRT7", "ANXA4", "SERPINA1", "LCN2", "CFTR", "KRT19", "SERPING1")
az$acinar <- 
c("REG1A", "PRSS1", "CTRB2", "CTRB1", "REG1B", "CELA3A", "PRSS2", "REG3A", "CPA1", "CLPS")
az$beta <- 
c("IAPP", "INS", "DLK1", "INS-IGF2", "G6PC2", "HADH", "ADCYAP1", "GSN", "NPTX2", "C12orf75")
az$alpha <- 
c("GCG", "TTR", "PPP1R1A", "CRYBA2", "TM4SF4", "MAFB", "GC", "GPX3", "PCSK2", "PEMT")
az$all <- c(
  "UBE2C", "TOP2A", "CDK1", "BIRC5", "PBK", "CDKN3", "MKI67", "CDC20", "CCNB2", "CDCA3",
  "ACP5", "APOE", "HLA-DRA", "TYROBP", "LAPTM5", "SDS", "FCER1G", "C1QC", "C1QB", "SRGN",
  "RGS5", "C11orf96", "FABP4", "CSRP2", "IL24", "ADIRF", "NDUFA4L2", "GPX3", "IGFBP4", "ESAM",
  "PLVAP", "RGCC", "ENG", "PECAM1", "ESM1", "SERPINE1", "CLDN5", "STC1", "MMP1", "GNG11",
  "NGFR", "CDH19", "UCN2", "SOX10", "S100A1", "PLP1", "TSPAN11", "WNT16", "SOX2", "TFAP2A",
  "COL1A1", "COL1A2", "COL6A3", "COL3A1", "TIMP3", "TIMP1", "CTHRC1", "SFRP2", "BGN", "LUM",
  "BHMT", "VSTM2L", "PHGR1", "TM4SF5", "ANXA13", "ASGR1", "DEFB1", "GHRL", "COL22A1", "OLFML3",
  "PPY", "AQP3", "MEIS2", "ID2", "GPC5-AS1", "CARTPT", "PRSS23", "ETV1", "PPY2", "TUBB2A",
  "SST", "RBP4", "RGS2", "PCSK1", "SEC11C", "HHEX", "LEPR", "MDK", "LY6H",
  "SPP1", "MMP7", "IGFBP7", "KRT7", "ANXA4", "LCN2", "CFTR", "KRT19", "SERPING1",
  "REG1A", "PRSS1", "CTRB2", "CTRB1", "REG1B", "CELA3A", "PRSS2", "REG3A", "CPA1", "CLPS",
  "IAPP", "INS", "DLK1", "INS-IGF2", "G6PC2", "HADH", "ADCYAP1", "GSN", "NPTX2", "C12orf75",
  "GCG", "TTR", "PPP1R1A", "CRYBA2", "TM4SF4", "MAFB", "GC", "PCSK2", "PEMT")
  