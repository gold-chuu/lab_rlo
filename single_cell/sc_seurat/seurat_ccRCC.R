'
seurat guide line
author : rlo
date : 230817
'

###library loading-----------------------------------------------------------------------------

library(dplyr)
library(Seurat)
library(patchwork)
library(ggplot2)

setwd("/data/project/")

# Load the PBMC dataset
pbmc_data <- Read10X(data.dir = "./RCC_HWS/H372TDSX7/test_sample/outs/filtered_feature_bc_matrix/")

# Initialize the Seurat object with the raw (non-normalized data).
pbmc <- CreateSeuratObject(counts = pbmc_data, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc

############################ Check & Filtering by mitochondria ################################################
pbmc <- readRDS('./RCC_HWS/SS/sc/sc_analysis/rcc.all.df.rds') 
pbmc <- SetIdent(pbmc, value = pbmc$donor_id)
pbmc$pANN_0.25_0.005_11547 <- NULL
pbmc$DF.classifications_0.25_0.005_11547 <- NULL
pbmc$DF.classifications_0.25_0.005_10509 <- NULL

#remove doublets & unassigned from Vireo---------------------------------------------------------------
dt <- SetIdent(pbmc, value = dt@meta.data$donor_id)
dt <- subset(df, idents = c('doublet','unassigned'), invert=TRUE) #37529 cells -> 24551 cells

dt[["percent.mt"]] <- PercentageFeatureSet(dt, pattern = "^MT-")
dt <- PercentageFeatureSet(dt, "^RP[SL]", col.name = "percent.rb")
dt <- PercentageFeatureSet(dt, "^HB[^(P)]", col.name = "percent.hb")
dt <- PercentageFeatureSet(dt, "PECAM1|PF4", col.name = "percent.plat")

##saving QC plot
png('/data/project/RCC_HWS/rlo/qc_plot/QC_violin_plot.png',width=500,height=800)
feats <- c("nFeature_RNA", "nCount_RNA", "percent.mt", "percent.rb", "percent.hb", "percent.plat")
VlnPlot(dt, group.by = "orig.ident", features = feats, pt.size = 0.1, ncol = 3) +
  NoLegend() 

dev.off()

png('/data/project/RCC_HWS/rlo/qc_plot/QC_ncount_percent.png',width=1000)
FeatureScatter(dt, "nCount_RNA", "percent.mt", group.by = "ident", pt.size = 0.5)
dev.off()

png('/data/project/RCC_HWS/rlo/qc_plot/QC_nCount_nFeature.png', width = 1000)
FeatureScatter(dt, "nCount_RNA", "nFeature_RNA")
dev.off()

## Filtering
dt <- subset(dt, subset = percent.mt < 25) # 38491 cells -> 37902
dt <- subset(dt, subset = percent.hb < 3) # 38491 cells -> 37529

png('/data/project/RCC_HWS/rlo/qc_plot/after_QC_violin_plot.png',width=500,height=800)
feats <- c("nFeature_RNA", "nCount_RNA", "percent.mt", "percent.rb", "percent.hb", "percent.plat")
VlnPlot(dt, group.by = "orig.ident", features = feats, pt.size = 0.1, ncol = 3) +
  NoLegend()
dev.off()

png('/data/project/RCC_HWS/rlo/qc_plot/after_QC_nCount_percent.png',width=1000)
FeatureScatter(dt, "nCount_RNA", "percent.mt", group.by = "ident", pt.size = 0.5)
dev.off()

png('/data/project/RCC_HWS/rlo/qc_plot/after_QC_nCount_nFeature.png',width=1000)
FeatureScatter(dt, "nCount_RNA", "nFeature_RNA", group.by = "ident", pt.size = 0.5)
dev.off()

backup <- dt
saveRDS(backup, '/data/project/RCC_HWS/rlo/data/qc_remove_backup.rds')

##########################################Normalization& feature selection###############################################
df <- NormalizeData(dt, normalization.method = "LogNormalize", scale.factor = 10000)

df <- FindVariableFeatures(df, selection.method = "vst", nfeatures = 2000)

# Identify the 10 most highly variable genes
top10 <- head(VariableFeatures(df), 10)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(df)
plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)

png('/data/project/RCC_HWS/rlo/plot/variable_features.png',width=1000)
plot1 + plot2
dev.off()


########################################Scaling Data& PCA ########################################
all.genes <- rownames(df)
df <- ScaleData(df, features = all.genes)
df <- RunPCA(df, features = VariableFeatures(object = df))
print(df[["pca"]], dims = 1:5, nfeatures = 5)
VizDimLoadings(df, dims = 1:2, reduction='pca')
DimPlot(df, reduction = 'pca')

png('/data/project/RCC_HWS/rlo/plot/PCA_plot.png', width = 1000)
DimPlot(df,reduction = 'pca')
dev.off()

########################################Draw UMAP##################################################
df <- RunUMAP(df, dims = 1:30)
df <- FindNeighbors(df, dims = 1:30)
df <- FindClusters(df, resolution = 0.5)

png('/data/project/RCC_HWS/rlo/plot/pro1.umap.png')
DimPlot(dt, reduction = "umap")
dev.off()
png('/data/project/RCC_HWS/rlo/plot/pro1.umap.id.png')
DimPlot(dt, reduction = "umap", group.by='donor_id')
dev.off()
png('/data/project/RCC_HWS/rlo/plot/pro1.umap.stat.png')
DimPlot(dt, reduction = "umap", group.by='Status')
dev.off()
