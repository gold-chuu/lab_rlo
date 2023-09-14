####################################################################################
################################Singlecell pathway analysis#########################
####################################################################################
#library loading -----------------------------------------


library(Seurat) 
library(devtools) 

library(msigdbr) 
library(fgsea) 
library(tidyr) 
library(dplyr) 
library(ggplot2) 
library(tibble) 
library(tidyverse) 
library(data.table)
library(AUCell)

setwd("~/lab_rlo/single_cell")

#relaoding-----------------------------------------------------------------------------------------
seurat_obj <- readRDS('/data/project/RCC_HWS/SS/sc/seurat/pro10.rds')

DefaultAssay(seurat_obj) <- "RNA"
seurat_obj <- SetIdent(seurat_obj, value = seurat_obj$seurat_clusters)

markers <- FindAllMarkers(seurat_obj, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
tbl <- markers %>%
    group_by(cluster) %>%
    slice_max(n = 50, order_by = avg_log2FC)
write.csv(tbl,'./GSEA/pro10.2.top20_markers.csv')
tbl <- markers %>% arrange(desc(avg_log2FC))
write.csv(tbl, file = "./GSEA/all_markers.csv")

fold_changes <- tbl$avg_log2FC
names(fold_changes) <- tbl$gene
 
head(fold_changes)

fgsea_sets <- tbl
######################################################################################################33
# Load GSEA gene sets
myData.genes %>%
  dplyr::filter(group == "CD14+ Mono_Control") %>%
  arrange(desc(logFC), desc(auc)) %>%
  head(n = 10)

#그리고 뽑은 정보들을 auc에 따라 1등부터 꼴등까지 줄을 세워봅니다.
CD14Mono.genes<- myData.genes %>%
  dplyr::filter(group == "CD14+ Mono_Control") %>%
  arrange(desc(auc)) %>% 
  dplyr::select(feature, auc)

fgseaRes <- fgsea(fgsea_sets, stats = fold_changes, nperm = 1000)

fgseaResTidy <- fgseaRes %>%
  as_tibble() %>%
  arrange(desc(NES))

#결과를 출력하기 좋게 정리합니다
fgseaResTidy %>% 
  dplyr::select(-leadingEdge, -ES, -nMoreExtreme) %>% 
  arrange(padj) %>% 
  head()


ggplot(fgseaResTidy %>% filter(padj < 0.05) %>% head(n= 50), aes(reorder(pathway, NES), NES)) +
  geom_col(aes(fill= NES<0)) +
  coord_flip() +
  labs(x="Pathway", y="Normalized Enrichment Score",
       title="Hallmark pathways NES from GSEA") + 
  theme_minimal()