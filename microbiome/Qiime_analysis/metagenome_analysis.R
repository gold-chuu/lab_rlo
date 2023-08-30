library(microbiomeMarker)
library(DESeq2)
library(tidyverse)
library(vegan)
library(ANCOMBC)
library(ComplexHeatmap)
library(phyloseq)
library(reshape2)

setwd("~/lab_rlo/microbiome/Qiime2_analysis/silva/")
list.files()

otuqza_file <-"./table.qza"
taxaqza_file <- "./taxonomy.qza"
sample_file <- "./sample-metadata.tsv"
treeqza_file <- "./rooted-tree.qza"

ps <- import_qiime2(otu_qza = otuqza_file, taxa_qza = taxaqza_file,sam_tab = sample_file, tree_qza = treeqza_file)
ps

taxonomy <- as.data.frame(import_qiime2("./taxonomy.qza"))

tax <- taxonomy %>%
  select(Taxon) %>% 
  separate(Taxon, c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), "; ")

p<-plot_richness(ps, x="Condition", measures=c("Observed", "Shannon")) +
  geom_boxplot() +
  theme_classic() +
  theme(strip.background = element_blank(), axis.text.x.bottom = element_text(angle = -90))

newSTorder = c("WT_NORMAL", "WT_NASH", "KO_NORMAL", "KO_NASH")

p$data$Condition <- as.character(p$data$Condition)
p$data$Condition <- factor(p$data$Condition, levels=newSTorder)
print(p)


dist = phyloseq::distance(ps, method="bray")
ordination = ordinate(ps, method="NMDS", distance=dist)

plot_ordination(ps, ordination, color="Condition") + 
  theme_classic() +
  theme(strip.background = element_blank()) +geom_point(size=2)

plot_ordination(ps, ordination, color="Diet") + 
  theme_classic() +
  theme(strip.background = element_blank()) +geom_point(size=2)

plot_ordination(ps, ordination, color="KO") + 
  theme_classic() +
  theme(strip.background = element_blank()) +geom_point(size=2)

######## phylum level abundance graph

ps.rel = transform_sample_counts(ps, function(x) x/sum(x)*100)
# agglomerate taxa
glom <- tax_glom(ps.rel, taxrank = 'Phylum', NArm = FALSE)
ps.melt <- psmelt(glom)
# change to character for easy-adjusted level
ps.melt$Phylum <- as.character(ps.melt$Phylum)

ps.melt <- ps.melt %>%
  group_by(Condition, Phylum) %>%
  mutate(median=median(Abundance))
# select group median > 1
keep <- unique(ps.melt$Phylum[ps.melt$median > 1])
ps.melt$Phylum[!(ps.melt$Phylum %in% keep)] <- "< 1%"

#to get the same rows together

ps.melt_sum <- ps.melt %>%
  group_by(Sample,Condition,Phylum) %>%
  summarise(Abundance=sum(Abundance))

pp<-ggplot(ps.melt_sum, aes(x = Sample, y = Abundance, fill = Phylum)) + 
  geom_bar(stat = "identity", aes(fill=Phylum)) + 
  labs(x="", y="%") +
  facet_wrap(~Condition, scales= "free_x", nrow=1) +
  theme_classic() + 
  theme(strip.background = element_blank(), 
        axis.text.x.bottom = element_text(angle = -90))

pp$data$Condition <- as.character(pp$data$Condition)
pp$data$Condition <- factor(pp$data$Condition, levels=newSTorder)
print(pp)

##### Genus level abundance bargraphs

ps.rel = transform_sample_counts(ps, function(x) x/sum(x)*100)
# agglomerate taxa
glom <- tax_glom(ps.rel, taxrank = 'Genus', NArm = FALSE)
ps.melt <- psmelt(glom)
# change to character for easy-adjusted level
ps.melt$Genus <- as.character(ps.melt$Genus)

ps.melt <- ps.melt %>%
  group_by(Condition, Genus) %>%
  mutate(median=median(Abundance))
# select group mean > 1
keep <- unique(ps.melt$Genus[ps.melt$median > 2.5])
ps.melt$Genus[!(ps.melt$Genus %in% keep)] <- "< 2.5%"
#to get the same rows together
ps.melt_sum <- ps.melt %>%
  group_by(Sample,Condition,Genus) %>%
  summarise(Abundance=sum(Abundance))

ppp<-ggplot(ps.melt_sum, aes(x = Sample, y = Abundance, fill = Genus)) + 
  geom_bar(stat = "identity", aes(fill=Genus)) + 
  labs(x="", y="%") +
  facet_wrap(~Condition, scales= "free_x", nrow=1) +
  theme_classic() + 
  theme(legend.position = "right", 
        strip.background = element_blank(), 
        axis.text.x.bottom = element_text(angle = -90))

ppp$data$Condition <- as.character(ppp$data$Condition)
ppp$data$Condition <- factor(ppp$data$Condition, levels=newSTorder)
print(ppp)

############# Bar graph
## ps.melt_sum ==2.5% below cut
## ps.melt, everything

z <- ps.melt[ps.melt$Genus %in% c("Parasutterella"),]
x <-ggplot(z, aes(x = Condition, y = Abundance)) + 
  geom_boxplot() + 
  ylab("Abundance") +
  facet_wrap(~Condition, scales= "free_x", nrow=1) +
  theme_classic() + 
  theme(legend.position = "right", 
        strip.background = element_blank(), 
        axis.text.x.bottom = element_text(angle = -90))

x$data$Condition <- as.character(x$data$Condition)
x$data$Condition <- factor(x$data$Condition, levels=newSTorder)
print(x)

##한번에 모든 plot을 그리고 저장하는 함수 짜버리기.
genus_name <- ps.melt$Genus %>% unique() %>% as.list()
genus_name <- setdiff(genus_name, "< 2.5%")

for (gene in genus_name) {
  abundance_chart <- ps.melt[ps.melt$Genus %in% gene,]
  
  abundance_plot <- ggplot(abundance_chart, aes(x = Condition, y = Abundance)) + 
    geom_boxplot() + ggtitle(paste("Gene title:", gene)) +
    #                    facet_wrap(~Condition, scales= "free_x", nrow=1) + 
    theme_classic() +
    theme(title = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.x = element_blank())
  
  abundance_plot$data$Condition <- as.character(abundance_plot$data$Condition)
  abundance_plot$data$Condition <- factor(abundance_plot$data$Condition, levels = newSTorder)
  
  ggsave(filename = paste0("./abundance_plot/Abundance_plot_", gene, ".png"), plot = abundance_plot)
}


########## Differential abundance analysis

sample_data(ps)$Condition <- as.factor(sample_data(ps)$Condition) # factorize for DESeq2
ps.taxa <- tax_glom(ps, taxrank = 'Genus', NArm = FALSE)


# pairwise comparison between gut and tongue
ps.taxa.sub <- subset_samples(ps.taxa, Condition %in% c("WT_NORMAL", "WT_NASH", "KO_NORMAL", "KO_NASH"))
#ps.taxa.sub <- subset_samples(ps.taxa, Condition %in% c("KO_NORMAL", "KO_NASH"))
# filter sparse features, with > 90% zeros
ps.taxa.pse.sub <- prune_taxa(rowSums(otu_table(ps.taxa.sub) == 0) < ncol(otu_table(ps.taxa.sub)) * 0.9, ps.taxa.sub)
ps_ds = phyloseq_to_deseq2(ps.taxa.pse.sub, ~ Condition)

# use alternative estimator on a condition of "every gene contains a sample with a zero"
ds <- estimateSizeFactors(ps_ds, type="poscounts")
ds = DESeq(ds, test="Wald", fitType="parametric")
alpha = 0.05 
res = results(ds, alpha=alpha)
res = res[order(res$padj, na.last=NA), ]
taxa_sig = rownames(res[1:20, ]) # select bottom 20 with lowest p.adj values
ps.taxa.rel <- transform_sample_counts(ps, function(x) x/sum(x)*100)
ps.taxa.rel.sig <- prune_taxa(taxa_sig, ps.taxa.rel)

# Only keep gut and tongue samples
ps.taxa.rel.sig <- prune_samples(colnames(otu_table(ps.taxa.pse.sub)), ps.taxa.rel.sig)

#### make final heatmap

matrix <- as.matrix(data.frame(otu_table(ps.taxa.rel.sig)))
rownames(matrix) <- as.character(tax_table(ps.taxa.rel.sig)[, "Genus"])
metadata_sub <- data.frame(sample_data(ps.taxa.rel.sig))

# Define the annotation color for columns and rows
annotation_col = data.frame(
  KO = as.factor(metadata_sub$KO), 
  `Condition` = as.factor(metadata_sub$Condition), 
  check.names = FALSE
)
rownames(annotation_col) = rownames(metadata_sub)

annotation_row = data.frame(
  Phylum = as.factor(tax_table(ps.taxa.rel.sig)[, "Phylum"])
)
rownames(annotation_row) = rownames(matrix)

# ann_color should be named vectors
phylum_col = RColorBrewer::brewer.pal(length(levels(annotation_row$Phylum)), "Paired")
names(phylum_col) = levels(annotation_row$Phylum)
ann_colors = list(
  KO = c(`Yes` = "red", `No` = "blue"),
  `Condition` = c(WT_NORMAL = "purple", WT_NASH = "yellow",KO_NORMAL="green", KO_NASH="orange"),
  Phylum = phylum_col
)

# force column order manually
anno_col_force = annotation_col[c(18,1,3,17,12,2,5,8,13,15,14,7,6,10,19,4,16,11,9),]


matrix_new_order = matrix[,rownames(anno_col_force)]

ComplexHeatmap::pheatmap(matrix_new_order, scale= "row", 
                         annotation_col = anno_col_force, 
                         annotation_row = annotation_row, 
                         annotation_colors = ann_colors, cluster_cols = F) ## order by input

##Sorting
anno_Col_for = annotation_col[c(19,17,15,16,12,13,3,6,9,11,18,7,4,14,8,5,2,1),]
matrix_new_ord = matrix[,rownames(anno_Col_for)]

ComplexHeatmap::pheatmap(matrix_new_ord, scale= "row", 
                         annotation_col = anno_Col_for, 
                         annotation_row = annotation_row, 
                         annotation_colors = ann_colors, cluster_cols = F)
##

### ANCOMBC....fix tmrw....

ps.taxa.sub <- subset_samples(ps.taxa, Condition %in% c("WT_NORMAL", "WT_NASH", "KO_NORMAL", "KO_NASH"))

# ancombc
out <- ancombc(phyloseq = ps.taxa.sub, formula = "Condition", 
               p_adj_method = "holm", prv_cut = 0.10, lib_cut = 1000, 
               group = "Condition", struc_zero = TRUE, neg_lb = TRUE, tol = 1e-5, 
               max_iter = 100, conserve = TRUE, alpha = 0.05, global = TRUE)

res <- out$res
#res <- res %>% rename("(Intercept)" = "ConditionKO_NASH") 

# select the bottom 20 with lowest p values
res.or_p <- rownames(res$q_val["(Intercept)"])[base::order(res$q_val[,"(Intercept)"])]
taxa_sig <- res.or_p[1:20]
ps.taxa.rel.sig <- prune_taxa(taxa_sig, ps.taxa.rel)
# Only keep gut and tongue samples 
ps.taxa.rel.sig <- prune_samples(colnames(otu_table(ps.taxa.sub)), ps.taxa.rel.sig)



## abudance plot 4-way comparsion for specific bacteria
df <- as.data.frame(matrix)
df2 <- melt(df)


# Plot 1
ggplot(df, aes(x = Condition, y = highest3[,1])) + 
  geom_boxplot() + 
  ylab("CLR abundancs") + # y axis title
  ggtitle(names(highest3)[1]) + # main title
  theme(title = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title.x=element_blank())

###########################################################################################################

##Abundance plot 4-way comparison
df <- as.data.frame(matrix)
df2 <- melt(df)
#highest10 <- df2[order(df2$value, decreasing = TRUE), ][1:10, ]
matching_genes <- rownames(df)[-c(1:2)][colSums(df[,-c(1:2)] == df2$value)]
abundance_gene <- rbind(as.data.frame(t(metadata_sub)), df) %>% t() %>% as.data.frame()

########################matrix의 value로 그린 plot
##한번에 모든 plot을 그리고 저장하는 함수 짜버리기.
genes <- unique(rownames(df))

for (gene in genes) {
  #gene_data <- subset(df, rownames(df) == gene)
  abundance_gene$Condition <- factor(abundance_gene$Condition, levels = c("WT_NORMAL", "WT_NASH", "KO_NORMAL", "KO_NASH"))
  
  abundance_plot <- ggplot(abundance_gene, aes(x = Condition, y = as.numeric(abundance_gene[[gene]]))) +
                    geom_boxplot() +
                    ggtitle(paste("Gene title:", gene)) +
                    theme(title = element_text(size = 7),
                          axis.text = element_text(size = 7),
                          axis.title.x = element_blank())
  ggsave(filename = paste0("./abundance_plot/Abundance plot_", gene, ".png"), plot = abundance_plot)
}

##################################################
