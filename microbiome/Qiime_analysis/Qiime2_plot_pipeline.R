##Import library
library(microbiomeMarker)
library(DESeq2)
library(tidyverse)
library(vegan)
library(ANCOMBC)
library(ComplexHeatmap)
library(phyloseq)
library(reshape2)

##Setting working directory
setwd("~/lab_rlo/microbiome/Qiime2_analysis/silva/")
list.files()

##Import input file, qiime2 output file
otuqza_file <-"./table.qza"
taxaqza_file <- "./taxonomy.qza"
sample_file <- "./sample-metadata.tsv"
treeqza_file <- "./rooted-tree.qza"
taxonomy <- as.data.frame(import_qiime2("./taxonomy.qza"))

qiime_file <- import_qiime2(otu_qza = otuqza_file, taxa_qza = taxaqza_file,sam_tab = sample_file, tree_qza = treeqza_file)
qiime_file

######################
##Save default value##
##################3###
tax <- taxonomy %>%
  select(Taxon) %>% 
  separate(Taxon, c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), "; ")

newSTorder = c("WT_NORMAL", "WT_NASH", "KO_NORMAL", "KO_NASH")

################################
##Alpha Diversity Measure_plot##
################################

