
from biom import Table
import pandas as pd

# TSV 파일 경로
tsv_file = pd.read_csv("/home/rlo/lab_rlo/microbiome/picrust2_analysis/KO_pathway_annotation_results.tsv", sep='\t', index_col=0)

# BIOM 파일 경로
biom_file = "KO_pathway.biom"

#
# 데이터 프레임을 BIOM 형식의 테이블로 변환
biom_table = Table(tsv_file.values.T, observation_ids=tsv_file.columns, sample_ids=tsv_file.index)

# BIOM 형식으로 저장
biom_table.to_hdf5(biom_table, 'pathway_annotation.biom')
