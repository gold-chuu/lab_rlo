"""
    date: 2023.07.04
    object: metabolic metabolite visualization
    author: rlo
"""

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mat
import matplotlib.pyplot as plt
from pathlib import Path
from sklearn.preprocessing import StandardScaler
# Reading excel file 
metabolite = pd.read_csv("/home/rlo/lab_rlo/metagenome/heatmap_matrix.csv", index_col=None)


col_names = metabolite.columns[2:]
row_names = metabolite["Compound name"]
scaler = StandardScaler()
df_standard = scaler.fit_transform(metabolite.iloc[:, 1:].iloc[:,1:])

standard = pd.DataFrame(df_standard, columns = col_names)
standard.to_excel('output.xlsx', index=False)
pd.concat(standard, row_names)

#Draw heatmap with sample_info, metabolite
metabolite.columns = metabolite.iloc[0]
metabolite = metabolite[1:].iloc[:, :23]

#heat_df = metabolite[~metabolite.apply(lambda row: row.astype(str).str.contains('N.D.')).any(axis=1)]
heat_df = metabolite.drop(metabolite.columns[1:7], axis=1)
heat_df.set_index(heat_df.columns[0], inplace=True)
#heat_df['열이름'] = pd.to_numeric(heat_df['열이름'], errors='coerce')
 
for i in range(0, 16):  # 2부터 7까지 열을 선택
    heat_df.iloc[:, i] = pd.to_numeric(heat_df.iloc[:, i], errors='coerce')

## heatmap 그리기
sns.heatmap(heat_df, annot=True)
plt.show()
