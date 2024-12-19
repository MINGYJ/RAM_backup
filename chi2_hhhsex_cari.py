import pandas
from sklearn.decomposition import PCA
import numpy as np
import matplotlib.pyplot as plot
import pandas as pd
from scipy import stats
import seaborn as sns
import math

raw_df=pd.read_spss("C:/Users/yujun.ming/Downloads/FSOM_Q2_Camps_ML_260624.sav")

raw_df=raw_df[raw_df.cohort.str.contains("FS<6")]
#raw_df=raw_df[(raw_df.hhh_edulv==('Primary school')) |(raw_df.hhh_edulv==('Illiterate'))]
cross_table=pd.crosstab(index=raw_df.hhh_sex,columns=raw_df.CARI_ECMEN_v3_camp)
cross_table=cross_table.groupby(lambda x: (x=="Female")).sum()
cross_table.index=['Female','Male']
if ('Food secure' in cross_table.columns):
        cross_table['Secure']=cross_table['Food secure']+cross_table['Marginally food secure']
else:
    cross_table['Secure']=cross_table['Marginally food secure']
cross_table['Insecure']=cross_table['Moderately food insecure']+cross_table['Severely food insecure']
cross_table=cross_table[['Secure','Insecure']]
print(cross_table)
print(stats.chi2_contingency(cross_table))
cross_t_per=cross_table.copy()
cross_t_per=cross_t_per.div(cross_t_per.sum(axis=1), axis=0)*100
print(cross_t_per)
#print(stats.chi2_contingency(cross_t_per))