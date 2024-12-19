from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random

#helper function
plt.rcParams.update({'font.size': 7})

raw_df=pd.read_spss("/Users/yujunming/Downloads/FSOM_Q2_Camps_ML_260624.sav")

#fiter out the rows in raw df
raw_df=raw_df[raw_df.hhh_sex==('Female')]
raw_df=raw_df[raw_df.hhh_marital==('Married')]
raw_df=raw_df[raw_df.cohort.str.contains('FS<6')]

df=pd.crosstab(index=raw_df.hhh_edulv,columns=raw_df.CARI_ECMEN_v3_camp)

edu_list=["Diploma","University education (e.g., bachelor's degree or higher)","Secondary school","Primary school","Illiterate"]
df=df.reindex(edu_list,axis="index")

#filter out the columns
# df=df[df.index.str.contains("FS<6")]

vul_list=['Least Economically Insufficient','Economically Insufficient','Highly Economically Insufficient']
print(df)


#set up figure


#index for graph
index_plot=[221,222,212]

def plot_cohort(input,index_list):
    df_group=df.copy()
    # df_group=df_group.groupby(lambda x: (input)in x).sum()
    # df_group.index=index_list
    df_group=df_group.div(df_group.sum(axis=1), axis=0)*100
    print(df_group)

    colors=['#519DE9','#7CC674','#009596','#EF9234', '#5752D1']
    ax1 = df_group.plot(kind = 'barh', stacked = True,  color = colors)
    ax1.set_title('CARI vs all cohorts')


# input='FS<6'
# index_list=['FS<6','FS>=6']
# plot_cohort(input,index_list)

input="Female"
index_list=['Female']
plot_cohort(input,index_list)

# input="Azraq"
# index_list=['Azraq','Zaatari']
# plot_cohort(input,index_list)

plt.show()