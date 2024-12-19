from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
from scipy import stats

#helper function
plt.rcParams.update({'font.size': 10})

raw_df=pd.read_spss("/Users/yujunming/Downloads/FSOM_Q2_Camps_ML_260624.sav")
#fiter out the rows in raw df
#raw_df=raw_df[raw_df.hhh_sex==('Male')]
#raw_df=raw_df[raw_df.hhh_marital!=('Married')]
#raw_df=raw_df[(raw_df.hhh_edulv==('Primary school')) |(raw_df.hhh_edulv==('Illiterate'))]
#raw_df=raw_df[(raw_df.hhh_edulv==('Secondary school'))|(raw_df.hhh_edulv==('University education (e.g., bachelor\'s degree or higher)')) | (raw_df.hhh_edulv==('Diploma'))]
#raw_df=raw_df[(raw_df.Number_members_been_working_last_days==0)]

df=pd.crosstab(index=raw_df.cohort,columns=raw_df.ECMEN_class_4pt_v3_camp)

#filter out the columns
df=df[df.index.str.contains("FS<6")]
#df=df[df.index.str.contains("Azraq")]

vul_list=['Least Economically Insufficient','Economically Insufficient','Highly Economically Insufficient']
print(df)

print(stats.chi2_contingency(df))

#set up figure


#index for graph
index_plot=[221,222,212]

def plot_cohort(input,index_list):
    df_group=df.groupby(lambda x: (input)in x).sum()
    # if ('Food secure' in df_group.columns):
    #     df_group['Secure']=df_group['Food secure']+df_group['Marginally food secure']
    # else:
    #     df_group['Secure']=df_group['Marginally food secure']
    # df_group['Insecure']=df_group['Moderately food insecure']+df_group['Severely food insecure']
    # df_group=df_group[['Secure','Insecure']]
    df_group.index=index_list
    # df_group=df_group.div(df_group.sum(axis=1), axis=0)*100
    df_2_group=df_group.T
    # print(df_group)
    # print(stats.chi2_contingency(df_group))
    # df_group=df_group.div(df_group.sum(axis=1), axis=0)*100
    # print(df_group)

    print(df_2_group)
    print(stats.chi2_contingency(df_2_group))
    df_2_group=df_2_group.div(df_2_group.sum(axis=0), axis=1)*100
    print(df_2_group)
    print(stats.chi2_contingency(df_2_group))



# input='FS<6'
# index_list=['FS<6','FS>=6']
# plot_cohort(input,index_list)

input="Block Chain"
index_list=['Block Chain','Mobile Money']
plot_cohort(input,index_list)

# input="Azraq"
# index_list=['Azraq','Zaatari']
# plot_cohort(input,index_list)

# plt.show()