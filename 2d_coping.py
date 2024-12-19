from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
from scipy import stats

#helper function
plt.rcParams.update({'font.size': 10})

raw_df=pd.read_spss("C:/Users/yujun.ming/Downloads/FSOM_Q2_Camps_ML_260624.sav")

raw_df=raw_df[raw_df.cohort.str.contains("FS<6")]


#get the proportion of the small subgroup
def get_rate(raw_df):
    print("Now, we have ")
    male_rate=raw_df[raw_df.hhh_sex==('Male')].shape[0]/raw_df.shape[0]
    female_rate=raw_df[raw_df.hhh_sex==('Female')].shape[0]/raw_df.shape[0]
    martial_rate=raw_df[raw_df.hhh_marital==('Married')].shape[0]/raw_df.shape[0]
    single_rate=raw_df[raw_df.hhh_marital!=('Married')].shape[0]/raw_df.shape[0]
    basic_edu_rate=raw_df[(raw_df.hhh_edulv==('Primary school')) |(raw_df.hhh_edulv==('Illiterate'))].shape[0]/raw_df.shape[0]
    higher_edu_rate=raw_df[(raw_df.hhh_edulv==('Secondary school'))|(raw_df.hhh_edulv==('University education (e.g., bachelor\'s degree or higher)')) | (raw_df.hhh_edulv==('Diploma'))].shape[0]/raw_df.shape[0]    
    no_work_rate=raw_df[(raw_df.Number_members_been_working_last_days==0)].shape[0]/raw_df.shape[0]
    work_rate=raw_df[(raw_df.Number_members_been_working_last_days>0)].shape[0]/raw_df.shape[0]
    print("Male:Female",male_rate,":",female_rate)
    print("Married:Single",martial_rate,":",single_rate)
    print("Basic EDU:Higher EDU",basic_edu_rate,":",higher_edu_rate,'\n')
    curr_data={'male_rate':male_rate,'female_rate':female_rate,'martial_rate':martial_rate,'single_rate':single_rate,'basic_edu_rate':basic_edu_rate,'higher_edu_rate':higher_edu_rate,'no_work_rate':no_work_rate,'work_rate':work_rate}
    return pd.DataFrame(curr_data,index=[0])

prev_data=get_rate(raw_df)

#fiter out the rows in raw df
#raw_df=raw_df[raw_df.hhh_sex==('Male')]
# raw_df=raw_df[raw_df.hhh_marital==('Married')]
# raw_df=raw_df[(raw_df.hhh_edulv==('Primary school')) | (raw_df.hhh_edulv==('Secondary school'))]
# raw_df=raw_df[(raw_df.hhh_edulv==('Secondary school'))|(raw_df.hhh_edulv==('University education (e.g., bachelor\'s degree or higher)')) | (raw_df.hhh_edulv==('Diploma'))]
raw_df=raw_df[(raw_df.Number_members_been_working_last_days==0)]
#raw_df=raw_df[raw_df.member_case_sum==5]



#get proportion of the small subgroup after filtering
curr_data=get_rate(raw_df)

#calculate weight
weights = prev_data
weights.iloc[0] = prev_data.iloc[0] / curr_data.iloc[0]
print(weights)
#apply
# Apply weights

raw_df = raw_df.assign(weighted_calc=1)
   # Apply gender weights
raw_df['weighted_calc'] *= np.where(raw_df['hhh_sex'] == 'Male', weights['male_rate'], weights['female_rate'])

# Apply marital status weights
raw_df['weighted_calc'] *= np.where(raw_df['hhh_marital'] == 'Married', weights['martial_rate'], weights['single_rate'])

# Apply work status weights
raw_df['weighted_calc'] *= np.where(raw_df['Number_members_been_working_last_days'] == 0, weights['no_work_rate'], weights['work_rate'])

# Apply education level weights
raw_df['weighted_calc'] *= np.where((raw_df['hhh_edulv'] == 'Primary school') | (raw_df['hhh_edulv'] == 'Illiterate'), weights['basic_edu_rate'], weights['higher_edu_rate'])

# Apply cohort-specific weights
cohort_weights = {
    "Azraq FS<6 Mobile Money": 0.895402453,
    "Zaatari FS<6 Mobile Money": 1.116523059,
    "Azraq FS<6 Block Chain": 0.917852134,
    "Zaatari FS<6 Block Chain": 1.033072518
}

for cohort, weight in cohort_weights.items():
    raw_df['weighted_calc'] *= np.where(raw_df['cohort'] == cohort, weight, 1)



df=pd.crosstab(index=raw_df.cohort,columns=raw_df.Max_coping_behaviour,values=raw_df.weighted_calc,aggfunc='sum')

# #filter out the columns
df=df[df.index.str.contains("FS<6")]

#vul_list=['Least Economically Insufficient','Economically Insufficient','Highly Economically Insufficient']
print(df)

# print(stats.chi2_contingency(df))

#set up figure


#index for graph
index_plot=[221,222,212]

def plot_cohort(input,index_list):
    df_group=df.groupby(lambda x: (input)in x).sum()
    df_group.index=index_list
    df_group=df_group.div(df_group.sum(axis=1), axis=0)*100
    print(df_group)

    colors=['#519DE9','#7CC674','#009596','#EF9234', '#5752D1']
    ax1 = df_group.plot(kind = 'bar', stacked = True,  color = colors)
    ax1.set_title('CARI vs all cohorts')



# input='FS<6'
# index_list=['FS<6','FS>=6']
# plot_cohort(input,index_list)

input="Block Chain"
index_list=['Mobile Money','Block Chain']
plot_cohort(input,index_list)

# input="Azraq"
# index_list=['Azraq','Zaatari']
# plot_cohort(input,index_list)

# plt.show()