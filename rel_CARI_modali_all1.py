from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
from scipy import stats
import seaborn as sns

#helper function
plt.rcParams.update({'font.size': 10})
fig = plt.figure(figsize=(7, 7))
raw_df=pd.read_spss("C:/Users/yujun.ming/Downloads/FSOM_Q2_Camps_ML_260624.sav")

df=raw_df[raw_df.cohort.str.contains("FS<6")]

#get the proportion of the small subgroup
def get_rate(raw_df):
    #print("Now, we have ")
    male_rate=raw_df[raw_df.hhh_sex==('Male')].shape[0]/raw_df.shape[0]
    female_rate=raw_df[raw_df.hhh_sex==('Female')].shape[0]/raw_df.shape[0]
    martial_rate=raw_df[raw_df.hhh_marital==('Married')].shape[0]/raw_df.shape[0]
    single_rate=raw_df[raw_df.hhh_marital!=('Married')].shape[0]/raw_df.shape[0]
    basic_edu_rate=raw_df[(raw_df.hhh_edulv==('Primary school')) |(raw_df.hhh_edulv==('Illiterate'))].shape[0]/raw_df.shape[0]
    higher_edu_rate=raw_df[(raw_df.hhh_edulv==('Secondary school'))|(raw_df.hhh_edulv==('University education (e.g., bachelor\'s degree or higher)')) | (raw_df.hhh_edulv==('Diploma'))].shape[0]/raw_df.shape[0]    
    no_work_rate=raw_df[(raw_df.Number_members_been_working_last_days==0)].shape[0]/raw_df.shape[0]
    work_rate=raw_df[(raw_df.Number_members_been_working_last_days>0)].shape[0]/raw_df.shape[0]
    #print("Male:Female",male_rate,":",female_rate)
    #print("Married:Single",martial_rate,":",single_rate)
    #print("Basic EDU:Higher EDU",basic_edu_rate,":",higher_edu_rate,'\n')
    curr_data={'male_rate':male_rate,'female_rate':female_rate,'martial_rate':martial_rate,'single_rate':single_rate,'basic_edu_rate':basic_edu_rate,'higher_edu_rate':higher_edu_rate,'no_work_rate':no_work_rate,'work_rate':work_rate}
    return pd.DataFrame(curr_data,index=[0])

prev_data=get_rate(raw_df)

def plot_crosstab(raw_df_cp,input,index_list):
    df=pd.crosstab(index=raw_df_cp.cohort,columns=raw_df_cp.CARI_ECMEN_v3_camp)
    df_group=df.groupby(lambda x: (input)in x).sum()
    df_group.index=index_list
    df_group=df_group.T
    p_result=stats.chi2_contingency(df_group)
    df_2_group=df_group.div(df_group.sum(axis=0), axis=1)*100
    return [df_group,df_2_group,p_result]

def is_float(val):
        try:
            float(val)
        except ValueError:
            return False
        else:
            return True


#fiter out the rows in raw df
#raw_df=raw_df[raw_df.hhh_sex==('Female')]
#raw_df=raw_df[raw_df.hhh_marital==('Married')]
#raw_df=raw_df[(raw_df.hhh_edulv==('Primary school')) |(raw_df.hhh_edulv==('Illiterate'))]
#raw_df=raw_df[(raw_df.hhh_edulv==('Secondary school'))|(raw_df.hhh_edulv==('University education (e.g., bachelor\'s degree or higher)')) | (raw_df.hhh_edulv==('Diploma'))]
#raw_df=raw_df[(raw_df.Number_members_been_working_last_days!=0)]
#raw_df=raw_df[raw_df.cohort.str.contains("Block Chain")]

def print_corr(df_copy):

    raw_df=df_copy.copy()

    #get proportion of the small subgroup after filtering
    curr_data=get_rate(raw_df)

    #calculate weight
    weights = prev_data.copy()
    weights.iloc[0] = prev_data.iloc[0] / curr_data.iloc[0]
    #print(weights)


    raw_df=raw_df[raw_df.cohort.str.contains("FS<6")]

    #print("Current Debt Amount",raw_df.Hh_debt_amt.describe())

    # #replace food secure
    # replace_rule={"Food secure":1,"Marginally food secure":0.5,"Moderately food insecure":-0.5,"Severely food insecure":-1}

    # raw_df["CARI_ECMEN_v3_camp"]=raw_df["CARI_ECMEN_v3_camp"].replace(replace_rule)


    #replace married
    raw_df["hhh_marital"]=np.where(raw_df["hhh_marital"]==("Married"),1,0)

    #replace gender/sex
    raw_df["hhh_sex"]=np.where(raw_df["hhh_sex"]==("Male"),1,0)

    #replace cohorts, higher is mobile money
    raw_df["cohort"]=np.where(raw_df["cohort"].str.contains("Mobile Money"),1,0)




    #apply weight
    raw_df = raw_df.assign(weighted_calc=1)
    # Apply gender weights
    raw_df['weighted_calc'] *= np.where(raw_df['hhh_sex'] == 1, weights['male_rate'], weights['female_rate'])

    # Apply marital status weights
    raw_df['weighted_calc'] *= np.where(raw_df['hhh_marital'] == 1, weights['martial_rate'], weights['single_rate'])

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

    raw_df["CARI_unrounded_ECMEN_v3_camp"]*=raw_df["weighted_calc"]
    print(raw_df["CARI_unrounded_ECMEN_v3_camp"].describe())
    
    #print(raw_df["weighted_calc"].value_counts())
    df=raw_df[["cohort","CARI_unrounded_ECMEN_v3_camp"]]
    corr=df.corr(method="spearman")
    #print(df.corr(method="spearman"))
    return -1*corr.iloc[0,1]

def loop_find():
    corr_df=pd.DataFrame()
    ori_copy=df.copy()
    selected_columns=["Hh_debt_amt","Hh_debt_amount_mnth","calc_tobacco_total","in_src_1st_amt","calc_health_services_cash",
                      "hhh_sex","hhh_marital","hhh_edulv","Number_members_been_working_last_days","camp","Number_members_below_five","member_case_sum","Number_members_school_age"]
    #for col in raw_df.columns:
    for col in selected_columns:
        col_values=raw_df[col].unique()
        if col_values.size<10 and col_values.size>1:
            #print(col,raw_df[col].unique())
            for value in col_values:
                raw_df_cp=ori_copy.copy()
                raw_df_cp=raw_df_cp[raw_df_cp[col]==value]
                if raw_df_cp.shape[0]>30:
                    corr_value=(print_corr(raw_df_cp))
                    print(col+"_"+str(value),corr_value)
                    if np.isnan(corr_value)==False:
                        corr_key=col+"_"+str(value)
                        cross=plot_crosstab(raw_df_cp,"Block Chain",['Mobile Money','Block Chain'])
                        corr_df=pd.concat([corr_df,pd.DataFrame({corr_key:[corr_value,raw_df_cp.shape[0],str(cross[0]),str(cross[1]),str(cross[2])]})],axis=1)
        elif col=="Hh_debt_amt" or col=="Hh_debt_amount_mnth" or col=="calc_tobacco_total" or col=="in_src_1st_amt" or col=="calc_health_services_cash":
             #print(col,raw_df[col].unique())
            mean_val=raw_df[col].median()
            col_values=["Larger than "+str(mean_val),"Smaller than "+str(mean_val)]
            raw_df_cp=ori_copy.copy()
            #in case it is larger than the mean
            raw_df_cp=raw_df_cp[raw_df_cp[col]>mean_val]
            if raw_df_cp.shape[0]>30:
                corr_value=(print_corr(raw_df_cp))
                print(col+"_"+str(col_values[0]),corr_value)
                if np.isnan(corr_value)==False:
                    corr_key=col+"_"+str(col_values[0])
                    cross=plot_crosstab(raw_df_cp,"Block Chain",['Mobile Money','Block Chain'])
                    corr_df=pd.concat([corr_df,pd.DataFrame({corr_key:[corr_value,raw_df_cp.shape[0],str(cross[0]),str(cross[1]),str(cross[2])]})],axis=1)
            #in case it is smaller than the mean
            raw_df_cp=ori_copy.copy()
            raw_df_cp=raw_df_cp[raw_df_cp[col]<=mean_val]
            if raw_df_cp.shape[0]>30:
                corr_value=(print_corr(raw_df_cp))
                print(col+"_"+str(col_values[1]),corr_value)
                if np.isnan(corr_value)==False:
                    corr_key=col+"_"+str(col_values[1])
                    cross=plot_crosstab(raw_df_cp,"Block Chain",['Mobile Money','Block Chain'])
                    corr_df=pd.concat([corr_df,pd.DataFrame({corr_key:[corr_value,raw_df_cp.shape[0],str(cross[0]),str(cross[1]),str(cross[2])]})],axis=1)


    #Origional set
    raw_df_cp=ori_copy.copy()                
    if raw_df_cp.shape[0]>30:
        corr_value=(print_corr(raw_df_cp))
        print("General_condition",corr_value)
        if np.isnan(corr_value)==False:
            corr_key="General"+"_"+"Condition"
            cross=plot_crosstab(raw_df_cp,"Block Chain",['Mobile Money','Block Chain'])
            corr_df=pd.concat([corr_df,pd.DataFrame({corr_key:[corr_value,raw_df_cp.shape[0],str(cross[0]),str(cross[1]),str(cross[2])]})],axis=1)

    #corr_df=corr_df.sort_values(by=0,axis=1)
    print(corr_df)
    corr_df.to_csv("C:/Users/yujun.ming/Downloads/corr.csv")


loop_find()
