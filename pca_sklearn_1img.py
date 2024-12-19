import pandas
from sklearn.decomposition import PCA
import numpy as np
import matplotlib.pyplot as plot
import pandas as pd
from scipy import stats
import seaborn as sns
import math

raw_df=pd.read_spss("C:/Users/yujun.ming/Downloads/FSOM_Q2_Camps_ML_260624.sav")
print(raw_df.CARI_ECMEN_v3_camp.value_counts())
raw_df=raw_df[raw_df.cohort.str.contains("FS<6")]
# function definition to compute magnitude o f the vector
def magnitude(x,y): 
    vector=[x,y]
    return math.sqrt(sum(pow(element, 2) for element in vector))


def char_to_data(raw_df,general=False,cari=True):

    #raw_df=raw_df[raw_df.cohort.str.contains("FS<6")]

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
    #raw_df=raw_df[raw_df.hhh_marital==('Married')]
    #raw_df=raw_df[(raw_df.hhh_edulv==('Primary school')) |(raw_df.hhh_edulv==('Illiterate'))]
    #raw_df=raw_df[(raw_df.hhh_edulv==('Secondary school'))|(raw_df.hhh_edulv==('University education (e.g., bachelor\'s degree or higher)')) | (raw_df.hhh_edulv==('Diploma'))]
    #raw_df=raw_df[(raw_df.Number_members_been_working_last_days!=0)]
    #raw_df=raw_df[raw_df.cohort.str.contains("Block Chain")]
    #raw_df=raw_df[raw_df.camp=="Za\'atari Camp"]
    #raw_df=raw_df[(raw_df.Hh_debt_amt>500.0)]
    #raw_df=raw_df[(raw_df.CARI_ECMEN_v3_camp=="Severely food insecure")|(raw_df.CARI_ECMEN_v3_camp=="Moderately food insecure ")]

    #get proportion of the small subgroup after filtering
    curr_data=get_rate(raw_df)
    #calculate weight
    weights = prev_data
    weights.iloc[0] = prev_data.iloc[0] / curr_data.iloc[0]
    print(weights)


    raw_df=raw_df[raw_df.cohort.str.contains("FS<6")]

    print("Current Debt Amount",raw_df.Hh_debt_amt.describe())

    #replace food secure
    replace_rule={"Food secure":1,"Marginally food secure":1,"Moderately food insecure":0,"Severely food insecure":0}

    raw_df["CARI_ECMEN_v3_camp"]=raw_df["CARI_ECMEN_v3_camp"].replace(replace_rule)

    #replace edu level
    replace_rule={"Illiterate":0,"Primary school":0,"Secondary school":1,"Diploma":1,"University education (e.g., bachelor's degree or higher)":1}

    raw_df["hhh_edulv"]=raw_df["hhh_edulv"].replace(replace_rule)
    
    #replace married
    raw_df["hhh_marital"]=np.where(raw_df["hhh_marital"]==("Married"),1,0)

    #replace gender/sex
    raw_df["hhh_sex"]=np.where(raw_df["hhh_sex"]==("Male"),1,0)
    raw_df['hhh_male']=raw_df["hhh_sex"]
    raw_df["hhh_female"]=1-raw_df["hhh_male"]

    #reverse the debt
    raw_df["Hh_debt_amt"]=raw_df["Hh_debt_amt"].astype(float)*-1
    raw_df["Hh_debt_amount_mnth"]=raw_df["Hh_debt_amount_mnth"].astype(float)*-1

    #replace cohorts, higher is mobile money
    raw_df["cohort"]=np.where(raw_df["cohort"].str.contains("Mobile Money"),1,0)


    #apply weight
    raw_df['weighted_calc'] = raw_df['cohort'].copy()
   # Apply gender weights
    raw_df['weighted_calc'] *= np.where(raw_df['hhh_sex'] == 1, weights['male_rate'], weights['female_rate'])

    # Apply marital status weights
    raw_df['weighted_calc'] *= np.where(raw_df['hhh_marital'] == 1, weights['martial_rate'], weights['single_rate'])

    # Apply work status weights
    raw_df['weighted_calc'] *= np.where(raw_df['Number_members_been_working_last_days'] == 0, weights['no_work_rate'], weights['work_rate'])

    # Apply education level weights
    raw_df['weighted_calc'] *= np.where((raw_df['hhh_edulv'] == 0), weights['basic_edu_rate'], weights['higher_edu_rate'])

   # Apply cohort-specific weights
    cohort_weights = {
        "Azraq FS<6 Mobile Money": 0.895402453,
        "Zaatari FS<6 Mobile Money": 1.116523059,
        "Azraq FS<6 Block Chain": 0.917852134,
        "Zaatari FS<6 Block Chain": 1.033072518
    }

    for cohort, weight in cohort_weights.items():
        raw_df['weighted_calc'] *= np.where(raw_df['cohort'] == cohort, weight, 1)

    #calculate the total spending
    raw_df = raw_df.assign(calc_spend_total=0)
    for col in raw_df.columns:
        if "_calc" in col:
            raw_df["calc_spend_total"]+=raw_df[col].astype(float)
    print(raw_df["calc_spend_total"].describe())


    print(raw_df["weighted_calc"].value_counts())
    raw_df["CARI_ECMEN_v3_camp"]=raw_df["CARI_ECMEN_v3_camp"].astype(float)
    raw_df["hhh_edulv"]=raw_df["hhh_edulv"].astype(float)
    raw_df["Number_members_been_working_last_days"]=raw_df["Number_members_been_working_last_days"].astype(float).fillna(0)
    raw_df["member_case_sum"]=raw_df["member_case_sum"].astype(float)
    raw_df["hhh_marital"]=raw_df["hhh_marital"].astype(float)
    raw_df["calc_tobacco_total"]=raw_df["calc_tobacco_total"].astype(float)
    raw_df["Hh_debt_amt"]=raw_df["Hh_debt_amt"].astype(float).fillna(0)
    raw_df['calc_tobacco_total']=raw_df['calc_tobacco_total'].astype(float)
    raw_df['calc_tobacco_per']=raw_df['calc_tobacco_total']/raw_df[raw_df['calc_spend_total']>0]['calc_spend_total']
    raw_df['calc_tobacco_per']=raw_df['calc_tobacco_per'].fillna(0)
    raw_df['in_src_1st_amt']=raw_df['in_src_1st_amt'].astype(float).fillna(0)
    raw_df['Hh_debt_amount_mnth']=raw_df['Hh_debt_amount_mnth'].astype(float).fillna(0)
    raw_df['Hh_debt_amount_mnth']=raw_df['Hh_debt_amount_mnth'].astype(float).fillna(0)
    raw_df['calc_health_services_cash']=raw_df['calc_health_services_cash'].astype(float).fillna(0)
    raw_df['calc_health_services_total']=raw_df['calc_health_services_total'].astype(float).fillna(0)
    raw_df['Number_members_below_five']=raw_df['Number_members_below_five'].astype(float).fillna(0)
    df=raw_df[[
                "weighted_calc",'Hh_debt_amt','calc_tobacco_per',
                'Number_members_been_working_last_days',
                'in_src_1st_amt',
               'Hh_debt_amount_mnth','calc_health_services_total',
               'Number_members_below_five',
               'member_case_sum',
               'hhh_edulv',
               'hhh_marital',#'hhh_female','hhh_male',
               ]]
    return df

def pca_draw(cari=True):
    df=(char_to_data(raw_df,cari))
    #replace name list
    rep_word={"weighted_calc":"Modality(Mobile Money)","Hh_debt_amt":"Less Debt","calc_tobacco_total":"Tobacco","calc_tobacco_per":"More proportion of tobacco spent among total expenditure",
              'Number_members_been_working_last_days':"Adults Working",
                'in_src_1st_amt':"Higher Income",'Hh_debt_amount_mnth':"Less Last Month New Debt",'calc_health_services_total':"More spent on Health Services",
                'Number_members_below_five':"More Children Below 5", 'hhh_marital':"Married",'member_case_sum':"Larger Family Size",
                'hhh_edulv':"Higher Education Level",'hhh_female':"Female","hhh_male":"Male"}
    print(raw_df.CARI_ECMEN_v3_camp.value_counts())
    colors = ['#BFD1EB' if (i == "Food secure" or i=="Marginally food secure") else '#F15959' 
              for i in raw_df#[(raw_df.hhh_sex=="Male")]
                            #[(raw_df.hhh_edulv==('Primary school')) |(raw_df.hhh_edulv==('Illiterate'))]
                            #(raw_df.hhh_edulv==('Secondary school'))|(raw_df.hhh_edulv==('University education (e.g., bachelor\'s degree or higher)')) | (raw_df.hhh_edulv==('Diploma'))]
                            ['CARI_ECMEN_v3_camp']]
    print("blue is",colors.count("blue"))
    print(df.value_counts())

    # You must normalize the data before applying the fit method
    df_normalized=(df - df.mean()) / df.std()
    pca = PCA(n_components=df.shape[1])
    pca.fit(df_normalized)


    # Reformat and view results
    loadings = pandas.DataFrame(pca.components_.T,
                columns=['PC%s' % _ for _ in range(len(df_normalized.columns))],
                index=df.columns)
    print(loadings)
    lds=loadings.to_numpy()
    #quiver for faster process
    #ax = plot.gca()
    #origin = np.array([[0]*len(lds[0]),[0]*len(lds[0])]) # origin point
    #ax.quiver(*origin, lds[:,0], lds[:,1], scale=2)
    ax_l=plot.subplot()
    ax_l.set_xlim(-3,3)
    ax_l.set_ylim(-3,3)
    ax_l.set_xlabel('PC1')
    ax_l.set_ylabel('PC2')
    for(i, txt) in enumerate(loadings.index):

        #replace name for readability
        for key in rep_word:
            txt=txt.replace(key,rep_word[key])

        print(txt,magnitude(lds[i,0], lds[i,1]))
        ax_l.annotate(txt, 
                    xy=(0, 0),
                    xytext =(lds[i,0]*3, lds[i,1]*3),
                    arrowprops=dict(arrowstyle= '<|-',
                                color='#363636',
                                lw=0.5,
                                ls='-',),
                    fontsize=8
                    )
    
    df_PCA = pca.transform(df_normalized)
    plot.scatter(df_PCA[:,0], df_PCA[:,1], c = colors)
    #plot.legend(["Food Insecure","Food Insecure"])
    print(pca.explained_variance_ratio_)
    

pca_draw()
plot.show()