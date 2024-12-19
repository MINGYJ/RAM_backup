import pandas as pd 
import pyreadstat
import matplotlib.pyplot as plt
df=pd.read_spss("C:/Users/yujun.ming/Downloads/FSOM_Q2_Camps_ML_260624.sav")
print(df.columns.tolist())
print(set(df.cohort.tolist()))
print(set(df.ECMEN_class_4pt_v3_camp.tolist()))

# ana_part=input("Please input the part you want to analyze:")

# data_matrix_money=pd.DataFrame()
# data_matrix_money["money"]=(df["cohort"].str.contains(ana_part)).astype(int)
# print(data_matrix_money["money"].value_counts())

# fig = plt.figure(figsize=(6, 3))
# ax1 = fig.add_subplot(121, projection='3d')
# ax2 = fig.add_subplot(122, projection='3d')

print(df["cohort","ECMEN_class_4pt_v3_camp"].corr())


