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