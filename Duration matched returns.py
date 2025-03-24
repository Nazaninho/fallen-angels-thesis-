#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from tqdm import tqdm
from dateutil.relativedelta import *
from pandas.tseries.offsets import MonthEnd
from joblib import Parallel, delayed    
import datetime


# In[2]:


# Load CRSP Fixed Term Indices Header Information from a CSV file
crsp_tr_headers = pd.read_csv("C:/Users/iraho/Downloads/MT data/TFZ_IDX.csv")

crsp_tr_headers = crsp_tr_headers[crsp_tr_headers.TIDXFAM == "FIXEDTERM"]
crsp_tr_headers = crsp_tr_headers.assign(term = [1,2,5,7,10,20,30])
crsp_tr_headers = crsp_tr_headers[['KYTREASNOX',
                                   'term',
                                   ]]
crsp_tr_headers.rename(columns={'KYTREASNOX': 'kytreasnox'}, inplace=True)
print(crsp_tr_headers.head())


# In[3]:


# Define the path to your CSV file
file_path = "C:/Users/iraho/Downloads/MT data/TFZ_MTH_FT.csv"

# Load the CSV with specific columns
# Specify which columns to read and what their new names should be
crsp_tr_ = pd.read_csv(file_path, usecols=['KYTREASNOX', 'MCALDT', 'TMYEARSTM', 'TMDURATN', 'TMRETADJ', 'TMYTM'])

# Rename the columns immediately after loading
crsp_tr_.columns = ['kytreasnox', 'date', 't_tmt', 't_dur', 't_ret', 't_yld']


# In[4]:


# Merge
crsp_tr_ = crsp_tr_.merge(crsp_tr_headers,
                         on  = ['kytreasnox'],
                         how = 'left')

crsp_tr_['kytreasnox'] = crsp_tr_['kytreasnox'].astype(int)


# In[5]:


# Convert Macauly Duration to Years #
crsp_tr_['date'] = pd.to_datetime(crsp_tr_['date'])
crsp_tr_['total_days'] = np.where(crsp_tr_['date'].dt.is_leap_year, 
                                  366, 
                                  365)
crsp_tr_['t_dur'] = crsp_tr_['t_dur']/crsp_tr_['total_days']


# In[6]:


# Convert Macauly Duration to Modified Duration #
# Scale return and yield to be in decimal format. #
crsp_tr_['t_ret'] = crsp_tr_['t_ret']/100
crsp_tr_['t_yld'] = crsp_tr_['t_yld']/100


# In[7]:


# We assume semi-annual coupon payments for U.S. T-Bonds.
# Empirically, this is the case.
crsp_tr_['t_mod_dur'] = ((crsp_tr_['t_dur']*2)/ (1+(crsp_tr_['t_yld']/2)))/2

# Align the dates #
crsp_tr_  = crsp_tr_[ crsp_tr_['date'] >= "2002-06-30"]
crsp_tr_ ['date'] = crsp_tr_ ['date'] + MonthEnd(0)


# In[8]:


# Create date indexed dataframe, where in each element of the "dat"
# column we will store the entire yield curve sub-dataframe.
crsp_dat = pd.DataFrame(np.nan, 
                        index = crsp_tr_['date'].unique(),
                        columns = ['dat'])

for m,t in enumerate(crsp_tr_['date'].unique()):   
    dfc = crsp_tr_[ crsp_tr_['date'] == t].T  
    dfc.columns = dfc.loc['term'] 
    crsp_dat['dat'].iloc[m] = dfc


# In[9]:


# In each element of the "dat" column, we store all the data we
# need including the duration, maturity and so on.    
crsp_dat = crsp_dat.reset_index()
crsp_dat.columns = ['date','dat']
crsp_dat = crsp_dat.set_index(['date'])
crsp_dat.index = crsp_dat.index+MonthEnd(0)
crsp_dat = crsp_dat.reset_index()


# In[17]:


import pandas as pd

# Define the path to your CSV file
file_path = "C:/Users/iraho/Downloads/MT data/merged_table.csv"

# Specify the columns to read from the CSV and their expected data types
dtype_spec = {
    'CUSIP': str,
    'TMT.x': float,  # Assuming 'TMT', 'DURATION', 'YIELD', 'RET_EOM' are numerical
    'DURATION.x': float,
    'YIELD.x': float,
    'RET_EOM': float
}

# Specify the columns to read from the CSV
use_cols = ['DATE.x', 'CUSIP', 'TMT.x', 'DURATION.x', 'YIELD.x', 'RET_EOM.x','T_DATE.x']

# Load the CSV with specified columns and data types, parsing 'DATE.x' as date
traced = pd.read_csv(
    file_path,
    usecols=use_cols,
    dtype=dtype_spec,
    parse_dates=['DATE.x'],
    low_memory=False
)

# Rename 'DATE.x' to 'date' and other necessary renames if required
traced.rename(columns={
    'T_DATE.x': 'T_DATE.x',
    'CUSIP' : 'cusip',
    'DATE.x': 'date',
    'TMT.x': 'tmt',
    'DURATION.x': 'duration',
    'YIELD.x': 'yield',
    'RET_EOM': 'ret_eom'
}, inplace=True)

# Now, set the index to 'date' and 'CUSIP', sort it, and reset the index
# This prepares the data exactly as per your original code structure
traced = traced.set_index(['date', 'cusip'])\
               .sort_index(level=['date', 'cusip'])\
               .reset_index()

# Print the head of the DataFrame to verify correctness
print(traced.head())


# In[18]:


traced['date'] = pd.to_datetime(traced['date'])
crsp_dat['date'] = pd.to_datetime(crsp_dat['date'])


df = traced.merge(  crsp_dat , left_on  = ['date'], 
                               right_on = ['date'], 
                               how      = "inner"  )


# In[50]:





# In[35]:


def ComputeCredit(x):  
    
    x_dur = x.duration
    x_tmt = x.tmt
      
    _df_dur = x.dat.T.t_mod_dur  
    _df_tmt = x.dat.T.t_tmt
    
              
    idx_dur = _df_dur .iloc[(_df_dur - x_dur).abs().argsort()]\
        .reset_index().term[:2].sort_values() 
    idx_tmt = _df_tmt .iloc[(_df_tmt - x_tmt).abs().argsort()]\
        .reset_index().term[:2].sort_values()     
                     
    x_dur_ = list(x.dat.T.t_dur[ idx_dur ])
    x_tmt_ = list(x.dat.T.t_dur[ idx_dur ])
    x_tmt_ = list(x.dat.T.t_tmt[ idx_tmt ])
    x_ttm_ = list(x.dat.T.term[  idx_tmt ])
    
    # Yields
    y_dur_ = list(x.dat.T.t_yld[ idx_dur ])
    y_tmt_ = list(x.dat.T.t_yld[ idx_tmt ])
    y_ttm_ = list(x.dat.T.t_yld[ idx_tmt ])
    
    # Interpolation (Linear)
    yld_interp_dur = np.interp( x_dur, x_dur_, y_dur_ )  
    yld_interp_tmt = np.interp( x_tmt, x_tmt_, y_tmt_ )  
    yld_interp_ttm = np.interp( x_tmt, x_ttm_, y_ttm_ )  
    
    # Returns
    r_dur_ = list(x.dat.T.t_ret[ idx_dur ])
    r_tmt_ = list(x.dat.T.t_ret[ idx_tmt ])
    r_ttm_ = list(x.dat.T.t_ret[ idx_tmt ])
    
    # Interpolation (Linear)
    ret_interp_dur = np.interp( x_dur, x_dur_, r_dur_ )  
    ret_interp_tmt = np.interp( x_tmt, x_tmt_, r_tmt_ )  
    ret_interp_ttm = np.interp( x_tmt, x_ttm_, r_ttm_ )   
    
    cusip = x.cusip 
    date  = x.date 
    
          
    return (cusip, date, 
            yld_interp_dur, yld_interp_tmt, yld_interp_ttm,
            ret_interp_dur, ret_interp_tmt, ret_interp_ttm
            )       


# In[15]:





# In[34]:





# In[36]:


import time

start_time = time.time()

df_export = pd.DataFrame(
    Parallel(n_jobs=14)(delayed(ComputeCredit)(x)
                       for x in tqdm(df.itertuples(index=False))),
    columns=['cusip','date', 
             'yld_interp_dur', 'yld_interp_tmt','yld_interp_ttm',
             'ret_interp_dur', 'ret_interp_tmt','ret_interp_ttm']
    ) 

for i in range(10000000):
    pass

end_time = time.time()
elapsed_time = end_time - start_time
print(f"The code ran for {elapsed_time} seconds.")


# In[37]:


# Assume original df has the same order and length as df_export
df_export['t_date'] = df['T_DATE.x'].values


# In[38]:


# Verify the output
print(df_export.head())

# If needed, merge based on a key, for example, 'cusip' and 'date'
# This is only necessary if the order or length differs
df_export = df_export.merge(df[['cusip', 'date', 'T_DATE.x']], on=['cusip', 'date'], how='left')


# In[39]:


# Define the path where you want to save the CSV file on your computer
file_path = "C:/Users/iraho/Downloads/MT data/df_export.csv"

# Save the DataFrame to a CSV file
df_export.to_csv(file_path, index=False)  # index=False means do not write row indices)


# In[ ]:




