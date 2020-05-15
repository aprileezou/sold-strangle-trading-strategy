
# coding: utf-8

# Instead of pulling out all the volatility surfaces, we build a function to generate volatility surfaces for a specific period.

# Also, 'SPX 3W.xlsx' has been changed a little bit. Name of Column 2: 'date' -> 'Date'

# In[118]:

import pandas as pd
import numpy as np


# In[119]:

df_1m = pd.read_excel('SPX 1M.xlsx')
df_1w = pd.read_excel('SPX 1W.xlsx')
df_2m = pd.read_excel('SPX 2M.xlsx')
df_2w = pd.read_excel('SPX 2W.xlsx')
df_3w = pd.read_excel('SPX 3W.xlsx')


# In[120]:

spx = df_1m.copy().set_index('Date').dropna()
spx = spx.iloc[:,-4]
spx


# In[121]:

def new_df(df,tenor):
    ndf = df.copy()
    ndf = ndf.dropna().set_index('Date')
    if tenor == '3w':
        ndf = ndf.iloc[:,1::3]
        colnames = ['90Last', '95Last', '97.5Last', 
                '100Last', '102.5Last','105Last', '110Last']
        ndf.columns = [ (x+tenor) for x in colnames]
    else:
        ndf = ndf.iloc[:,1:8]
        ndf.columns = [ (x+tenor).replace(' ','') for x in ndf.columns]
    return ndf


# In[122]:

ndf_1w = new_df(df_1w,'1w')
ndf_2m = new_df(df_2m,'2m')
ndf_2w = new_df(df_2w,'2w')
ndf_3w = new_df(df_3w,'3w')


# In[123]:

ndf = ndf_1w.join(ndf_2w).join(ndf_3w).join(ndf_1m).join(ndf_2m).join(spx)
ndf= ndf.dropna()
ndf


# In[255]:

import datetime as dtm
import calendar
def get_months_forward(date,delta):
    newdate = date+dtm.timedelta(31)
    day = newdate.day
    month = newdate.month
    year = newdate.year
    return pd.datetime(year,month,day)


# In[256]:

def get_month_data(start, end=True, ndf=ndf):
    if end:
        end = get_months_forward(start,1)
    df_month = ndf[ndf.index>=start]
    df_month = df_month[df_month.index<=end]
    return df_month


# In[257]:

test = get_month_data(pd.datetime(2019,9,1))
test


# In[258]:

import matplotlib.pyplot as plt
get_ipython().magic('matplotlib notebook')
from scipy import interpolate


# In[259]:

x = [90,95,97.5,100,102.5,105,110]
y = [7,14,21,31,61]
xnames = [str(i)+'Last' for i in x]
ynames = ['1w','2w','3w','1m','2m']
xdict = dict(zip(x,xnames))
ydict = dict(zip(y,ynames))


# In[260]:

def spline_interp(df,date):
    
    xlist = []
    ylist = []
    xnum = len(x)
    ynum = len(y)
    for i in range(xnum):
        xlist.append([x[i] for j in range(ynum)])
        ylist.append([y[j] for j in range(ynum)])
        
    zlist = []
    for cx in x:
        current = []
        for cy in y:
            current.append(f(cx,cy,test,date))
        zlist.append(current)
        
    xnew, ynew = np.mgrid[90:110:400j, 7:60:54j]
    tck = interpolate.bisplrep(xlist, ylist, zlist)
    znew = interpolate.bisplev(xnew[:,0], ynew[0,:], tck)
    
    return xnew,ynew,znew


# In[261]:

def f(x,y,df,date,xdict=xdict,ydict=ydict):
    return df[xdict[x]+ydict[y]].loc[date]


# In[262]:

from mpl_toolkits.mplot3d import Axes3D
def plot_3d(x,y,z):
    fig = plt.figure()
    ax = Axes3D(fig)
    ax.plot_surface(x, y, z, rstride=1, cstride=1, cmap='rainbow')
    plt.show()


# In[263]:

date = test.index[3]
xnew,ynew,znew = spline_interp(test,date)
plot_3d(xnew,ynew,znew)


# In[264]:

def find_near(value,array):
    for i in range(len(array)):
        current = array[i]
        if value<current:
            if i!=0:
                last = array[i-1]
                a1 = (current-value)/(current-last)
                a2 = 1-a1
                index1 = i-1
                index2 = i
            else:
                a1,a2 = 1,0
                index1,index2 = i,i
            break
        if i==len(array)-1:
            a1,a2 = 1,0
            index1,index2 = i,i
    return a1,a2,index1,index2

def calculate_iv(a1,a2,i1,i2,b1,b2,j1,j2,znew):
    zi1j1 = znew[i1,j1]
    zi1j2 = znew[i1,j2]
    zi2j1 = znew[i2,j1]
    zi2j2 = znew[i2,j2]
    return a1*b1*zi1j1 + a1*b2*zi1j2 + a2*b1*zi2j1 + a2*b2*zi2j2


# In[265]:

def get_iv_list(df):
    iv_put = []
    iv_call = []
    date_list = []
    T = len(df.index)
    end_date = get_months_forward(df.index[-1],1)
    for i in range(T):
        date = df.index[-(i+1)]
        SPX_price = df.iloc[-(i+1),-1]
        t = T-(i+1)
        if (end_date-date).days>=7:
            date_list.append(date)
            if i==0:
                strike_low = 0.95*SPX_price
                strike_up = 1.05*SPX_price
                money_low = 0.95
                money_up = 1.05
                iv_put.append(df.loc[date,'95Last1m'])
                iv_call.append(df.loc[date,'105Last1m'])
            else:
                money_low = strike_low/SPX_price*100
                money_up = strike_up/SPX_price*100
                xnew,ynew,znew = spline_interp(df,date)
                mlist = xnew[:,0]
                tlist = ynew[0]

                ma1,ma2,mindex1,mindex2 = find_near(money_low,mlist)
                ta1,ta2,tindex1,tindex2 = find_near(t,tlist)
                put_iv = calculate_iv(ma1,ma2,mindex1,mindex2,ta1,ta2,tindex1,tindex2,znew)
                iv_put.append(put_iv)

                ma1,ma2,mindex1,mindex2 = find_near(money_up,mlist)
                ta1,ta2,tindex1,tindex2 = find_near(t,tlist)
                call_iv = calculate_iv(ma1,ma2,mindex1,mindex2,ta1,ta2,tindex1,tindex2,znew)
                iv_call.append(call_iv)
    return iv_put,iv_call,date_list


# In[266]:

get_iv_list(test)


# In[268]:

iv_call, iv_put,date_list = get_iv_list(test)
plt.plot(date_list,iv_call)
plt.plot(date_list,iv_put)
plt.legend(['put','call'])
plt.xlabel('Date')
plt.ylabel('Black IV')


# In[ ]:



