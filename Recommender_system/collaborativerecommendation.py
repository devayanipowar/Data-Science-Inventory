# -*- coding: utf-8 -*-
"""
Created on Sun Nov 25 22:27:00 2018

@author: Owner
"""


import pandas as pd
from sklearn.metrics import mean_squared_error
from sklearn.decomposition import TruncatedSVD
import numpy as np


train = pd.read_csv('train.dat',sep='\s+')
test = pd.read_csv('test.dat',sep='\s+')
matrix = pd.concat([train,test]).pivot('userID','movieID','rating')
mean = matrix.mean()
usermean = matrix.mean(axis=1)
#zero-mean method : each of users ratings is shifted to each movies mean rating, leaving unrated movies at zero rating
zero_mean = matrix-mean
mean1 = zero_mean.fillna(0)
bmean = -zero_mean.isnull()

i = 0
converge = 1000
while i<11:
    i += 1
    svd = TruncatedSVD(n_components=20,random_state=42)
    svd.fit(mean1)
    mean1svd = pd.DataFrame(svd.inverse_transform(svd.transform(mean1)),columns=mean1.columns,index=mean1.index)

    mse = mean_squared_error(mean1svd[bmean].fillna(0),zero_mean[bmean].fillna(0))
    
    print('%i %f %f'%(i,mse,converge-mse))
    mean1svd[bmean] = zero_mean[bmean]
    x = mean1svd[bmean]
    y = zero_mean[bmean]
    mean1 = mean1svd
    if converge-mse<0.00001: break
    converge = mse

m = mean1+mean
m = m.clip(lower=1,upper=5)

test['rating'] = test.apply(lambda x:m[m.index==x.userID][x.movieID].values[0],axis=1)

missing = np.where(test.rating.isnull())[0]
test.ix[missing,'rating'] = usermean[test.loc[missing].userID].values

test.to_csv('submit.csv',index=False)