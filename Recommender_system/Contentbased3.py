# -*- coding: utf-8 -*-
"""
Created on Mon Nov 26 01:58:48 2018

@author: Owner
"""
import numpy as np
import matplotlib.pyplot as plt
from sklearn import cross_validation
from sklearn.ensemble import RandomForestRegressor

import warnings
warnings.filterwarnings("ignore")
from sklearn.linear_model import Ridge


Train = np.loadtxt('preprocessedtrain.txt')
test = np.loadtxt('preprocessedtest.txt')

imdbScores = np.loadtxt('imdbScores.txt')
x = Train[:,:-1]
y = Train[:,-1]
trainSet = 0.1*np.array(range(1,10))

#randFor = RandomForestRegressor(max_depth=15, random_state=0, n_estimators=35, max_features=0.5)
# Fit the model to training data
#randFor.fit(x,y)
model = Ridge(alpha=0.8, solver = "lsqr", fit_intercept=False)

print("Fitting Model")
#model.fit(x,y)
 # Compute the predicted output
#yHat = randFor.predict(test)
preds_validation = model.predict(test)
result_file = 'format.dat'
  
output = open(result_file, 'w')
for t in preds_validation:
    if int(t) == -1:
        t = 0
    output.write(str(t))
    output.write("\n")
output.close()

