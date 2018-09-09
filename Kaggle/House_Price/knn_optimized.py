
# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import seaborn as sns
import math
from sklearn import linear_model
import matplotlib.pyplot as plt

raw_train = pd.read_csv('train.csv')    # ID is the index
raw_test = pd.read_csv('test.csv')
corrmat = raw_train.corr()
top = corrmat.nlargest(10, 'SalePrice').index
train = raw_train[top]
train_y = train['SalePrice']
train_x = train.iloc[:, 1:]

test = raw_test[top[1:]]
test.isnull().sum()
test = test.fillna(0)

x = [1,2,3]
y = [4,5,6]

case = pd.DataFrame({'SalePrice':[20,30,50,50],
                     'x':[1,2,3,4],
                     'y':[1,1,2,3],
                     'z':[1,1,5,6]})
target = pd.DataFrame({'x':[1,0],
                       'y':[1,0],
                       'z':[1,1]})

# KNN from Scratch
def get_distance(testPoint, trainPoint):
    dist = 0
    for i in range(0,len(testPoint)):
        dist += (testPoint[i] - trainPoint[i+1])**2      # The first variable is "SalePrice"
    distance = math.sqrt(dist)
    return distance

def get_dfDistance(testPoint, train):
    dic = {}
    for i in range(0, train.shape[0]):
        dic[i] = get_distance(testPoint, train.loc[i])
    return dic


def knn_predict(test, train,k):
    predictions = []
    for i in range(0, test.shape[0]):
        dic = get_dfDistance(test.loc[i], train)
        top = sorted(dic, key=dic.get)
        ktop = top[:k]
        pred = train.iloc[ktop]['SalePrice'].mean()
        predictions.append(pred)
        print (i)
    return predictions

knn_predict(target, case, 1)

# Library
from sklearn.neighbors import KNeighborsRegressor

knn = KNeighborsRegressor(n_neighbors=1)
knn.fit(case.iloc[:,1:], case.SalePrice)

knn.predict(target)

        
my_predictions = knn_predict(test, train, 2)
