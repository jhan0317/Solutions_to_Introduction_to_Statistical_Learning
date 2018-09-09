
import pandas as pd
import numpy as np
import seaborn as sns
from sklearn import linear_model
import matplotlib.pyplot as plt

raw_train = pd.read_csv('train.csv')    # ID is the index
raw_test = pd.read_csv('test.csv')

print(raw_train.head())

print (raw_train.columns)
print (raw_train.shape)

na_count = raw_train.isnull().sum()
na_rate = na_count / len(raw_train)
na_df = pd.concat([na_count, na_rate], axis=1, keys=['count', 'percent'])
na_df = na_df.sort_values(['percent'], ascending=False)
na_df.head(20)

sns.distplot(raw_train[['SalePrice']])

# Heat Map
corrmat = raw_train.corr()
f, ax = plt.subplots(figsize=(12, 9))
sns.heatmap(corrmat, vmax=.8, square=True, ax=ax)
plt.show()

# Top 10 heat map
k = 10
corrmat = raw_train.corr()
top = corrmat.nlargest(k, 'SalePrice').index  # Find the index of top 10 highest correlated variables
top_mat = corrmat.loc[top, top]               # Location their postions in heat map by index
fig, ax = plt.subplots(figsize=(8, 6))
sns.set(font_scale=1.25)
sns.heatmap(top_mat, annot=True, annot_kws={'size': 12}, square=True)
plt.show()

train = raw_train[top]
train_y = train['SalePrice']
train_x = train.iloc[:, 1:]

test = raw_test[top[1:]]
test.isnull().sum()
test = test.fillna(0)

# Parametric approach
linearR = linear_model.LinearRegression()
linearR.fit(train_x, train_y)
pred_y = linearR.predict(test)
result = pd.DataFrame({'Id':raw_test['Id'].as_matrix(), 'SalePrice':pred_y.astype(np.int32)})
# result.to_csv('predictions.csv', index=False)

case = pd.DataFrame({'SalePrice':[20,30,50,50],
                     'x':[1,2,3,4],
                     'y':[1,1,2,3],
                     'z':[1,1,5,6]})
target = pd.DataFrame({'x':[1,0],
                       'y':[1,0],
                       'z':[1,1]})

# Non-parametric approach
import math

def get_distance(df, target):
    dic = {}
    for i in range(0, df.shape[0]):
        dist = 0
        for col in df.columns[1:-1]:
            dist += (df.loc[i][col] - target[col])**2
        dic[i] = math.sqrt(dist)
    return dic

dic = get_distance(case, target.loc[0])

def knn_predict(df, df_target,k):
    predictions = []
    for i in range(0, df_target.shape[0]):
        dic = get_distance(df, df_target.loc[i])
        top = sorted(dic, key=dic.get)
        ktop = top[:k]
        print (ktop)
        pred = df.iloc[ktop]['SalePrice'].mean()
        predictions.append(pred)
    return predictions

# Library
from sklearn.neighbors import KNeighborsRegressor

knn = KNeighborsRegressor(n_neighbors=3)
knn.fit(train.iloc[:,1:], train.SalePrice)
knn.predict(test)

#try1 = knn_predict(train,test,3)