import pandas as pd
import numpy as np
from sklearn import linear_model

raw_train = pd.read_csv('train.csv')
raw_test = pd.read_csv('test.csv')

print (raw_train.columns)
train_y = raw_train[['Survived']]

train_X = raw_train[['Pclass', 'SibSp', 'Parch']]

clf = linear_model.LogisticRegression(C=1.0, penalty='l1', tol=1e-6)
clf.fit(train_X, train_y)

test_X = raw_test[['Pclass', 'SibSp', 'Parch']]
predictions = clf.predict(test_X)
result = pd.DataFrame({'PassengerId':raw_test['PassengerId'].as_matrix(), 'Survived':predictions.astype(np.int32)})
result.to_csv('predictions.csv', index=False)
