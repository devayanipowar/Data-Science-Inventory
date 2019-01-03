import re
import numpy as np
from sklearn import model_selection
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier  
from sklearn.metrics import classification_report, confusion_matrix  
from sklearn.decomposition import TruncatedSVD
from imblearn.over_sampling import SMOTE
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier
from imblearn.pipeline import make_pipeline as pipe
from sklearn.naive_bayes import BernoulliNB, ComplementNB, MultinomialNB
from sklearn.model_selection import cross_val_predict

size = 100001

def Load(x):
    with open(x)as f: lines = [line.rstrip('\n') for line in f]
    labels = [int(l[0]) for l in lines]
    docs = [re.sub(r'[^\w]',' ',l[1:]).split() for l in lines]
    features = []
    for doc in docs:
        line = [0]*size
        for index, val in enumerate(doc):
            line[int(val)] = 1
        features.append(line)
    return features, labels

def Loadt(x):
        with open(x)as f: lines = [line.rstrip('\n') for line in f]
        docs = [re.sub(r'[^\w]',' ',l).split() for l in lines]
        features = []
        for doc in docs:
            line = [0]*size
            for index, val in enumerate(doc):
                line[int(val)] = 1
            features.append(line)
        return features


print("Loading training data")
features, labels = Load("train_drugs.data")
k_folds = 10

tsvd = TruncatedSVD(n_components=1500,n_iter=50)

print("reducing data...")
tsvd.fit(features,labels)
rfeature= tsvd.transform(features)
#X_train, X_test, y_train, y_test = train_test_split(rfeature, labels,random_state=4, test_size=0.2)

'''
print("balancing data.....")
sm = SMOTE(random_state=42,kind='svm')
rfeature, labels = sm.fit_sample(train_r, labels)
'''
print("load data")
test_f= Loadt("test.data")
print("reduce test data")
test_rf = tsvd.transform(test_f)


print("start classification....")
clf = DecisionTreeClassifier(random_state=53,class_weight={0: 1, 1: 1.5})
#clf = SVC(kernel='linear')  
'''
#clf=RandomForestClassifier(n_estimators= 15,random_state=53,class_weight={0: 1, 1: 1.5})
#clf.fit(rfeature, labels)  
#y_pred= clf.predict(test_r)
#print(classification_report(y_test,y_pred))
'''
pipeline=pipe(SMOTE(random_state=42,kind='svm'),clf)
model=pipeline.fit(rfeature,labels)
test_predicted = model.predict(test_rf)


#clf.fit(rfeature, labels)
#test_predicted = clf.predict(test_rf)

print('writing results')
np.savetxt('final1output3.txt', test_predicted, delimiter='; ')


    
      
    




