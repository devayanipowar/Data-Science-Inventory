# -*- coding: utf-8 -*-
"""
Created on Wed Sep 19 23:47:00 2018

@author: owner
"""

import nltk, string
from sklearn.metrics import accuracy_score
from sklearn.metrics.pairwise import cosine_similarity
from nltk import word_tokenize
from nltk.stem.porter import PorterStemmer
from nltk.stem import WordNetLemmatizer
import pandas as pd
import numpy as np
from nltk.corpus import stopwords
import re
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.model_selection import train_test_split
import operator
from collections import Counter
from sklearn.metrics import accuracy_score
#from textblob import TextBlob
#nltk.download('punkt')

# Step 1 - Bag of words concept and process wo
stemmer = PorterStemmer()
lemmatizer = WordNetLemmatizer()
stop_words = set(stopwords.words('english'))
def StemTokenizer(tokens):
	return [stemmer.stem(token) for token in tokens]

def Stemmer(text):
    word_tokens = word_tokenize(text.lower())
    filtered_sentence = [w for w in word_tokens if not w in stop_words]
    return StemTokenizer(filtered_sentence)

def lemmaTokenizer(tokens):
    return [lemmatizer.lemmatize(token) for token in tokens]

def lemmatize(text):
    word_tokens = word_tokenize(text.lower())
    filtered_sentence = [w for w in word_tokens if not w in stop_words]
    return lemmaTokenizer(filtered_sentence)

class KNNClassifier():
    def fit(self, dtrain, ltrain):
        self.dtrain = dtrain
        self.ltrain = ltrain
        self.vec_train = vec_tfidf.fit_transform(dtrain)

    def predict(self, data_test, k):
        vec_test = vec_tfidf.transform(data_test)
        #print(vec_test.shape)
        cos_sim = cosine_similarity(self.vec_train, vec_test)
        tcos_sim = np.transpose(cos_sim)
        neighbours = []
        for val in tcos_sim:
            distances = []
            for x in range(len(val)):
                distances.append([x, val[x]])
            distances.sort(key=operator.itemgetter(1),reverse=True)
            neighbour = []
            print(distances[1][0])
            for x in range(k):
                rowNo = distances[x][0]
                #neighbour.append([self.ltrain[rowNo], distances[x][1] ])
                neighbour.append(int(self.ltrain[rowNo]));
            #print(neighbour)
            counter=Counter(neighbour)
            neighbours.append(counter.most_common(1)[0][0])
            print(counter)
        return neighbours


def Load_text(x):
    text=[]
    label=[]
    counter=1
    with open(x) as f:
        for line in f:
            if counter==0:
                counter=counter+1
                continue
            line.strip()
            label.append(int(line[:1]))
            text.append(line[2:])
    return text,label


def Load_test(x):
    text=[]
    counter=1
    with open(x) as f:
        for line in f:
            if counter==0:
                counter=counter+1
                continue
            line.strip()       
            text.append(line)
    return text

#1 load train text data
print("loading train data....")
data_train,lable_train=Load_text('train.txt')

#2 load test data
print("loading test data....")
test_data = Load_test("test.txt")
#X_train, X_test, y_train, y_test = train_test_split(data_train, lable_train, test_size=0.2)

#3 initialize Vectorizer
vec_tfidf = TfidfVectorizer(tokenizer=Stemmer ,sublinear_tf = True, min_df = 0.005, stop_words = 'english' )
#vec_tfidf = TfidfVectorizer(tokenizer=lemmatize, stop_words = 'english' )

#4 initialize KNNClassifier
print("initializing classifier")
classifier = KNNClassifier()
classifier.fit(data_train, lable_train)
print("prediction starting.....")
result = classifier.predict(test_data, 11)
print("writing result into file...")
np.savetxt('final1output.txt', result, delimiter='; ')
print("prediction completed....xxxxx")
#accuracy = accuracy_score(y_test, result)
#print(accuracy)
