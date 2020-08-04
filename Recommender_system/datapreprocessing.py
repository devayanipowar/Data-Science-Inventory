# -*- coding: utf-8 -*-
"""
Created on Sun Nov 25 12:12:46 2018

@author: Owner
"""

import pandas as pd
import numpy as np
import chardet
from sklearn.feature_extraction.text import CountVectorizer
def ReadMovieFileGroupAsList(filename, colums):    
    df_data = ReadMovieFile(filename, colums)
    df_data['movieID'] = df_data['movieID'] .astype('float')
    return df_data.groupby('movieID').agg(lambda x: x.tolist())

def ReadMovieFileGroupAsString(filename, colums):    
    df_data = ReadMovieFile(filename, colums)
    df_data['movieID'] = df_data['movieID'] .astype('float')
    return df_data.groupby('movieID').agg(lambda x: "%s" % ' '.join(x))   

def ReadMovieFile(filename, colums):
    with open(filename, 'rb') as f:
        result = chardet.detect(f.read())
    return pd.read_csv(filename, sep='\s+', usecols=colums, encoding=result['encoding'])
    
def space_tokenizer(s):
   return s.split(' ')
df = pd.read_csv('train.dat',sep='\s+')
dftest = pd.read_csv('test.dat',sep='\s+')
#df2 = pd.read_csv('movie_actors.dat',sep='\s+',encoding='Latin-1')
#df4 = pd.read_csv('movie_tags.dat',sep='\s+')
#df_mtags = pd.read_csv('movie_tags.dat', sep= '\s+')
#utags = pd.read_csv('user_taggedmovies.dat',encoding='Latin-1')
'''
df_tags = ReadMovieFile("movie_tags.dat", ["movieID", "tagID", "tagWeight"])
df_tag_data = ReadMovieFile("tags.dat", ["id", "value"])
df_tags['tagID'] = df_tags['tagID'] .astype('float')
df_tag_data['id'] = df_tag_data['id'] .astype('float')
df_tags = df_tags.merge(df_tag_data, left_on="tagID", right_on="id")

df_tags_minor = df_tags.drop(["tagID", "tagWeight", "id"], axis=1).groupby('movieID').agg(lambda x: "%s" % ' '.join(x))
cv = CountVectorizer(tokenizer=space_tokenizer )
vec_tag_matrix = cv.fit_transform(df_tags_minor['value']).toarray()
vec_tag_data = pd.DataFrame(vec_tag_matrix, columns=cv.get_feature_names())
vec_tag_data["movieID"] = df_tags_minor.index.get_values()
vec_tag_data.reset_index(drop=True)    
vec_tag_data.set_index('movieID', inplace=True)
'''
#Fill in missing data of numerical columns
print("Reading and transforming data for Genre...")
df_genre = ReadMovieFileGroupAsString("movie_genres.dat", ["movieID", "genre"])
cv = CountVectorizer(tokenizer=space_tokenizer)
vec_genre_matrix = cv.fit_transform(df_genre['genre']).toarray()
vec_genre_data = pd.DataFrame(vec_genre_matrix, columns=cv.get_feature_names())
vec_genre_data["movieID"] = df_genre.index.get_values()
vec_genre_data.reset_index(drop=True)    
vec_genre_data.set_index('movieID', inplace=True)
vec_genre_data['movieID'] = vec_genre_data.index.values
new = df.merge(vec_genre_data,on = 'movieID')
test_new = dftest.merge(vec_genre_data,on = 'movieID')

rating = new['rating']
new = new.drop('rating', axis=1)
new = new.join(rating)
df_Arr = np.array(new)
df_Arr_test = np.array(test_new)

df_Arr = df_Arr.astype(np.float)
df_Arr_test = df_Arr_test.astype(np.float)

ratings = df_Arr[:,-1]
#D_Arr = scale(D_Arr)
#D_Arr_test = scale(D_Arr_test)

np.savetxt('preprocessedtrain.txt',df_Arr)
np.savetxt('preprocessedtest.txt',df_Arr_test)
print ('Generated text file of preprocessed dataset')

np.savetxt('rating.txt',rating)
