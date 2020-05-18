
# coding: utf-8




import pandas as pd
import glob
from nltk.corpus import stopwords 
from nltk.stem.porter import PorterStemmer
import numpy as np
from nltk.tokenize import RegexpTokenizer
from gensim.models.word2vec import Word2Vec





pos_review=(glob.glob("E:\\파이썬으로텍스트마이닝분석\\데이터\\aclImdb\\train\\pos\\*.txt"))[0:100]





lines_pos=[]
for i in pos_review:
    try:
        f = open(i, 'r')
        temp = f.readlines()[0]
        lines_pos.append(temp)
        f.close()
    except Exception as e:
        continue





len(lines_pos)





stop_words = stopwords.words('english')





tokenizer = RegexpTokenizer('[\w]+')





text=[]
for line in lines_pos:
    words =  line.lower()
    tokens = tokenizer.tokenize(words)
    stopped_tokens = [i for i in list(set(tokens)) if not i in stop_words+["br"]]
    stopped_tokens2 = [i for i in stopped_tokens if len(i)>1]
    text.append(stopped_tokens2)




model = Word2Vec(text, sg=1, window=2, min_count=3)





model.init_sims(replace=True)





model.wv.similarity('film', 'movie')





model.wv.most_similar("good",topn =5)





len(model.wv.index2word)





model.wv.index2word[0:5]

