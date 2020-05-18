
# coding: utf-8



import pandas as pd
import glob
from afinn import Afinn
from nltk.corpus import stopwords 
from nltk.stem.porter import PorterStemmer
from nltk.tokenize import RegexpTokenizer
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from scipy import sparse
import matplotlib.pyplot as plt





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




tokenizer = RegexpTokenizer('[\w]+')





stop_words = stopwords.words('english')





vec = TfidfVectorizer(stop_words=stop_words)





vector_lines_pos = vec.fit_transform(lines_pos)





A=vector_lines_pos.toarray()





A.shape





A=A.transpose()





A.shape





A_sparse = sparse.csr_matrix(A)





similarities_sparse = cosine_similarity(A_sparse,dense_output=False)





list(similarities_sparse.todok().items())[35000:35010]





vec.get_feature_names()[100:105]





vec.get_feature_names()[1469]





vec.get_feature_names()[108]





df=pd.DataFrame(list(similarities_sparse.todok().items()),columns=["words","weight"])





df2=df.sort_values(by=['weight'],ascending=False)





df2=df2.reset_index(drop=True)





df3=df2.loc[np.round(df2['weight']) < 1]





df3=df3.reset_index(drop=True)





df3.head(10)

