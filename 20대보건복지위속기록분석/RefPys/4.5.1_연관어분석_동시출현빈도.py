
# coding: utf-8



import pandas as pd
import glob
from afinn import Afinn
from nltk.corpus import stopwords 
from nltk.stem.porter import PorterStemmer
from nltk.tokenize import RegexpTokenizer
import numpy as np
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





count = {}   #동시출현 빈도가 저장될 dict
for line in lines_pos:
    words =  line.lower()
    tokens = tokenizer.tokenize(words)
    stopped_tokens = [i for i in list(set(tokens)) if not i in stop_words+["br"]]
    stopped_tokens2 = [i for i in stopped_tokens if len(i)>1]
    for i, a in enumerate(stopped_tokens2):
        for b in stopped_tokens2[i+1:]:
            if a>b: 
                count[b, a] = count.get((b, a),0) + 1  
            else :
                count[a, b] = count.get((a, b),0) + 1     





df=pd.DataFrame.from_dict(count, orient='index')





list1=[]
for i in range(len(df)):
    list1.append([df.index[i][0],df.index[i][1],df[0][i]])





df2=pd.DataFrame(list1, columns=["term1","term2","freq"])





df3=df2.sort_values(by=['freq'],ascending=False)





df3_pos=df3.reset_index(drop=True)





df3_pos.head(20)





class MakeGraphml:

    def make_graphml(self, pair_file, graphml_file):
        out = open(graphml_file, 'w', encoding = 'utf-8')

        entity = []
        e_dict = {}
        count = []
        for i in range(len(pair_file)):
            e1 = pair_file.iloc[i,0]
            e2 = pair_file.iloc[i,1]
            #frq = ((word_dict[e1], word_dict[e2]),  pair.split('\t')[2])
            frq = ((e1, e2), pair_file.iloc[i,2])
            if frq not in count: count.append(frq)   # ((a, b), frq)
            if e1 not in entity: entity.append(e1)
            if e2 not in entity: entity.append(e2)
        print('# terms: %s'% len(entity))

      
        for i, w in enumerate(entity):
            e_dict[w] = i + 1 # {word: id}

        out.write(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlnshttp://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">" +
            "<key id=\"d1\" for=\"edge\" attr.name=\"weight\" attr.type=\"double\"/>" +
            "<key id=\"d0\" for=\"node\" attr.name=\"label\" attr.type=\"string\"/>" +
            "<graph id=\"Entity\" edgedefault=\"undirected\">" + "\n")

        # nodes
        for i in entity:
            out.write("<node id=\"" + str(e_dict[i]) +"\">" + "\n")
            out.write("<data key=\"d0\">" + i + "</data>" + "\n")
            out.write("</node>")

        # edges
        for y in range(len(count)):
            out.write("<edge source=\"" + str(e_dict[count[y][0][0]]) + "\" target=\"" + str(e_dict[count[y][0][1]]) + "\">" + "\n")
            out.write("<data key=\"d1\">" + str(count[y][1]) + "</data>" + "\n")
            out.write("</edge>")

        out.write("</graph> </graphml>")
        print('now you can see %s' % graphml_file)

        out.close()





gm = MakeGraphml()





graphml_file = '연관어네트워크.graphml'





gm.make_graphml(df3_pos.iloc[0:(len(np.where(df3_pos['freq']>10)[0])),:], graphml_file)





neg_review=(glob.glob("E:\\파이썬으로텍스트마이닝분석\\데이터\\aclImdb\\train\\neg\\*.txt"))[0:100]





lines_neg=[]
for i in neg_review:
    try:
        f = open(i, 'r')
        temp = f.readlines()[0]
        lines_neg.append(temp)
        f.close()
    except Exception as e:
        continue





len(lines_neg)





count = {}   #동시출현 빈도가 저장될 dict
for line in lines_neg:
    words =  line.lower()
    tokens = tokenizer.tokenize(words)
    stopped_tokens = [i for i in list(set(tokens)) if not i in stop_words+["br"]]
    stopped_tokens2 = [i for i in stopped_tokens if len(i)>1]
    for i, a in enumerate(stopped_tokens2):
        for b in stopped_tokens2[i+1:]:
            if a>b: 
                count[b, a] = count.get((b, a),0) + 1  
            else :
                count[a, b] = count.get((a, b),0) + 1 





df=pd.DataFrame.from_dict(count, orient='index')





list1=[]
for i in range(len(df)):
    list1.append([df.index[i][0],df.index[i][1],df[0][i]])





df2=pd.DataFrame(list1, columns=["term1","term2","freq"])





df3=df2.sort_values(by=['freq'],ascending=False)





df3_neg=df3.reset_index(drop=True)




df3_neg.head(20)





import networkx as nx
import operator





G_pos=nx.Graph()





for i in range((len(np.where(df3_pos['freq']>10)[0]))):
    G_pos.add_edge(df3_pos['term1'][i], df3_pos['term2'][i], weight=int(df3_pos['freq'][i]))





dgr = nx.degree_centrality(G_pos)
btw = nx.betweenness_centrality(G_pos)
cls = nx.closeness_centrality(G_pos)
egv = nx.eigenvector_centrality(G_pos)





sorted_dgr = sorted(dgr.items(), key=operator.itemgetter(1), reverse=True)
sorted_btw = sorted(btw.items(), key=operator.itemgetter(1), reverse=True)
sorted_cls = sorted(cls.items(), key=operator.itemgetter(1), reverse=True)
sorted_egv = sorted(egv.items(), key=operator.itemgetter(1), reverse=True)





print("** degree **")
for x in range(10):
    print(sorted_dgr[x])

print("** betweenness **")
for x in range(10):
    print(sorted_btw[x])

print("** closeness **")
for x in range(10):
    print(sorted_cls[x])
    
print("** eigenvector **")
for x in range(10):
    print(sorted_egv[x])





G_neg=nx.Graph()





for i in range((len(np.where(df3_neg['freq']>10)[0]))):
    G_neg.add_edge(df3_neg['term1'][i], df3_neg['term2'][i], weight=int(df3_neg['freq'][i]))





dgr = nx.degree_centrality(G_neg)
btw = nx.betweenness_centrality(G_neg)
cls = nx.closeness_centrality(G_neg)
egv = nx.eigenvector_centrality(G_neg)





sorted_dgr = sorted(dgr.items(), key=operator.itemgetter(1), reverse=True)
sorted_btw = sorted(btw.items(), key=operator.itemgetter(1), reverse=True)
sorted_cls = sorted(cls.items(), key=operator.itemgetter(1), reverse=True)
sorted_egv = sorted(egv.items(), key=operator.itemgetter(1), reverse=True)





print("** degree **")
for x in range(10):
    print(sorted_dgr[x])

print("** betweenness **")
for x in range(10):
    print(sorted_btw[x])

print("** closeness **")
for x in range(10):
    print(sorted_cls[x])
    
print("** eigenvector **")
for x in range(10):
    print(sorted_egv[x])

