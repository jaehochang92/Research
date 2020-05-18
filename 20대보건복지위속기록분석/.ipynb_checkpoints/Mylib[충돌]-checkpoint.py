import os, re, doctest
from datetime import datetime

## KonlPy.Kkma + text analysis
from konlpy.tag import Kkma, Hannanum
## For data preprocessing
import pandas as pd
import numpy as np
## Utilities
import gc, time
from tqdm import tqdm as tqdm_base
# First follow instructions on 
# https://ipywidgets.readthedocs.io/en/latest/user_install.html#installing-the-jupyterlab-extension
from termcolor import colored
## Text handling
from wordcloud import WordCloud
from collections import Counter
import gensim as gs
## Visualization
import pyLDAvis.gensim as gsvis
import pyLDAvis
from mpl_toolkits import mplot3d
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
# import gexf
## Machines
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.decomposition import LatentDirichletAllocation
## For data handling
import pandas as pd
import pickle
# import datatable as dt
import numpy as np

def mMscl(list):
    if len(np.unique(list)) > 1:
        list -= min(list)
        den = max(list) - min(list)
        return(list/den+1)
    else:
        return(list)
    
def tqdm(*args, **kwargs):
    if hasattr(tqdm_base, '_instances'):
        for instance in list(tqdm_base._instances):
            tqdm_base._decr_instances(instance)
    return tqdm_base(*args, **kwargs)
    
def lngrd(l, m, M): # f(x)=a*exp(b*x)
    b = np.log(M/m)/(l-1)
    a = m/np.exp(b)
    return([a*np.exp(b*x) for x in range(1, l+1)])
    
def gcol() : 
    stop = False
    i = 0
    while stop == False :
        i = i+1
#         print('Collecting %d ...' % i)
        n = gc.collect()
#         print('Unreachable objects :', n)
#         print('Remaining Garbage :', gc.garbage)
        if n == 0 :
            stop = True

# Tokenizers  
def get_noun(msg_txt):
    """
    Return the nouns of string.
    >>> get_noun('창의적 사고')
    ['창의', '사고']
    """
    kkma = Kkma()
    nouns = list()
    pattern = re.compile("[ㄱ-ㅎㅏ-ㅣ]+")
#     for dumpstr in ['보건복지위원회', '보건복지', '자유한국당'] : # remove trivial words
#         msg_txt = re.sub(dumpstr, "", msg_txt)
    msg_txt = re.sub(pattern, "", msg_txt).strip()

    if len(msg_txt) > 0:
        pos = kkma.pos(msg_txt)
        for keyword, typeof in pos:
            # 고유명사 또는 보통명사
            if typeof == "NNG" or typeof == "NNP":
                nouns.append(keyword)
    return nouns

def get_all_token(msg_txt):
    """
    Return all tokens of string.
    >>> get_all_token('창의적 사고')
    ['창의', '적', '사고']
    """
    kkma = Kkma()
    nouns = list()

    if len(msg_txt) > 0:
        pos = kkma.pos(msg_txt)
        for keyword, typeof in pos:
            nouns.append(keyword)
        
    return nouns            
            
# Preparing Data
class prpr_data:
    def __init__(self, data) :
        self.data = data
    
    def by_speaker(self, speaker) : # Input must be lists!
        idx2 = self.data['SPEAKER'].isin(speaker)
        filtered_data = self.data[idx2]
        return prpr_data(filtered_data)

    def by_meeting(self, meeting) : # Input must be [lower,upper]!
        idx3 = self.data['MEETING_NAME'].apply(lambda x: x[1:4]).isin([str(i) for i in np.arange(meeting[0], meeting[1]+1)])
        filtered_data = self.data[idx3]
        return prpr_data(filtered_data)
    
    def get_data(self) :
        return self.data
    
def get_corpus_by_group(raw_data, 회차, gv = 'PARTY'):
    groups = raw_data[gv].unique()
    merged_tokens_by_group, tokens_by_group = [], []
    for cr in tqdm(groups, gv):
        if pd.isna(cr):
            crd = raw_data.loc[pd.isna(raw_data[gv])]
        else:
            crd = raw_data.loc[raw_data[gv] == cr]
        crd = prpr_data(crd).by_meeting(회차).get_data()
        temp = []
        for i in crd.token:
            temp += i
        tokens_by_group.append(temp)
        for j in crd.token.keys():
            crd.loc[j, 'token'] = ' '.join(crd.loc[j, 'token'])
        merged_tokens_by_group.append(' '.join(crd.token))
    print(colored('\nTokens by Group\n', 'red'))
    [print(groups[i],len(tokens_by_group[i])) for i in range(len(tokens_by_group))]
    print(colored('\nMerged Tokens by Group\n', 'red'))
    [print(groups[i],len(merged_tokens_by_group[i])) for i in range(len(merged_tokens_by_group))]
    gcol()
    return(groups, merged_tokens_by_group, tokens_by_group)

class doLDA:
    def __init__(self, tokend_docs, use_tfidf, top_n_words = 10):
        self.tokend_docs = tokend_docs
        self.dictionary = gs.corpora.Dictionary(self.tokend_docs)
        self.use_tfidf = use_tfidf
        self.top_n_words = top_n_words
#         count = 0
#         for k, v in self.dictionary.iteritems():
#             print(k, v)
#             count += 1
#             if count > 10:
#                 break
                
    def get_bow(self, no_below, no_above):
        self.dictionary.filter_extremes(no_below, no_above)
        self.bow_corpus = [self.dictionary.doc2bow(doc) for doc in self.tokend_docs]
    
    def fit_model(self, num_topics, alp, bt):
        rndst=2020
        chnksz=2e3
        pss=10
        alpha, beta = alp, bt
        if self.use_tfidf is False:
            lda_model = gs.models.LdaMulticore(corpus=self.bow_corpus, num_topics=num_topics, id2word=self.dictionary,
                                               random_state=rndst, chunksize=chnksz, passes=pss, alpha=alp, eta=bt)
        if self.use_tfidf is True:
            self.tfidf = gs.models.TfidfModel(self.bow_corpus)
            self.bow_corpus = self.tfidf[self.bow_corpus]
            lda_model = gs.models.LdaMulticore(corpus=self.bow_corpus, num_topics=num_topics, id2word=self.dictionary,
                                              random_state=rndst, chunksize=chnksz, passes=pss, alpha=alp, eta=bt)
#         For Topic printing
        
        terms = [','.join(re.split('  ', re.sub("[*+0-9.\"]", "", terms))) for _, terms in lda_model.print_topics(num_topics, self.top_n_words)]
#         terms = [re.split('  ', re.sub("[*+0-9.\"]", "", terms)) for _, terms in lda_model.print_topics(num_topics, self.top_n_words)]
        terms = [{'Terms' : terms[ti]} for ti in range(len(terms))]
#         terms = [{'Term{}'.format(i+1) : terms[ti][i] for i in range(self.top_n_words)} for ti in range(len(terms))]
        tv = [{'Topic' : ti+1} for ti in range(lda_model.num_topics)]
        [tv[ti].update(terms[ti]) for ti in range(len(tv))]
        logperpl = lda_model.log_perplexity(self.bow_corpus)
        coher = gs.models.CoherenceModel(lda_model, texts=self.tokend_docs, dictionary= self.dictionary).get_coherence()
        return(lda_model, {'-log(Perplexity)':-logperpl, 'Coherence':coher}, pd.DataFrame(tv))

def prprwrf(ax, ntl, scores, set_labels = False):
    sDF = pd.DataFrame(scores)
    x = np.array([[e for e in sDF.loc[sDF.ntopic == nt, '-log(Perplexity)']] for nt in ntl])
    y = np.array([[e for e in sDF.loc[sDF.ntopic == nt, 'Coherence']] for nt in ntl])
    z = np.array([[e for e in sDF.loc[sDF.ntopic == nt, 'Beta']] for nt in ntl])
    if set_labels is True:
        ax.set_xlabel('-log(Perplexity)')
        ax.set_ylabel('Coherence')
        ax.set_zlabel('Beta')
    return(ax, x, y, z)
    
def LDA_tuning(inst_doLDA, weight_on_coher, ntopics, hypars): # inst_doLDA should have bow_corpus.
    """
    doclist = pd.read_pickle('./data/sample_doc.pickle')
    inst_doLDA = doLDA(doclist, use_tfidf=False)
    inst_doLDA.get_bow(no_below = 0, no_above = 1)
    # inst_doLDA.fit_model(3, 'asymmetric', .5)
    eps, weight_on_coher, ntopics = np.exp(-10), 0., range(5, 1-1, -1)
    hypars = {'Alpha':['asymmetric'],'Beta':lngrd(l=10, m=eps, M=1-eps)}
    _,topicDF,_ = LDA_tuning(inst_doLDA, weight_on_coher, ntopics, hypars)
    topicDF
    """
    # Tuning Process
    models, topicDF = [], []
    scores = {'ntopic':[], '-log(Perplexity)':[], 'Coherence':[], 'Beta':[], 'Alpha':[]}
#     tmstmp = datetime.now().strftime("%y%m%d;%H%M")
    fig1, ax1 = plt.subplots(1, 1, figsize=(15, 12), subplot_kw={'projection': '3d'})
    fig2, ax2 = plt.subplots(1, 1, figsize=(15, 12), subplot_kw={'projection': '3d'})
    for idx, nt in enumerate(ntopics):
        for bt in tqdm(hypars['Beta'], 'Beta'):
            m,pc,tDF = inst_doLDA.fit_model(nt, hypars['Alpha'][0], bt)
            if (idx > 0):
                if (pc['Coherence'] <= max(scores['Coherence'])):
                    print('nt = {}, bt = {} skipped!'.format(nt,bt))
                    next
            pc['ntopic'], pc['Alpha'], pc['Beta'] = nt, hypars['Alpha'], bt
            models.append(m);topicDF.append(tDF)
            scores = {'{}'.format(k):scores[k]+[pc[k]] for k in scores.keys()}
            del m,pc,tDF
            ax2, x, y, z = prprwrf(ax2, [nt], scores, set_labels=True)
            ax2.plot_wireframe(x, y, z, color = plt.cm.rainbow(nt/max(ntopics)))
            fig2.savefig('./result/innerPlot_Tuning_State.png', dpi = 300)
        ax1, x, y, z = prprwrf(ax1, ntopics[0:idx+1], scores, set_labels=True)
        ax1.plot_wireframe(x, y, z, color = plt.cm.rainbow(np.linspace(0, 1)))
        fig1.savefig('./result/outerPlot_Tuning_State.png', dpi = 300)
        agg = np.array([mMscl(l) for l in [scores[k] for k in list(scores.keys())[1:3]]])
        agg = np.average(agg, axis=0, weights=[1-weight_on_coher, weight_on_coher]) # last axis
        optset = {'{}'.format(k):scores[k][np.argmax(agg)] for k in scores.keys()}
        print('\nOptimal set of parameters so far (nt={}): \n{}'.format(nt, {'{}'.format(k,nt):round(optset[k],4) for k in optset.keys() if k is not 'Alpha'}))
    flda = models[np.argmax(agg)]
    gcol()
    return(flda, topicDF[np.argmax(agg)], optset)

def LDAvis(lda_fit, doLDA_instance, dirctr):
    forgsvis = gsvis.prepare(lda_fit, doLDA_instance.bow_corpus, doLDA_instance.dictionary) # Take long time
    pyLDAvis.display(forgsvis)
    filename = dirctr + '/' + str(lda_fit.num_topics) + '_pyLDAvis.pickle'
    with open(filename, 'wb') as data:
        pickle.dump(forgsvis, data, protocol=pickle.HIGHEST_PROTOCOL)
    pyLDAvis.save_html(forgsvis, dirctr + '/' + str(lda_fit.num_topics) + '_pyLDAvis.html')
    
print("==============\nimport done!\n==============")






# Deprecated.1
# plt.figure(figsize=(18, 10))
# for ki in range(len(scores.keys())):
#     k = scores.keys()[ki]
#     sc = scores[k]
#     plt.subplot(1, len(keys), ki+1)
#     plt.grid(True)
#     plt.plot(np.arange(min(), nt), sc, '--');plt.plot(ntopics, sc, 'ro')
#     plt.title('{}, {}-topics is optimal'.format(k,str(ntopics[np.argmax(sc)])))
#     plt.xlabel('No. of Topics');plt.ylabel(k)
# plt.savefig('./result/' + tmstmp + '_tuning_scores.png')