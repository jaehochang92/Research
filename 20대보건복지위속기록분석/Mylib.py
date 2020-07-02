print('Start loading Mylib...')

from Utilities import *

'TEXT ANALYSIS & MODELING========================================================================='
'Tokenization'
from konlpy.tag import Mecab

'Text Analysis'
from wordcloud import WordCloud
import gensim as gs

'Sklearn'
# from sklearn.decomposition import LatentDirichletAllocation, TruncatedSVD
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.preprocessing import MinMaxScaler

# from sklearn.model_selection import GridSearchCV
# from scipy.cluster import hierarchy
# from scipy.spatial import distance
# from sklearn.metrics.pairwise import cosine_similarity

'VISUALIZATION===================================================================================='
'Text networks'
# import networkx as nx
# try:
#     import pygraphviz
#     from networkx.drawing.nx_agraph import graphviz_layout
# except ImportError:
#     try:
#         import pydot
#         from networkx.drawing.nx_pydot import graphviz_layout
#     except ImportError:
#         raise ImportError("This example needs Graphviz and either "
#                           "PyGraphviz or pydot")
# import gexf

'Model visualization'
import pyLDAvis.gensim as gsvis
import pyLDAvis
from mpl_toolkits import mplot3d
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm


##################################################################################################

# Tokenizer

def get_noun(string, rms=None):
    """
    Returns the nouns of string.
    >>> get_noun('창의적 사고', '창의적')
    ['사고']
    """
    if rms:
        for rm in rms:  # remove trivial words
            string = re.sub(rm, "", string)
    pattern = re.compile("[ㄱ-ㅎㅏ-ㅣ]+")
    string = re.sub(pattern, "", string).strip()

    mecab = Mecab()
    nouns = list()
    if len(string) > 0:
        pos = mecab.pos(string)
        for keyword, typeof in pos:
            # 고유명사 또는 보통명사
            if typeof == "NNG" or typeof == "NNP":
                nouns.append(keyword)
    return nouns


def make_ngram(data, ngram, mincnt, thrsh):
    if ngram > 1:
        kgram = gs.models.Phrases(data, min_count=mincnt, threshold=thrsh)  # higher threshold fewer phrases.
        if ngram > 2:
            cgram = ngram
            while cgram > 2:
                kgram = gs.models.Phrases(kgram[data], threshold=thrsh)
                cgram -= 1
        kgram_mod = gs.models.phrases.Phraser(kgram)
        return ([kgram_mod[d] for d in data])
    else:
        return (data)


# LDAs

class doLDA:
    def __init__(self, tokend_docs, use_tfidf, top_n_words=10):
        self.tokend_docs = tokend_docs
        self.dictionary = gs.corpora.Dictionary(self.tokend_docs)
        self.use_tfidf = use_tfidf
        self.top_n_words = top_n_words

    def get_bow(self, no_below, no_above):
        self.dictionary.filter_extremes(no_below, no_above)
        self.bow_corpus = [self.dictionary.doc2bow(doc) for doc in self.tokend_docs]

    def fit_model(self, num_topics, alp, bt, rndst, iters):
        chnksz = 2e3
        pss = 100
        alpha, beta = alp, bt

        if self.use_tfidf:
            self.tfidf = gs.models.TfidfModel(self.bow_corpus)
            self.bow_corpus = self.tfidf[self.bow_corpus]

        lda_model = gs.models.LdaMulticore(corpus=self.bow_corpus, num_topics=num_topics, id2word=self.dictionary,
                                           random_state=rndst, chunksize=chnksz, passes=pss, alpha=alp, eta=bt,
                                           iterations=iters)
        #         For Topic printing

        terms = [','.join(re.split('  ', re.sub("[*+0-9.\"]", "", terms))) for _, terms in
                 lda_model.print_topics(num_topics, self.top_n_words)]
        terms = [{'Terms': terms[ti]} for ti in range(len(terms))]
        tv = [{'Topic': ti + 1} for ti in range(lda_model.num_topics)]
        [tv[ti].update(terms[ti]) for ti in range(len(tv))]
        logperpl = lda_model.log_perplexity(self.bow_corpus)
        coher = gs.models.CoherenceModel(lda_model, texts=self.tokend_docs, dictionary=self.dictionary).get_coherence()
        return lda_model, {'neglp': -logperpl, 'coherence': coher}, pd.DataFrame(tv)


class plotTuning:
    def __init__(self, xyz_labels):
        self.xyz_labels = xyz_labels  # x is a coloring value for Vis3_surf
        self.fig, self.ax = plt.subplots(1, 2, figsize=(15, 6), subplot_kw={'projection': '3d'})
        [axi.set_xlabel(self.xyz_labels[0]) for axi in self.ax]
        [axi.set_ylabel(self.xyz_labels[1]) for axi in self.ax]
        [axi.set_zlabel(self.xyz_labels[2]) for axi in self.ax]

    def prepare_data_for_plT(self, ntl, scores):
        self.ntl = ntl
        self.scores = scores
        sDF = pd.DataFrame(self.scores)
        self.x = np.array([[e for e in sDF.loc[sDF.ntopic == nt, self.xyz_labels[0]]] for nt in ntl])
        self.y = np.array([[e for e in sDF.loc[sDF.ntopic == nt, self.xyz_labels[1]]] for nt in ntl])
        self.z = np.array([[e for e in sDF.loc[sDF.ntopic == nt, self.xyz_labels[2]]] for nt in ntl])

    def wireframe3D(self, clrs):
        self.ax[0].plot_wireframe(self.x, self.y, self.z, color=clrs)
        self.fig.savefig('./result/Plot_Tuning_State.png')

    def surface3D(self):
        K = len(self.x[0])
        colors = plt.cm.Greens([np.linspace(.4, 1, num=K) for i in self.x])  # adjust palette here!
        rcount, ccount, _ = colors.shape
        self.ax[1].plot_surface(self.x, self.y, self.z, facecolors=colors, rcount=rcount, ccount=ccount, shade=False)
        self.ax[1].set_facecolor((0, 0, 0, 0))
        self.fig.savefig('./result/Plot_Tuning_State.png')


def LDA_tuning(inst_doLDA, weight_on_coher, ntopics, hypars, rand, iters=10 ** 2, verb=False):
    #     inst_doLDA should have bow_corpus.
    #     Must use beta of increasing order.
    """
    tkns = pd.read_pickle('./data/sample_doc.pickle')
    tkns = [d for d in tkns.values()][1]
    inst = doLDA([tkns], False, 15)
    inst.get_bow(0, 1)

    flda, topicDF, optparset = LDA_tuning(inst_doLDA=inst, weight_on_coher=1., ntopics= range(4, 15),
                                          hypars = {'alpha':'asymmetric', 'beta':['auto']}, rand=1954,
                                          iters = 200)
    plt.close()
    results = {'project':'이재명인터뷰', 'flda':flda, 'inst':inst, 'optparset':optparset,
               'ngram':1, 'random_seed':3920, 'mkdir':True, 'write':False, 'ldavis':True}
    tmstmp = save_results(**results)
    """
    # Tuning Process
    models, topicDF = [], []
    ntl = []
    TSplt = plotTuning(xyz_labels=['beta', 'coherence', 'neglp'])
    outscores = {'ntopic': [], 'neglp': [], 'coherence': [], 'beta': []}
    #     Topic Level==========================================================================================
    if verb:
        ntopics = tqdm(ntopics)
    for idx, nt in enumerate(ntopics):
        inscores = {'ntopic': [], 'neglp': [], 'coherence': [], 'beta': []}
        #         Beta Level==========================================================================
        for bt in hypars['beta']:
            m, pc, tDF = inst_doLDA.fit_model(nt, hypars['alpha'], bt, rndst=rand, iters=iters)
            pc.update({'ntopic': nt, 'beta': bt})
            my_dict(inscores).appnd(pc)
            if all([type(i) is not str for i in hypars['beta']]):
                TSplt.prepare_data_for_plT([nt], inscores)
                TSplt.wireframe3D(plt.cm.rainbow(nt / max(ntopics)))
            if idx is 0:
                models.append(m);
                topicDF.append(tDF)
            else:
                if (max(inscores['coherence']) <= max(outscores['coherence'])):
                    models.append(m);
                    topicDF.append(tDF)
                else:
                    models.append(m);
                    topicDF.append(tDF)
        #         Beta Level==========================================================================
        my_dict(outscores).appnd(inscores)
        ntl.append(nt)
        agg = np.array([mMscl(l) for l in [outscores[k] for k in list(outscores.keys())[1:3]]])
        agg = np.average(agg, axis=0, weights=[1 - weight_on_coher, weight_on_coher])  # last axis
        optset = {'{}'.format(k): outscores[k][np.argmax(agg)] for k in outscores.keys()}
        #         print('\nOptimal set of parameters so far (nt={}): \n{}'.
        #               format(nt, {'{}'.format(k,nt):round(optset[k],4) for k in optset.keys() if type(optset[k]) is not str}))
        if len(ntl) > 1 and all([type(i) is not str for i in hypars['beta']]):
            TSplt.prepare_data_for_plT(ntl, outscores)
            TSplt.surface3D()
    #     Topic Level===========================================================================================
    plt.plot(ntopics, outscores['coherence'], 'b--')
    plt.plot(ntopics, outscores['coherence'], 'ro')
    flda = models[np.argmax(agg)]
    optset.update({'alpha': hypars['alpha']})
    return (flda, topicDF[np.argmax(agg)], optset)


def LDAvis(lda_fit, doLDA_instance, dirctr):
    forgsvis = gsvis.prepare(lda_fit, doLDA_instance.bow_corpus, doLDA_instance.dictionary)  # Take long time
    pyLDAvis.display(forgsvis)
    filename = dirctr + '/' + str(lda_fit.num_topics) + '_pyLDAvis.pickle'
    with open(filename, 'wb') as data:
        pickle.dump(forgsvis, data, protocol=pickle.HIGHEST_PROTOCOL)
    pyLDAvis.save_html(forgsvis, dirctr + '/' + str(lda_fit.num_topics) + '_pyLDAvis.html')


def fitdocs(inst, flda, dict_of_corps, agg=False):
    doc_topics = {'{}'.format(p): Counter(
        [(sorted(flda.get_document_topics(inst.dictionary.doc2bow(d)), key=operator.itemgetter(1))[-1][0] + 1)
         for d in c])
        for p, c in dict_of_corps.items()}
    tops = [d for c in dict_of_corps.values() for d in c]
    doc_topics['Total'] = Counter(
        [(sorted(flda.get_document_topics(inst.dictionary.doc2bow(d)), key=operator.itemgetter(1))[-1][0] + 1)
         for d in tops])
    ls = [sum(doc_topics[d].values()) for d in doc_topics]
    tops = [sorted(doc_topics[p].items(), key=operator.itemgetter(1), reverse=True) for p in doc_topics]
    tops = [t[:5] for t in tops]
    tops = [[str((ti[0], round(ti[1] / li * 100, 2))) for ti in t] for t, li in zip(tops, ls)]
    tops = pd.DataFrame(tops, index=doc_topics.keys()).T
    tops = tops[[tops.columns[-1]] + tops.columns[:-1].to_list()]
    if not agg:
        tops = tops.drop(['Total'], 1)
    return (tops)


def save_results(project, flda, inst, optparset, ngram, random_seed, mkdir, ldavis, write):
    tmstmp = datetime.now().strftime("%y%m%d_%H%M")
    if mkdir:
        mydir = './result/' + tmstmp + '_' + project  # 완료시간 기준
        os.makedirs(mydir)
        flda.save(mydir + '/' + 'ldafit.pickle')  # fitted LDA
        if ldavis:
            LDAvis(flda, inst, mydir)
    if write:
        optparset['file'] = tmstmp + '_' + project
        optparset['ngram'] = ngram
        optparset['random_seed'] = random_seed
        f = pd.read_csv("./result/optparset.csv")
        f.append(optparset, ignore_index=True, sort=False).to_csv("./result/optparset.csv", index=False)
    gcol()
    return (tmstmp)


##########################################################################################################
print("==============\nMylib imported!\n==============")

if __name__ == "__main__":
    from Mylib import *
