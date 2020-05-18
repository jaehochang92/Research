import importlib, random
# import Mylib
# importlib.reload(Mylib)
from Mylib import *
# %matplotlib inline

# Only once
GSAPI = GgleAPI('credentials.json', 'sheets')
GSAPI.Create_Service('v4', ['https://www.googleapis.com/auth/spreadsheets'])
# ==========================================================================
project = '21대_총선'
random_seed = random.Random().randint(1, 99999)
tpc_grd = [12, 11, 10, 9, 8, 7]
save = True

#     1. Data Preparation
dict_of_docs = pd.read_pickle('./data/election_21st.pickle')



iterators = [(ng,alp) 
             for ng in [1, 2, 3]
             for alp in ['symmetric'
#                          , 'asymmetric'
                        ]]

for i in tqdm(iterators, 'iterations'):
    ngram, alpha = i
    # # Testing?
    # R = random.Random(2020)
    # dict_of_docs = {'{}'.format(k):R.sample(dict_of_docs[k], 30) for k in dict_of_docs.keys()}

#         1.1. Data Filtering
    rms = ['정책', '개혁', '추진']
    dict_of_docs = {'{}'.format(k):list(set(dict_of_docs[k]) - set(rms)) for k in dict_of_docs.keys()}

#         1.2 ngrams model    
    ngramed = make_ngram([v for v in dict_of_docs.values()], ngram, mincnt = 2, thrsh = 2)
    dict_of_docs = {'{}'.format(k):ngramed[i] for i, k in enumerate(dict_of_docs)}

    #     3. Fit LDA and tune parameters!

    #     3.1. Prepare doLDA instance and hyper parameters
    inst = doLDA([v for v in dict_of_docs.values()], use_tfidf=False, top_n_words=15)
    inst.get_bow(no_below = .0, no_above = 1)

    #     3.2. Tune, get topic DF and optimal solutions
    flda, topicDF, optparset = LDA_tuning(inst_doLDA=inst, 
                                              weight_on_coher=1.,
                                              ntopics= tpc_grd,
                                              hypars = {'alpha':alpha, 'beta':['auto']},
                                              rand = random_seed)
    plt.close()

    if save == True:
            results = {
                'project':project, 'GSAPI':GSAPI, 'flda':flda, 'inst':inst, 
                'optparset':optparset, 'topicDF':topicDF, 'ngram':ngram, 'random_seed':random_seed,
                'dict_of_docs':dict_of_docs
            }
            save_results(mkdir = False, API = True, LDAvis = False, **results)

print('============\nResults saved!\n============')