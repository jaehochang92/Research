import importlib, random
# import Mylib
# importlib.reload(Mylib)
from Mylib import *
# %matplotlib inline

def generate_grouped_dod(data, select_col):
    groups = np.unique(data[select_col])
    return({'{}'.format(g):my_list(data.loc[[idx 
                              for gi, idx in zip(data[select_col], data.index) if gi == g]].token).sum()
            for g in groups})

# Only once
GSAPI = GgleAPI('credentials.json', 'sheets')
GSAPI.Create_Service('v4', ['https://www.googleapis.com/auth/spreadsheets'])

# ==========================================================================
project = '20대_보건복지부'
random_seed = 2020
tpc_grd = np.arange(40, 8-4, -4) # Remark: decreasing order!
save = True

iterators = [(ng,alp) 
             for ng in [1, 2, 3]
             for alp in ['symmetric', 'asymmetric'][:]]

# 1. Data Preparation
raw_data = pd.read_pickle('./result/HWC_noun_token.pkl')
raw_data.loc[pd.isna(raw_data.PARTY), 'PARTY'] = '非의원'

rms = [
    '정도', '이것을', '제가', '지금', '필요', '관련', '부분', '수고', '얘기', 
    '말씀', '하다', '검토', '경우', '오늘', '상황', '계속', '일정', '올해', # 저정보 용어
    '국민', '문제', # 상투어
    '간사', '민주당', '원님', '복지부', '위원회', '위원장', '위원', '의원', '장관', # 명칭
    '정돈', '보고', '진행', '답변', '발언', '예', '다음', '생각', '질의', # 진행
    '김광수','윤소하','남인순','전혜숙','정춘숙','김상훈','기동민','박인숙','인재근' # 인명
]
man = pd.read_csv('./data/190915_용어_수작업.csv') # Manual term removals
man = man[man['code'] == 1]['단어']
rms.extend(man)
raw_data.token = [list(set(d) - set(rms)) for d in raw_data.token]

for i in tqdm(iterators, 'iterations'):
    ngram, alpha = i
    data = raw_data.copy()
#     Testing?
#     R = random.Random(random_seed)
#     data = data.loc[R.sample([i for i in data.index], 200)]

#     1.2 ngrams model
    data.token = make_ngram(data.token, ngram, mincnt = 3, thrsh = 5)
#     2. Grouping Corpora
#     2.1. Group Corpora
    dict_of_docs = generate_grouped_dod(data, 'token')

#     3. Fiting

#     3.1. tfidf prior
#     tv = TfidfVectorizer()
#     tv_matrix = tv.fit_transform([' '.join(d) for d in dict_of_docs.values()])
#     tv_matrix = tv_matrix.toarray()
#     vocab = tv.get_feature_names()
#     tfidf = pd.DataFrame(tv_matrix, columns=vocab)

#     3.2. Prepare doLDA instance and hyper parameters
    inst = doLDA([v for v in dict_of_docs.values()], use_tfidf=False, top_n_words=15)
    inst.get_bow(no_below = 0, no_above = 1)
#     bt_grd = lngrd(l = 10, m = np.exp(-32), M = .5)
#     print(r'Tuning time: {} $\times$ ?s/it'.format(len(tpc_grd)*len(bt_grd)))

#     3.3. Tune, get topic DF and optimal solutions
    flda, topicDF, optparset = LDA_tuning(inst_doLDA=inst, 
                                          weight_on_coher=1.,
                                          ntopics= tpc_grd,
                                          hypars = {'alpha':alpha, 'beta':['auto']},
                                          rand = random_seed,
                                          iters = 10**2
                                         )
    plt.close()

#     4. Save files
    if save == True:
        for 전후, 회차 in [('정권교체전', [343, 351]),
               ('정권교체후', [352, 364])]:
            gdata = data.loc[[idx
                              for n in range(회차[0],회차[1]+1)
                              for idx, m in zip(data.MEETING_NAME.index, data.MEETING_NAME)
                              if re.search(str(n), m) is not None]]
            results = {
                'project':project+'_'+전후, 'GSAPI':GSAPI, 'flda':flda, 'inst':inst, 
                'optparset':optparset, 'topicDF':topicDF, 'ngram':ngram, 'random_seed':random_seed,
                'dict_of_docs':generate_grouped_dod(gdata, 'PARTY')
            }
            save_results(mkdir = False, API = True, LDAvis = False, **results)