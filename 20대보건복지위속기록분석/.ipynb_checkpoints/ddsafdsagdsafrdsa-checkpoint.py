from Mylib import *
import random
# %matplotlib inline

# 0. READ RAW DATA
save = True
raw_data = pd.read_pickle('./result/HWC_noun_token.pkl')

for 전후, 회차 in [['정권교체전', [343, 351]], ['정권교체후', [352, 364]]]:
    print(전후, 회차)

    # 1. DATA PREPROCESSING
    R = random.Random(2020)
    raw_data = raw_data.loc[R.sample([i for i in raw_data.index], 50)]
#     raw_data = raw_data[[not i for i in pd.isna(raw_data.PARTY)]] # Remove PARTY==NaN
#     parties = ['자유한국당', '더불어민주당'] # 보수 vs 진보진영
    
#     1.1. GET CORPUSES BY GROUPS
    parties, mtkn_prty, _= get_corpus_by_group(raw_data, 회차, gv = 'PARTY')

#     2. TFIDF
#     tv = TfidfVectorizer(use_idf=True, smooth_idf=False, ngram_range=(1,1))
#     tv_matrix = tv.fit_transform(mtkn_prty).toarray()
#     vocab = tv.get_feature_names()
#     TFIDF = pd.DataFrame(np.round(tv_matrix, 4), columns=vocab)
#     TFIDF.index = parties
#     TFIDF.shape # vocab에 대한 TFIDF score
#     TFIDF.loc['자유한국당'].sort_values(ascending=False)[:10]
#     TFIDF.loc['더불어민주당'].sort_values(ascending=False)[:10]

#     1.1. DATA FILTERING
    rms = [
        '정도', '이것을', '제가', '지금', '필요', '관련', '부분', '수고', '얘기', 
        '말씀', '하다', '검토', '경우', '오늘', '상황', '계속', '일정', '올해', # 저정보 용어
        '복지', '보건', '국민', '문제', # 상투어
        '민주당', '원님', '복지부', '위원회', '위원장', '위원', '의원', '장관', # 명칭
        '보고', '진행', '답변', '발언', '예', '다음', '생각', '질의' # 진행
    ]
    raw_data.token = [list(set(d) - set(rms)) for d in raw_data.token]
    man = pd.read_csv('./data/190915_용어_수작업.csv') # Manual term removals
    man = man[man['code'] == 1]['단어']
    raw_data.token = [list(set(d) - set(man)) for d in raw_data.token]

#     3. LDA
    inst = doLDA(raw_data.token, use_tfidf=False, top_n_words=15)
    inst.get_bow(no_below = 0, no_above = 1)
    grd = np.linspace(1e-4,.9999, num = 5);print('grid for hyper-parameters: {}'.format(grd))
    flda, topicDF, optparset = LDA_tuning(inst_doLDA=inst, 
                                          weight_on_coher=1.,
                                          ntopics=np.arange(4,4+2, 2),
                                          hypars = {'Alpha':['symmetric'], 'Beta':grd}
                                         )
#     flda.print_topics(num_words=15)

#     Estimate Topics by Parties
    tops = ['Topic'+ str(s) for s in np.arange(0, flda.num_topics)+1]
    party_topics = pd.DataFrame([], columns=tops)
    for cr in  parties:
        dd =[]
        for d in raw_data[raw_data.PARTY == cr]['token']:
            dd += d
        dd = pd.DataFrame(flda.get_document_topics(inst.dictionary.doc2bow(dd), minimum_probability=0)).T.iloc[1:2,:]
        dd.index, dd.columns = [cr], tops
        party_topics = party_topics.append(dd)

#     Save files
    if save == True:
        # 0. mkdir
        tmstmp = datetime.now().strftime("%y%m%d;%H%M")
        mydir = './result/' + tmstmp + ';' + 전후 # 완료시간 기준
        os.makedirs(mydir)
        flda.save(mydir + '/' + str(flda.num_topics) + 'ldafit.pickle', ignore=('state', 'dispatcher')) # fitted LDA
        f= open(mydir + "/optparset.txt","w+")
        [f.write('{} : {}\n'.format(k, optparset[k])) for k in optparset.keys()]
        f.close()
        party_topics.to_csv(mydir+'/gammas.csv', encoding='EUC-KR')
#         TFIDF.to_csv(mydir+'/TFIDF.csv', encoding='EUC-KR')
        topicDF.to_csv(mydir+'/topicDF.csv', index=False, encoding='EUC-KR')
        pd.DataFrame({'{}'.format(p) : list(party_topics.columns[(-party_topics.loc[p]).argsort()[:5]]) for p in party_topics.index}).to_csv(mydir+'/topics_top5.csv', encoding='EUC-KR')
        LDAvis(flda, inst, mydir)
        print('Results saved!')
    
# AFTER FITTING
# path = mydir+'/'+str(optcoh)+'_pyLDAvis.pickle'
# pyLDAvis = pd.read_pickle(path)
# cflda = pd.read_pickle(mydir + '/' + str(flda.num_topics) + 'ldafit.pickle')
# cparty_topics = pd.read_csv(mydir+'/gammas.csv', index_col = 0, encoding='euc-kr')
# ctopicDF = pd.read_csv(mydir+'/topicDF.csv', index_col = 0, encoding='euc-kr')
# ctopics_top5 = pd.read_csv(mydir+'/topics_top5.csv', index_col = 0, encoding='euc-kr')
# pd.set_option('display.max_colwidth', -1)