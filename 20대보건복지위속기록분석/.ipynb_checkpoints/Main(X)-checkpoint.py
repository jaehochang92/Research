from Mylib import *
from os import *
import random

### 1. Data Loading & Cleansing(Only Once)

# Loading Tokens
noun_tokened = pd.read_pickle('./result/HWC_noun_token.pkl')
#print(noun_tokened.groupby(['MEETING_NAME']).size())
# print(noun_tokened.groupby(['PARTY']).size())
# print(noun_tokened.groupby(['SPEAKER']).size())
# all_tokened = pd.read_pickle('./result/HWC_all_token.pkl')

# Data cleansing
noun_tokened.loc[noun_tokened['SPEAKER']=='박인숙 위원', 'PARTY']='자유한국당'
# Domain Knowledge based Filtering
Coding = pd.read_csv('./data/단어집.csv')
DKF = Coding[Coding['code'] == 1]['단어']

### 2. Custom criterions 

# Data Preparation
addf = [
    '국민', # 상투어
    '위원장', '위원', '의원', '장관', # 직책
    '답변', '진술', '정회', '중단', '초과', '발언', '질의', # 회의 진행용어
]

DKF = DKF.append(pd.Series(addf))
noun_tokened['token'] = [list(set(d) - set(DKF)) for d in noun_tokened['token']]

# 전체 데이터
# fdata = noun_tokened # All data

#### 2.1. 정당별

#정당 = ['국민의당']
# 정당 = ['더불어민주당'] # 진보
정당 = ['자유한국당'] # 보수

fdata = prpr_data(noun_tokened).by_party(정당).get_data()

#진영별?
#정당 = ['바른정당','자유한국당'] # 보수
#정당 = ['국민의당','더불어민주당','무소속'] # 진보

#### 2.2. 회차별

# 회차 = [343, 350] # 정권교체 전
회차 = [352, 364] # 정권교체 후

fdata = prpr_data(noun_tokened).by_meeting(회차).get_data()

#### 2.3. 디렉토리 생성

tokens = fdata['token'].copy()

print('ALL', noun_tokened.shape)
print('fdata', fdata.shape)

mydir = './result/' + datetime.now().strftime("%y%m%d;%H%M")
makedirs(mydir)

### 3. Word Cloud

# random.seed(1001)

# wordsvec = [item for sublist in tokens for item in sublist]
# wordsvec = pd.Series(wordsvec)
# #print(wordsvec.loc[0:5])
# wcld = WordCloud(width=600, height=600, background_color='white', 
#                  font_path = './etc/NanumBarunGothic.ttf')
# cnt = Counter(wordsvec.tolist())
# wcld = wcld.generate_from_frequencies(cnt)
# #wcld.to_image()
# wcld.to_file(mydir+'/WC.png')

### 4. Corpus Normalization & TF-IDF

gcol()
for j in tqdm(pd.DataFrame.keys(tokens)):
    tokens[j] = ' '.join(tokens[j])
ncorpus = tokens[~tokens.isin(addf)].tolist() # Normalized corpus.
#print('Normalized corpus example : %s' %ncorpus[1:3])

# TF-IDF
tv = TfidfVectorizer(min_df=0., max_df=1., use_idf=True)
tv_matrix = tv.fit_transform(ncorpus)
tv_matrix = tv_matrix.toarray()

vocab = tv.get_feature_names()
TFIDF = pd.DataFrame(np.round(tv_matrix, 2), columns=vocab)
TFIDF.to_csv(mydir+'/TFIDF.csv')
#TFIDF.head(10) # vocab에 대한 TFIDF score
#TFIDF.shape # vocab에 대한 TFIDF score

### 5. LDA

# Tuning
a, b = 1, 30

perpl, coher = [], []
for top in tqdm(range(a,b+1)) :
    _,p,c = doLDA(fdata['token'], top, 'ALL', False)
    perpl.append(p)
    coher.append(c)
    del _,p,c
    
LDAtuningVis(a, b, perpl, coher)
plt.savefig(mydir+'/perpcoh.png')

# Final results with pyLDAvis
optcoh = range(a,b+1)[np.argmax(coher)]
flda = doLDA(fdata['token'], optcoh, mydir, True)
fldatops = flda[0].print_topics(num_words=15)
fldatops = pd.DataFrame(fldatops, columns=['Topic','Representation'])
fldatops.to_excel(mydir+'/fldatops.xlsx')
path = mydir+'/'+str(optcoh)+'_pyLDAvis.pickle'
pyLDAvis = pd.read_pickle(path)

# # Gathering keywords
# ftopkw = np.unique([j[0] for 
#                     i in range(flda[0].num_topics) for
#                     j in flda[0].get_topic_terms(i)])
# ftopkw = [flda[0].id2word[i] for i in ftopkw]

# Gephi (coocurrence)
gcol()
norm_corp = fdata['token'].copy()
for j in tqdm(range(len(norm_corp))):
#     norm_corp.iloc[j] = ' '.join(intersection(norm_corp.iloc[j],ftopkw))
    norm_corp.iloc[j] = ' '.join(norm_corp.iloc[j])
norm_corp = norm_corp.tolist() # Normalized corpus.

print('Normalized corpus example : %s' %norm_corp[1:3])

cv = CountVectorizer(ngram_range=(1,1))
X = cv.fit_transform(norm_corp)

Xc = (X.T * X) # This is the matrix manipulation step
Xc.setdiag(0) # We set the diagonals to be zeroes as it's pointless to be 1
names = cv.get_feature_names() # This are the entity names (i.e. keywords)
df = pd.DataFrame(data = Xc.toarray(), columns = names, index = names)

df.to_csv(mydir+'/coocurr_for_gephi.csv', sep = ',', encoding = 'utf-8')