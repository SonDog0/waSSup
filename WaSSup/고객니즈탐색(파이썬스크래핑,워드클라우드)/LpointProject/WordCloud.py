from konlpy.tag import Okt
from collections import Counter
twitter = Okt()
from wordcloud import WordCloud
import matplotlib.pyplot as plt
import re

# 텍스트 데이터 가져오기
f = open('./data/Lpoint_ppomppu_mix_unique.txt' , encoding='utf-8')
docs = f.read()

# 명사추출
nouns = twitter.nouns(docs)

# 금칙어 , 광고문자 제거
wdlist = []
# stopword = '이용|빅플|장의|보기|이미지|블로그|검색|롯데|홈쇼핑|돌잡이|방송|패키지|상품|구매|라이브|판매|이번|로그인|포인트|롭스'
# 네이버블로그

stopword = '소액|수퍼|에누리|오키|여러가지|루걸|판매|롯데|포인트|홈쇼핑|장기|옥션|대리|결제|렌터카|렌트|하이마트|스타벅스'
# 뽐뿌

# 2자이상 단어 추출
for i in range(0,len(nouns)):
    nouns[i] = re.sub(stopword,'',nouns[i])
    if len(nouns[i]) >= 2 :
        wdlist.append(nouns[i])

wc = Counter(wdlist) # 빈도수 확인
print(wc)

# 워드클라우드로 출력
wcimg = WordCloud(font_path=r'c:/Windows/Fonts/malgun.ttf',background_color='white')\
    .generate_from_frequencies(wc)
plt.imshow(wcimg, interpolation='bilinear')
plt.axis('off')
plt.show()

#
