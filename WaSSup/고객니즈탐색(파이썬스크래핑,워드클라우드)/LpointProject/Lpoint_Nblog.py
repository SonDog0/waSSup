# 네이버 블로그에서 'Lpoint' 검색시
# 각 블로그 작성글의 제목 , 요약 추출

import requests
from bs4 import BeautifulSoup

blog_title = []
url ='https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=Lpoint&sm=tab_pge&srchby=all&st=sim&where=post'
headers = {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.92 Safari/537.36'}
for i in range(1,1000,10):
    # 네이버 블로그검색에서 제공하는 최대 데이터인 1000개의 게시글에서 제목 및 요약 추출

    params = {'start': i}
    res = requests.get(url,headers=headers,params=params)
    html = BeautifulSoup(res.text,'lxml')

    # 게시글 제목 / 요약 추출
    for a in html.select('li.sh_blog_top'):
        blog_title.append(a.text)


# 확인코드
for i in range(len(blog_title)):
    print(blog_title[i])

# 파일저장
f = open('./data/Lpoint_Nblog.txt', 'a', encoding='UTF-8')
for i in range(0, len(blog_title)):
    f.write(blog_title[i])
    f.write('\n')
