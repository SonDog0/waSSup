import requests
from bs4 import BeautifulSoup


headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.92 Safari/537.36'}

title = []

for i in range (1,238):
    # Lpoint 키워드 검색 페이지당 20개 게시글 , 총 238페이지
    # => 4000여개 게시글 수집

    url = 'http://www.ppomppu.co.kr/search_bbs.php?search_type=sub_memo&page_no='+str(i)+'&keyword=Lpoint&page_size=20&bbs_id=&order_type=date&bbs_cate=1'


    res = requests.get(url, headers=headers)
    html = BeautifulSoup(res.text, 'lxml')

    # 게시글 제목 추출 및 저장
    for a in html.select('form div div span a'):
        title.append(a.text)

# 확인코드
for i in range(0,len(title)):
    print(title[i])


# 파일저장

f = open('./data/Lpoint_ppomppu.txt','a' , encoding='UTF-8')
for i in range(0,len(title)):
    f.write(title[i])
    f.write('\n')



