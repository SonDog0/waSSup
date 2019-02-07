# 중복행 제거
# ( 광고성 게시글의 반복된 등장 으로 인해 데이터

# 중복성 제거할 데이터 load
import pandas as pd
data = pd.read_csv('./data/Lpoint_ppomppu_mix.txt' , sep = '\t' , header = None)

# 데이터 파악
print(data.describe())

# 중복행 제거
unique_son =data.drop_duplicates()

# 중복행 제거 파악
print(unique_son.describe())

# 정제 된 파일로 재생성
unique_son.to_csv('./data/Lpoint_ppomppu_mix_unique.txt',sep= '\t' , index = False)


