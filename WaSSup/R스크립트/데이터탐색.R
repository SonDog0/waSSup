#파일불러오기
library(data.table)

setwd('c:/Java/Lpoint')
Sys.setlocale(category = "LC_ALL", locale = "Kor")

Product <- fread('01.Pruduct.csv', header = T, stringsAsFactors = F, sep=",", quote='\"')
Search1 <- fread('02.Search12.csv', header = T, stringsAsFactors = F, sep = ",")
Search2 <- fread('03.Search22.csv', header = T, stringsAsFactors = F, sep = ",", quote="\"")
Custom <- fread('04.Custom.csv', header = T, stringsAsFactors = F, sep = ",",encoding = "UTF-8")
Session <- fread('05.Session.csv', header = T, stringsAsFactors = F, sep = ",",encoding = "UTF-8")
Master <- fread('06.Master2.csv', header = T, stringsAsFactors = F, sep = ",",quote='\"')

# Product BUY_AM 숫자형으로 변경
library(stringr)
Product$PD_BUY_AM <- str_remove_all(Product$PD_BUY_AM,',')
Product$PD_BUY_AM <- str_remove_all(Product$PD_BUY_AM,' ')
Product$PD_BRA_NM <- str_remove_all(Product$PD_BRA_NM,'\\[')
Product$PD_BRA_NM <- str_remove_all(Product$PD_BRA_NM,'\\]')
Product$PD_BUY_AM <- as.numeric(Product$PD_BUY_AM)

# Product와 Session Search1에  CLNT_ID SESS_ID를 합친 변수 생성
# (Session에 중복된 SESS_ID가 있어 오류가 있기때문 고유값 생성)
Product$code<-paste(Product$CLNT_ID, Product$SESS_ID)
Session$code<-paste(Session$CLNT_ID, Session$SESS_ID)
Search1$code<-paste(Search1$CLNT_ID, Search1$SESS_ID)

# 데이터 합치기 
# Product기준으로 P_row = 클라이언트ID, 단일상품액, 구매건수, 구매금액, 상품코드, 상품브랜드
# 기기유형, 상품 대분류, 중분류, 소분류, 지역 대분류, 중분류
library(sqldf)
P_row<-sqldf("select p.CLNT_ID as CLANTID, p.SESS_ID as SESS_ID, p.code as code,
             p.PD_BUY_AM as PD_BUY_AM,
             p.PD_BUY_CT as PD_BUY_CT, p.PD_BUY_AM*p.PD_BUY_CT as buym , p.PD_C as PD_C,
             p.PD_BRA_NM as brand, p.HITS_SEQ as HITS_SEQ , c.CLNT_GENDER as CLNT_GENDER,
             c.CLNT_AGE as CLNT_AGE, s.SESS_DT as date, s.DVC_CTG_NM as DVC,
             m.CLAC1_NM as CLAC1_NM, m.CLAC2_NM as CLAC2_NM, m.CLAC3_NM as CLAC3_NM, s.ZON_NM as ZON,
             s.CITY_NM as CITY
             from Product p left join Custom c on p.CLNT_ID = c.CLNT_ID
             left join Session s on p.code = s.code left join Master m on p.PD_C = m.PD_C")
C_row <- sqldf("select CLANTID, CLNT_GENDER, CLNT_AGE, avg(buym) as M_AVG, sum(buym) as M_SUM,
               max(buym) as M_MAX, count(CLANTID) as BUY_CT, min(date) as first_day,
               max(date) as last_day, count(distinct(CLAC1_NM)) as x_idx1, 
               count(distinct(CLAC2_NM)) as x_idx2, count(distinct(CLAC3_NM)) as x_idx3
               from P_row group by CLANTID")
# 값 정제 
# 날짜형으로 변경(최근성 변수 생성 : 2018/10/01 - 마지막 구매일 = 일수)
library(lubridate)
P_row$date <-ymd(P_row$date)
C_row$last_day <- ymd(C_row$last_day)
C_row$first_day <- ymd(C_row$first_day)
C_row$R_grad <- as.Date('2018-10-01')-C_row$last_day

# C_row에 방문빈도(기간 내에 1일 방문=1, 2일방문 = 2) 생성
b<-sqldf("select CLANTID, count(distinct(date))as f from P_row group by CLANTID")
C_row$Fv <- b$f
rm(b)
# c_row에 결합할 c_row_avgbuydate DF생성
# ※평균재구매일 = (마지막 구매일 - 첫 구매일) / (구매횟수 - 1) 
c_row_avgbuydate <- sqldf('select floor((last_day-first_day)/(Fv-1)) as AVGbuyDate from C_row')

# c_row에 결합 
C_row <- cbind(C_row,c_row_avgbuydate)

# NA값 0으로 변경 
C_row$AVGbuyDate <- ifelse(is.na(C_row$AVGbuyDate),0,C_row$AVGbuyDate)
rm(c_row_avgbuydate)

# P_row에 구매 월,일넣기
P_row$M <- format(as.Date(P_row$date), "%m")
P_row$day <- format(as.Date(P_row$date), "%d")
P_row$M <- as.numeric(P_row$M)
P_row$day <- as.numeric(P_row$day)


# 기초통계분석

#어떤 서비스를 제공할 것인가

# 기초통계량
summary(P_row)
summary(C_row)
# C_row의 결과 숫자형 변수의 경우 MAX금액이 극단 이상치로 보이는 변수가 대부분임
# AGE와 GENDER NA값 존재(Product에는 있던 클라이언트 ID가 회원(Custom)테이블에 존재하지 않았음)
# Product left join Custom 결과로 NA값 생성됨
# 성별과 나이를 포함한 분석의 경우 NA값 고려하여 분석
# 숫자형 변수들 이상치 제거 필요

# 1. 각 변수별 탐색

# 총  922,737개의 클라이언트ID
barplot(table(C_row$CLNT_GENDER))
table(C_row$CLNT_GENDER)
# 총 671,679명중 여성570616명 / 남성 101063명 /na 값 453,184개

library(plotrix)
# 성별 파이플롯
pie3D(table(C_row$CLNT_GENDER), main="남녀비율", col=rainbow(length(table(C_row$CLNT_GENDER))),
      labels=c("F","M"), cex=1)
legend(-1,1,c("F = 570,616명 / 85%", "M = 101,063명 / 15%"), cex=0.5, fill=rainbow(length(table(C_row$CLNT_GENDER))))

# 나이별 막대그래프
barplot(table(C_row$CLNT_AGE), main="연령대 막대그래프", col=rainbow(length(table(C_row$CLNT_AGE))))
summary(C_row$CLNT_AGE)
table(C_row$CLNT_AGE)

# 평균구매금액 산포도/boxplot
plot(C_row$M_AVG)
boxplot(C_row$M_AVG)
summary(C_row$M_AVG)

# 분석시 이상치 제거 필요

# 총구매금액 산포도/boxplot
plot(C_row$M_SUM)
boxplot(C_row$M_SUM)
summary(C_row$M_SUM)
# 분석시 이상치 제거 필요

# 최대 구매금액 산포도/boxplot
plot(C_row$M_MAX)
boxplot(C_row$M_MAX)
summary(C_row$M_MAX)

# 구매횟수 산포도 / boxplot
plot(C_row$BUY_CT)
boxplot(C_row$BUY_CT)
summary(C_row$BUY_CT)

# date변수 고객기간이나 구매기간 등을 알기엔 불충분
# 최근성, 구매주기, 구매 빈도 등을 추출하기 위해 사용가능

# x_idx1, 2, 3 구매다양성 상품 대분류, 중분류, 소분류별 다양하게 구매한 정도
# 상품 대분류
boxplot(C_row$x_idx1)
summary(C_row$x_idx1)
#상품 중분류
boxplot(C_row$x_idx2)
summary(C_row$x_idx2)
#상품 소분류
boxplot(C_row$x_idx3)
summary(C_row$x_idx3)
# summary() 통해 확인 극도로 다양한 쇼핑을 즐긴 집단 분류해야됨

# 최근성 R_grad 2018-10-01 - 마지막 구매일
C_row$R_grad<-as.numeric(C_row$R_grad)
boxplot(C_row$R_grad)
summary(C_row$R_grad)

# 방문빈도
boxplot(C_row$Fv)
summary(C_row$Fv)

# 평균 재구매 일수
boxplot(C_row$AVGbuyDate[C_row$AVGbuyDate != 0])
summary(C_row$AVGbuyDate[C_row$AVGbuyDate != 0])
석


# 고객데이터 분석
# 변수별 상관관계
library(corrplot)
# 총구매금액 800000만원 이하로 설정
C_row_sumx<-C_row[C_row$M_SUM<=800000,]
# 평균구매주기 1일 이상
C_row_sumx<-C_row[C_row_sumx$AVGbuyDate >=1,]

#나이, 평균구매금액, 총구매금액, 최대구매금액, 소분류다양성지수, 최근성, 빈도수, 평균구매주기 선택
C_row_cor <- C_row_sumx[,c(3:6,12:15)]
#NA값제거
C_row_cor2 <- na.omit(C_row_cor)
#숫자형으로 변경
C_row_cor2$R_grad <-as.numeric(C_row_cor2$R_grad)
#확인
str(C_row_cor2)
names(C_row_cor2) <- c('연령', '평균구매금액', '총구매금액', '최대구매금액', '상품대분류', '최근성', '방문빈도', '평균구매일수')
C_row_corr<-cor(C_row_cor2)
corrplot(C_row_corr, method = "ellipse", )

# 고객 세분화를 위한 변수로 
# (Fv, M_AVG : 평군구매금액과 구매 빈도에 따른 온라인 선호지수 추출) 
# 또는 (R_grad,Fv : 최근성과 구매빈도에 따른 온라인 선호지수 추출)이 괜찮아보임
rm(C_row_cor)
rm(C_row_cor2)
rm(C_row_corr)
rm(C_row_sumx)

library(caret)# 군집분석을 위한 라이브러리
C_row_clust<-C_row[,c(13,14)]
str(C_row_clust)
# 이상치 제거 및 표준화(최근성은 이상치제거X)
# 최근성 군집분석
R_kmeans<-kmeans(C_row_clust$R_grad*-1, centers = 3, iter.max = 10000)
R_kmeans$centers

B<-C_row
B$R_kmeans<-R_kmeans$cluster

# 군집파악
B$R_grad <- as.numeric(B$R_grad)
summary(B$R_grad[B$R_kmeans==2])
summary(C_row$R_grad[C_row$R_Cluster==2])

# C_row에 최근성 점수 넣기
C_row$R_cluster <- 1
C_row$R_cluster[C_row$R_grad <= 119] <- 2
C_row$R_cluster[C_row$R_grad <= 58] <- 3


# 방문빈도 극단이상치 제거후 군집분석
F_kmeans<-kmeans(C_row_clust$Fv[C_row_clust$Fv<=8], centers = 3, iter.max = 10000)
F_kmeans$centers

A<-C_row[C_row$Fv<=8,]
A$F_cluster<-F_kmeans$cluster

# 군집파악
summary(A$Fv[A$F_cluster==2])
summary(C_row$R_grad[C_row$R_Cluster==1])

# C_row에 방문빈도 점수 넣기
C_row$F_cluster <- 3
C_row$F_cluster[C_row$Fv <= 5] <- 2
C_row$F_cluster[C_row$Fv <= 2] <- 1

# 클러스터 변수만들기
C_row$cluster <- paste(C_row$R_cluster,C_row$F_cluster)
C_row$cluster2 <- "C"
C_row$cluster2[C_row$cluster == '1 1' | C_row$cluster == '2 1'] <- "D"
C_row$cluster2[C_row$cluster == '2 2' | C_row$cluster == '2 3' | C_row$cluster == '1 2' | C_row$cluster == '1 3'] <- "E"
C_row$cluster2[C_row$cluster == '3 3'] <- "A"
C_row$cluster2[C_row$cluster == '3 2'] <- "B"
C_row$cluster2[C_row$cluster == '3 1'] <- "C"

# 집단별 수, 백분율
table(C_row$cluster2)
table(C_row$cluster2)/922737*100
# 고객 세분화 엑셀파일로 정리함
rm(A)
rm(B)
rm(F_kmeans)
rm(R_kmeans)
rm(C_row_clust)


# 추가 데이터 생성 P_row 와 Search1에 고객집단 변수 생성
library(sqldf)
a <- sqldf("select c.cluster2 as cluster from P_row p left join C_row c on p.CLANTID=c.CLANTID")
P_row$cluster<-a$cluster
b<- sqldf("select c.cluster2 as cluster from Search1 s left join C_row c on s.CLNT_ID = c.CLANTID")
Search1$cluster <- b$cluster
rm(a)
rm(b)

# 고객 집단별 탐색

# 집단별 매출액
a<-sqldf("select cluster , sum(buym) from P_row group by cluster")
View(a)
# 총매출액
sum(a$'sum(buym)')
rm(a)

#집단별 연령대
pie(table(C_row$CLNT_AGE[C_row$cluster2 == "A"]))
pie(table(C_row$CLNT_AGE[C_row$cluster2 == "B"]))
pie(table(C_row$CLNT_AGE[C_row$cluster2 == "C"]))
pie(table(C_row$CLNT_AGE[C_row$cluster2 == "D"]))
pie(table(C_row$CLNT_AGE[C_row$cluster2 == "E"]))
table(C_row$CLNT_AGE[C_row$cluster2 == "E"])
sum(table(C_row$CLNT_AGE[C_row$cluster2 == "E"]))
# 집단별 성별
pie(table(C_row$CLNT_GENDER[C_row$cluster2 == "A"]))
pie(table(C_row$CLNT_GENDER[C_row$cluster2 == "B"]))
pie(table(C_row$CLNT_GENDER[C_row$cluster2 == "C"]))
pie(table(C_row$CLNT_GENDER[C_row$cluster2 == "D"]))
pie(table(C_row$CLNT_GENDER[C_row$cluster2 == "E"]))
table(C_row$CLNT_GENDER[C_row$cluster2 == "E"])

#######################################
# 대응분석을 위한 데이터 전처리 후 생성[상품] - 대응분석은 SAS로 진행
library(reshape)
str(P_row)
P_row_clu <- P_row[,c(14,21)]
m<-melt(id=2 , P_row_clu)
P_row_clu <- dcast(m, value~...)
write.csv(P_row_clu, file ="P_row_clu.csv", row.names =TRUE)
rm(m)
rm(P_row_clu)

#DAU구하기P_row 에서 아이디와 구매날짜 추출
P_date<-P_row[,-c(2:11,13:17)]
# 중복값 제거
P_date<-unique(P_date)
# 날짜별 방문 ID 빈도수 출력
library(sqldf)
count_P_date<-sqldf("select date, count(CLANTID) as count from P_date group by date")
# M, day 데이터 추가
library(readxl)
count_P_date$M<-format(as.Date(count_P_date$date), "%m")
count_P_date$day<-format(as.Date(count_P_date$date), "%d")
# 월별 DAU 출력
library(ggplot2)
ggplot(count_P_date, aes(x=day, y=count, colour=M, group=M)) + geom_line(size = 1.2) + geom_point(size = 3) +
  ylim(0,max(count_P_date$count))+
  ggtitle("DAU") + theme(plot.title = element_text(size=10))
rm(P_date)
rm(count_P_date)

########## 요일별 구매자수 확인
library(sqldf)
non_holiday <- sqldf('select date, count(CLANTID) as counts from P_row group by date')
non_holiday$date<-as.character(non_holiday$date)
non_holiday <- sqldf('select * from non_holiday where
                     date not in  ("2018-05-06","2018-05-07","2018-05-08","2018-05-09","2018-05-10","2018-05-11","2018-05-12") 
                     and date not in ("2018-05-20","2018-05-21","2018-05-22","2018-05-23","2018-05-24","2018-05-25","2018-05-26")
                     and date not in ("2018-06-03","2018-06-04","2018-06-05","2018-06-06","2018-06-07","2018-06-08","2018-06-09")
                     and date not in ("2018-06-10","2018-06-11","2018-06-12","2018-06-13","2018-06-14","2018-06-15","2018-06-16")
                     and date not in ("2018-08-12","2018-08-13","2018-08-14","2018-08-15","2018-08-16","2018-08-17","2018-08-18")
                     and date not in ("2018-09-23","2018-09-24","2018-09-25","2018-09-26","2018-09-27","2018-09-28","2018-09-29","2018-09-30")')


non_holiday$dow <- 'sunday'
non_holiday$dow[non_holiday$date == "2018-04-02"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-04-09"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-04-16"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-04-23"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-04-30"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-05-14"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-05-28"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-06-18"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-06-25"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-07-02"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-07-09"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-07-16"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-07-23"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-07-30"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-08-06"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-08-20"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-08-27"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-09-03"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-09-10"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-09-17"] <- 'monday'
non_holiday$dow[non_holiday$date == "2018-04-03"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-04-10"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-04-17"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-04-24"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-05-01"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-05-15"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-05-29"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-06-19"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-06-26"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-07-03"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-07-10"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-07-17"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-07-24"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-07-31"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-08-07"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-08-21"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-08-28"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-09-04"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-09-11"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-09-18"] <- 'tuesday'
non_holiday$dow[non_holiday$date == "2018-04-04"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-04-11"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-04-18"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-04-25"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-05-02"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-05-16"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-05-30"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-06-20"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-06-27"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-07-04"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-07-11"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-07-18"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-07-25"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-08-01"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-08-08"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-08-22"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-08-29"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-09-05"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-09-12"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-09-19"] <- 'wednesday'
non_holiday$dow[non_holiday$date == "2018-04-05"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-04-12"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-04-19"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-04-26"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-05-03"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-05-17"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-05-31"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-06-21"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-06-28"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-07-05"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-07-12"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-07-19"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-07-26"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-08-02"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-08-09"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-08-23"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-08-30"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-09-06"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-09-13"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-09-20"] <- 'thursday'
non_holiday$dow[non_holiday$date == "2018-04-06"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-04-13"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-04-20"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-04-27"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-05-04"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-05-18"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-06-01"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-06-22"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-06-29"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-07-06"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-07-13"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-07-20"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-07-27"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-08-03"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-08-10"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-08-24"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-09-31"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-09-07"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-09-14"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-09-21"] <- 'friday'
non_holiday$dow[non_holiday$date == "2018-04-07"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-04-14"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-04-21"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-04-28"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-05-05"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-05-18"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-06-02"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-06-23"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-06-30"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-07-07"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-07-14"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-07-21"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-07-28"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-08-04"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-08-11"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-08-25"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-09-01"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-09-08"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-09-15"] <- 'saturday'
non_holiday$dow[non_holiday$date == "2018-09-22"] <- 'saturday'
names(non_holiday) <- c("date",'counts','dow')
non_holiday$no_dow[non_holiday$dow == "sunday"] <- '1'
non_holiday$no_dow[non_holiday$dow == "monday"] <- '2'
non_holiday$no_dow[non_holiday$dow == "tuesday"] <- '3'
non_holiday$no_dow[non_holiday$dow == "wednesday"] <- '4'
non_holiday$no_dow[non_holiday$dow == "thursday"] <- '5'
non_holiday$no_dow[non_holiday$dow == "friday"] <- '6'
non_holiday$no_dow[non_holiday$dow == "saturday"] <- '7'
hist_dow <- sqldf('select dow,sum(counts) as counts from non_holiday group by dow')
str(hist_dow)
hist_dow$counts <- as.numeric(hist_dow$counts)
hist_dow$counts <- hist_dow$counts/20
hist_dow1 <- hist_dow[4,]
hist_dow1 <- rbind(hist_dow1, hist_dow[2,])
hist_dow1 <- rbind(hist_dow1, hist_dow[6,])
hist_dow1 <- rbind(hist_dow1, hist_dow[7,])
hist_dow1 <- rbind(hist_dow1, hist_dow[5,])
hist_dow1 <- rbind(hist_dow1, hist_dow[1,])
hist_dow1 <- rbind(hist_dow1, hist_dow[3,])
hist_dow1$dow <- factor(hist_dow1$dow, levels = c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"), ordered=TRUE)
library(ggplot2)
ggplot(x=reorder(-counts),data = hist_dow1, aes(dow, counts,fill=dow)) + geom_bar(stat="identity")
rm(hist_dow)
rm(hist_dow1)
rm(non_holiday)

#분류별 빈도랭킹
library(sqldf)
a<-sqldf("select CLAC1_NM, count(CLAC1_NM) as count from P_row group by CLAC1_NM order by count desc")
b<-sqldf("select CLAC2_NM, count(CLAC2_NM) as count from P_row group by CLAC2_NM order by count desc")
c<-sqldf("select CLAC3_NM, count(CLAC3_NM) as count from P_row group by CLAC3_NM order by count desc")

#분류별 매출랭킹
library(sqldf)
a<-sqldf("select CLAC1_NM, sum(buym) from P_row group by CLAC1_NM")
b<-sqldf("select CLAC2_NM, sum(buym) from P_row group by CLAC2_NM")
c<-sqldf("select CLAC3_NM, sum(buym) from P_row group by CLAC3_NM")



######################################## 요일별 그래프

#5월 14일(구매고객이 가장 많았던 날)
a<-P_row[P_row$date == "2018-05-14"]
a<-sqldf("select CLAC1_NM, count(CLAC1_NM) as count from a group by CLAC1_NM order by count desc")
#6월 11일(구매고객이 두번째 많았던 날)
b<-P_row[P_row$date == "2018-06-11"]
b<-sqldf("select CLAC1_NM, count(CLAC1_NM) as count from b group by CLAC1_NM order by count desc")
#7월 8일(구매고객이 세번째 많았던 날)
c<-P_row[P_row$date == "2018-07-08"]
c<-sqldf("select CLAC1_NM, count(CLAC1_NM) as count from c group by CLAC1_NM order by count desc")

#9월 23일(구매고객이 가장 적었던 날)
a<-P_row[P_row$date == "2018-09-23"]
a<-sqldf("select CLAC1_NM, count(CLAC1_NM) as count from a group by CLAC1_NM order by count desc")
#9월 24일(구매고객이 두번째 적었던 날)
b<-P_row[P_row$date == "2018-09-24"]
b<-sqldf("select CLAC1_NM, count(CLAC1_NM) as count from b group by CLAC1_NM order by count desc")
#9월 22일(구매고객이 세번째 적었던 날)
c<-P_row[P_row$date == "2018-09-22"]
c<-sqldf("select CLAC1_NM, count(CLAC1_NM) as count from c group by CLAC1_NM order by count desc")

# 일별 검색 이용 고객수 확인
library(sqldf)
ranks2 <- sqldf('select SESS_DT,sum(SEARCH_CNT) as counts from Search2 group by SESS_DT order by SESS_DT,counts desc')
str(ranks2)
ranks2$SESS_DT <- as.character(ranks2$SESS_DT)
ranks2$M <- substr(ranks2$SESS_DT,6,6)
ranks2$day <- substr(ranks2$SESS_DT,7,8)
str(ranks2)
library(ggplot2)
ggplot(ranks2, aes(x=day, y=counts, colour=M, group=M)) + geom_line(size = 1.2) + geom_point(size = 3) +
  ylim(min(ranks2$counts)-1000,max(ranks2$counts))+
  ggtitle(" ") + theme(plot.title = element_text(size=30))
rm(ranks2)

# 날짜별 구매빈도수/ 검색빈도수
D_pro<-sqldf("select date, count(CLANTID) as count from P_row group by date")
da <- sqldf("select SESS_DT as date, sum(SEARCH_CNT) from Search2 group by SESS_DT")
str(da)
da$date <- ymd(da$date)
D_pro$S_count <- da$`sum(SEARCH_CNT)`

# 구매빈도수와 검색 빈도수의 상관관계
D_pro$count <-as.numeric(D_pro$count)
cor_Dpro <- D_pro[,c(2:3)]
cor_Dpro <- na.omit(cor_Dpro)
names(cor_Dpro)<-c("구매수","검색수")
plot(cor_Dpro, main = "구매수와 검색수의 선형관계")
abline(lsfit(cor_Dpro$구매수,cor_Dpro$검색수), lwd=2 , col="red", )
# 검색량이 많아지면 구매량이 많아짐
rm(da)
rm(cor_Dpro)
rm(D_pro)


#요일별 검색 이용 고객수 확인
non_holiday <- sqldf('select SESS_DT, sum(SEARCH_CNT) as counts from Search2 group by SESS_DT')
non_holiday$SESS_DT<-as.character(non_holiday$SESS_DT)
non_holiday <- sqldf('select * from non_holiday where 
      SESS_DT not in  ("20180506","20180507","20180508","20180509","20180510","20180511","20180512") 
                     and SESS_DT not in ("20180520","20180521","20180522","20180523","20180524","20180525","20180526")
                     and SESS_DT not in ("20180603","20180604","20180605","20180606","20180607","20180608","20180609")
                     and SESS_DT not in ("20180610","20180611","20180612","20180613","20180614","20180615","20180616")
                     and SESS_DT not in ("20180812","20180813","20180814","20180815","20180816","20180817","20180818")
                     and SESS_DT not in ("20180923","20180924","20180925","20180926","20180927","20180928","20180929","20180930")')
non_holiday$dow <- 'sunday'
non_holiday$dow[non_holiday$SESS_DT== "20180402"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180409"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180416"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180423"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180430"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180514"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180528"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180618"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180625"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180702"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180709"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180716"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180723"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180730"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180806"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180820"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180827"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180903"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180910"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180917"] <- 'monday'
non_holiday$dow[non_holiday$SESS_DT == "20180403"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180410"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180417"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180424"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180501"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180515"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180529"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180619"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180626"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180703"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180710"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180717"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180724"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180731"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180807"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180821"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180828"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180904"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180911"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180918"] <- 'tuesday'
non_holiday$dow[non_holiday$SESS_DT == "20180404"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180411"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180418"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180425"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180502"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180516"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180530"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180620"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180627"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180704"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180711"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180718"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180725"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180801"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180808"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180822"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180829"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180905"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180912"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180919"] <- 'wednesday'
non_holiday$dow[non_holiday$SESS_DT == "20180405"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180412"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180419"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180426"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180503"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180517"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180531"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180621"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180628"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180705"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180712"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180719"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180726"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180802"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180809"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180823"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180830"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180906"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180913"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180920"] <- 'thursday'
non_holiday$dow[non_holiday$SESS_DT == "20180406"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180413"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180420"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180427"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180504"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180518"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180601"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180622"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180629"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180706"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180713"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180720"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180727"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180803"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180810"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180824"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180931"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180907"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180914"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180921"] <- 'friday'
non_holiday$dow[non_holiday$SESS_DT == "20180407"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180414"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180421"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180428"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180505"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180518"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180602"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180623"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180630"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180707"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180714"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180721"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180728"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180804"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180811"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180825"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180901"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180908"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180915"] <- 'saturday'
non_holiday$dow[non_holiday$SESS_DT == "20180922"] <- 'saturday'
names(non_holiday) <- c("date",'counts','dow')
hist_dow <- sqldf('select dow,sum(counts) as counts from non_holiday group by dow')
hist_dow$counts <- as.numeric(hist_dow$counts)
hist_dow$counts <- hist_dow$counts/20
hist_dow1 <- hist_dow[4,]
hist_dow1 <- rbind(hist_dow1, hist_dow[2,])
hist_dow1 <- rbind(hist_dow1, hist_dow[6,])
hist_dow1 <- rbind(hist_dow1, hist_dow[7,])
hist_dow1 <- rbind(hist_dow1, hist_dow[5,])
hist_dow1 <- rbind(hist_dow1, hist_dow[1,])
hist_dow1 <- rbind(hist_dow1, hist_dow[3,])
hist_dow1$dow <- factor(hist_dow1$dow, levels = c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"), ordered=TRUE)
library(ggplot2)
options(scipen = 100)
ggplot(x=reorder(-counts),data = hist_dow1, aes(dow, counts,fill=dow)) + geom_bar(stat="identity")
rm(hist_dow)
rm(hist_dow1)
rm(non_holiday)

# 검색을 하고 구매하는 고객의 비율
library(sqldf)
a<-sqldf("select distinct(CLANTID) from P_row")
# 구매한 고객 수
nrow(a) #922737
# 검색한 고객 수(구매고객 중)
b<-sqldf("select distinct(CLNT_ID) from Search1")
nrow(b) #511477
511477/922737*100
# 구매고객중 55.43%가 검색활용 구매

# 검색을 하고 구매하는 A고객의 비율
library(sqldf)
a<-sqldf("select distinct(CLANTID) from P_row where cluster = 'A'")
# 구매한 고객 수
nrow(a) #58238
# 검색한 A고객 수(구매고객 중)
b<-sqldf("select distinct(CLNT_ID) from Search1 where cluster = 'A'")
nrow(b) #55040

55040/58238*100
# 구매고객중 94.50%가 검색활용 구매

# 검색을 하고 구매하는 B고객의 비율
library(sqldf)
a<-sqldf("select distinct(CLANTID) from P_row where cluster = 'B'")
# 구매한 고객 수
nrow(a) #86962
# 검색한 B고객 수(구매고객 중)
b<-sqldf("select distinct(CLNT_ID) from Search1 where cluster = 'B'")
nrow(b) #64634

64634/86962*100
# 구매고객중 74.32%가 검색활용 구매

# 검색을 하고 구매하는 C고객의 비율
library(sqldf)
a<-sqldf("select distinct(CLANTID) from P_row where cluster = 'C'")
# 구매한 고객 수
nrow(a) #235184
# 검색한 C고객 수(구매고객 중)
b<-sqldf("select distinct(CLNT_ID) from Search1 where cluster = 'C'")
nrow(b) #114080

114080/235184*100
# 구매고객중 48.50%가 검색활용 구매

# 검색을 하고 구매하는 D고객의 비율
library(sqldf)
a<-sqldf("select distinct(CLANTID) from P_row where cluster = 'D'")
# 구매한 고객 수
nrow(a) #438548
# 검색한 D고객 수(구매고객 중)
b<-sqldf("select distinct(CLNT_ID) from Search1 where cluster = 'D'")
nrow(b) #196451

196451/438548*100
# 구매고객중 44.79%가 검색활용 구매

# 검색을 하고 구매하는 E고객의 비율
library(sqldf)
a<-sqldf("select distinct(CLANTID) from P_row where cluster = 'E'")
# 구매한 고객 수
nrow(a) #438548
# 검색한 E고객 수(구매고객 중)
b<-sqldf("select distinct(CLNT_ID) from Search1 where cluster = 'E'")
nrow(b) #196451

81272/103805*100
# 구매고객중 44.79%가 검색활용 구매
rm(a,b)


# 검색어로 워드클라우드 만들기
library(Rttf2pt1)
library(extrafont)
library(extrafontdb)
font_import(pattern="NanumPen")
fonts()
fonttable()
library(wordcloud)
#전체검색어
wordcloud(words = names(table(Search1$KWD_NM)), freq=table(Search1$KWD_NM), scale=c(3,0.1),min.freq = 200, colors = brewer.pal(9,'BuPu'), random.color = F,random.order = F,rot.per = 0.25)
table(Search1$KWD_NM)
#A집단 검색어
wordcloud(words = names(table(Search1$KWD_NM[Search1$cluster=="A"])), freq=table(Search1$KWD_NM[Search1$cluster=="A"]), scale=c(3,0.1),min.freq = 200, colors = brewer.pal(9,'BuPu'), random.color = F,random.order = F,rot.per = 0.25)
#B집단 검색어
wordcloud(words = names(table(Search1$KWD_NM[Search1$cluster=="B"])), freq=table(Search1$KWD_NM[Search1$cluster=="B"]), scale=c(3,0.1),min.freq = 200, colors = brewer.pal(9,'BuPu'), random.color = F,random.order = F,rot.per = 0.25)
#C집단 검색어
wordcloud(words = names(table(Search1$KWD_NM[Search1$cluster=="C"])), freq=table(Search1$KWD_NM[Search1$cluster=="C"]), scale=c(3,0.1),min.freq = 200, colors = brewer.pal(9,'BuPu'), random.color = F,random.order = F,rot.per = 0.25)
#D집단 검색어
wordcloud(words = names(table(Search1$KWD_NM[Search1$cluster=="D"])), freq=table(Search1$KWD_NM[Search1$cluster=="D"]), scale=c(3,0.1),min.freq = 200, colors = brewer.pal(9,'BuPu'), random.color = F,random.order = F,rot.per = 0.25)
#E집단 검색어
wordcloud(words = names(table(Search1$KWD_NM[Search1$cluster=="E"])), freq=table(Search1$KWD_NM[Search1$cluster=="E"]), scale=c(3,0.1),min.freq = 200, colors = brewer.pal(9,'BuPu'), random.color = F,random.order = F,rot.per = 0.25)
















