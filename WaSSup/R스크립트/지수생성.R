## 선호지수 만들기
setwd('c:/Java/Lpoint')
P_row$M <- as.numeric(P_row$M)
P_row_4m <- P_row[P_row$M == 4,]
P_row_5m <- P_row[P_row$M == 5,]
P_row_6m <- P_row[P_row$M == 6,]
P_row_7m <- P_row[P_row$M == 7,]
P_row_8m <- P_row[P_row$M == 8,]
P_row_9m <- P_row[P_row$M == 9,]

P_row_4m1 <- sqldf("select CLAC1_NM, sum(PD_BUY_CT) as count, sum(buym) as buym, count(distinct(CLANTID)) as id_count from P_row_4m group by CLAC1_NM")
P_row_5m1 <- sqldf("select CLAC1_NM, sum(PD_BUY_CT) as count, sum(buym) as buym, count(distinct(CLANTID)) as id_count from P_row_5m group by CLAC1_NM")
P_row_6m1 <- sqldf("select CLAC1_NM, sum(PD_BUY_CT) as count, sum(buym) as buym, count(distinct(CLANTID)) as id_count from P_row_6m group by CLAC1_NM")
P_row_7m1 <- sqldf("select CLAC1_NM, sum(PD_BUY_CT) as count, sum(buym) as buym, count(distinct(CLANTID)) as id_count from P_row_7m group by CLAC1_NM")
P_row_8m1 <- sqldf("select CLAC1_NM, sum(PD_BUY_CT) as count, sum(buym) as buym, count(distinct(CLANTID)) as id_count from P_row_8m group by CLAC1_NM")
P_row_9m1 <- sqldf("select CLAC1_NM, sum(PD_BUY_CT) as count, sum(buym) as buym, count(distinct(CLANTID)) as id_count from P_row_9m group by CLAC1_NM")
# 소분류별 상품 빈도수 지수
P_row_m3c <- sqldf("select a.CLAC1_NM as CLAC1_NM, a.count as count4, b.count as count5, c.count as count6, d.count as count7,
                   e.count as count8 , f.count as count9
                   from P_row_4m1 a inner join P_row_5m1 b on a.CLAC1_NM = b.CLAC1_NM
                   inner join P_row_6m1 c on b.CLAC1_NM = c.CLAC1_NM inner join P_row_7m1 d on c.CLAC1_NM = d.CLAC1_NM
                   inner join P_row_8m1 e on d.CLAC1_NM = e.CLAC1_NM inner join P_row_9m1 f on e.CLAC1_NM = f.CLAC1_NM")
P_row_m3c$index4<-100
P_row_m3c$index5<-P_row_m3c$count5/P_row_m3c$count4*100
P_row_m3c$index6<-P_row_m3c$count6/P_row_m3c$count4*100
P_row_m3c$index7<-P_row_m3c$count7/P_row_m3c$count4*100
P_row_m3c$index8<-P_row_m3c$count8/P_row_m3c$count4*100
P_row_m3c$index9<-P_row_m3c$count9/P_row_m3c$count4*100




#소분류별 구매금액 지수
P_row_m3b <- sqldf("select a.CLAC1_NM as CLAC1_NM, a.buym as buym4, b.buym as buym5, c.buym as buym6, d.buym as buym7,
                   e.buym as buym8 , f.buym as buym9
                   from P_row_4m1 a inner join P_row_5m1 b on a.CLAC1_NM = b.CLAC1_NM
                   inner join P_row_6m1 c on b.CLAC1_NM = c.CLAC1_NM inner join P_row_7m1 d on c.CLAC1_NM = d.CLAC1_NM
                   inner join P_row_8m1 e on d.CLAC1_NM = e.CLAC1_NM inner join P_row_9m1 f on e.CLAC1_NM = f.CLAC1_NM")
P_row_m3b$index4 <- 100
P_row_m3b$index5<-P_row_m3b$buym5/P_row_m3b$buym4*100
P_row_m3b$index6<-P_row_m3b$buym6/P_row_m3b$buym4*100
P_row_m3b$index7<-P_row_m3b$buym7/P_row_m3b$buym4*100
P_row_m3b$index8<-P_row_m3b$buym8/P_row_m3b$buym4*100
P_row_m3b$index9<-P_row_m3b$buym9/P_row_m3b$buym4*100

#소분류별 고객다양성 지수
P_row_m3i <- sqldf("select a.CLAC1_NM as CLAC1_NM, a.id_count as id_count4, b.id_count as id_count5, c.id_count as id_count6, d.id_count as id_count7,
                   e.id_count as id_count8 , f.id_count as id_count9
                   from P_row_4m1 a inner join P_row_5m1 b on a.CLAC1_NM = b.CLAC1_NM
                   inner join P_row_6m1 c on b.CLAC1_NM = c.CLAC1_NM inner join P_row_7m1 d on c.CLAC1_NM = d.CLAC1_NM
                   inner join P_row_8m1 e on d.CLAC1_NM = e.CLAC1_NM inner join P_row_9m1 f on e.CLAC1_NM = f.CLAC1_NM")
P_row_m3i$index4 <- 100
P_row_m3i$index5<-P_row_m3i$id_count5/P_row_m3i$id_count4*100
P_row_m3i$index6<-P_row_m3i$id_count6/P_row_m3i$id_count4*100
P_row_m3i$index7<-P_row_m3i$id_count7/P_row_m3i$id_count4*100
P_row_m3i$index8<-P_row_m3i$id_count8/P_row_m3i$id_count4*100
P_row_m3i$index9<-P_row_m3i$id_count9/P_row_m3i$id_count4*100

a <- P_row_m3b 
b <- P_row_m3c
c <- P_row_m3i

library(sqldf)
options('scipen'=100)
a1 <- sqldf('select index5,index6,index7,index8,index9 from a')

names(a) <- c("CLAC1_NM","buym4","buym5","buym6","buym7","buym8","buym9","b_index4","b_index5","b_index6","b_index7","b_index8","b_index9")
names(b) <- c("CLAC1_NM","count4","count5","count6","count7","count8","count9","c_index4","c_index5","c_index6","c_index7","c_index8","c_index9")
names(c) <- c("CLAC1_NM","id_count4","id_count5","id_count6","id_count7","id_count8","id_count9","i_index4","i_index5","i_index6","i_index7","i_index8","i_index9")



pack <- sqldf('select a.CLAC1_NM,a.b_index4,a.b_index5,a.b_index6,a.b_index7,a.b_index8,a.b_index9, b.c_index4, b.c_index5,b.c_index6,b.c_index7,b.c_index8,b.c_index9 from a join b using(CLAC1_NM)')

fpack <- sqldf('select p.CLAC1_NM,p.b_index4,p.b_index5,p.b_index6,p.b_index7,p.b_index8,p.b_index9, p.c_index4, p.c_index5,p.c_index6,p.c_index7,p.c_index8,p.c_index9 ,c.i_index4,c.i_index5,c.i_index6,c.i_index7,c.i_index8,c.i_index9 from pack p join c using(CLAC1_NM)')

findex <- sqldf('select (0.3*b_index5 + 0.2*c_index5 + 0.5*i_index5) as index5 from fpack group by CLAC3_NM')


findex <- sqldf('select CLAC1_NM,
                (0.3*b_index4 + 0.2*c_index4 + 0.5*i_index4) as index4,
                (0.3*b_index5 + 0.2*c_index5 + 0.5*i_index5) as index5,
                (0.3*b_index6 + 0.2*c_index6 + 0.5*i_index6) as index6,
                (0.3*b_index7 + 0.2*c_index7 + 0.5*i_index7) as index7, 
                (0.3*b_index8 + 0.2*c_index8 + 0.5*i_index8) as index8, 
                (0.3*b_index9 + 0.2*c_index9 + 0.5*i_index9) as index9 
                from fpack group by CLAC1_NM')

write.csv(findex, 'index.csv')