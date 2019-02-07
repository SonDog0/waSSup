#####수요 트랜드 예측
#<시계열분석 사용 코드>
library(data.table)
library(forecast)
library(TTR)
library(tseries)

library(lubridate)

setwd('c:/Java/Lpoint')

blouse <- fread('blouse.txt', header = T, sep='\t')

blouse$date <- as.factor(blouse$date)
blouse$index <- as.numeric(blouse$index)

ts.blouse <- ts(blouse[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.blouse))

model1 <- auto.arima(ts.blouse)
predicts1 <- forecast(model1, h=4)
predicts1
plot(predicts1)

# Point Forecast     Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       18.25658 10.352832 26.16033 6.168840 30.34432
# 2019.0027       19.63091  9.768030 29.49379 4.546936 34.71489
# 2019.0055       19.63091  9.101021 30.16080 3.526834 35.73499
# 2019.0082       19.63091  8.473818 30.78800 2.567609 36.69421


cardigan <- fread('cardigan.txt', header = T, sep='\t')

cardigan$date <- as.factor(cardigan$date)
cardigan$index <- as.numeric(cardigan$index)

ts.cardigan <- ts(cardigan[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.cardigan))

model2 <- auto.arima(ts.cardigan)
predicts2 <- forecast(model2, h=4)
predicts2
plot(predicts2)

# Point Forecast    Lo 80    Hi 80      Lo 95    Hi 95
# 2019.0000       13.77765 8.353433 19.20186  5.4820270 22.07327
# 2019.0027       14.52582 7.090241 21.96140  3.1540823 25.89756
# 2019.0055       13.51009 4.663075 22.35710 -0.0202497 27.04042
# 2019.0082       12.60115 2.795112 22.40718 -2.3958895 27.59819

coat <- fread('coat.txt', header = T, sep='\t')

coat$date <- as.factor(coat$date)
coat$index <- as.numeric(coat$index)

ts.coat <- ts(coat[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.coat))

model3 <- auto.arima(ts.coat)
predicts3 <- forecast(model3, h=4)
predicts3
plot(predicts3)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       30.45683 23.97695 36.93672 20.54671 40.36696
# 2019.0027       34.79280 26.18217 43.40343 21.62398 47.96162
# 2019.0055       34.79280 25.28018 44.30542 20.24450 49.34110
# 2019.0082       34.79280 24.45660 45.12900 18.98495 50.60065

jacket <- fread('jacket.txt', header = T, sep='\t')

jacket$date <- as.factor(jacket$date)
jacket$index <- as.numeric(jacket$index)

ts.jacket <- ts(jacket[,2], start=c(2017,1), frequency = 365)
plot(decompose(ts.jacket))

model4 <- auto.arima(ts.jacket)
predicts4 <- forecast(model4, h=4)
predicts4
plot(predicts4)

# Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
# 2019.0000       7.925714 -2.358417 18.20985 -7.802508 23.65394
# 2019.0027       8.572539 -2.532744 19.67782 -8.411525 25.55660
# 2019.0055       8.994886 -2.636020 20.62579 -8.793049 26.78282
# 2019.0082       9.182875 -3.215284 21.58103 -9.778474 28.14422

jean <- fread('jean.txt', header = T, sep='\t')

jean$date <- as.factor(jean$date)
jean$index <- as.numeric(jean$index)

ts.jean <- ts(jean[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.jean))

model5 <- auto.arima(ts.jean)
predicts5 <- forecast(model5, h=4)
predicts5
plot(predicts5)

# Point Forecast    Lo 80    Hi 80      Lo 95    Hi 95
# 2019.0000       16.00236 9.170293 22.83442  5.5536156 26.45110
# 2019.0027       16.48559 7.512187 25.45899  2.7619548 30.20922
# 2019.0055       16.92883 6.550588 27.30708  1.0566764 32.80099
# 2019.0082       17.34437 5.900282 28.78846 -0.1578537 34.84660

jump <- fread('jump.txt', header = T, sep='\t')

jump$date <- as.factor(jump$date)
jump$index <- as.numeric(jump$index)

ts.jump <- ts(jump[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.jump))

model6 <- auto.arima(ts.jump)
predicts6 <- forecast(model6, h=4)
predicts6
plot(predicts6)

# Point Forecast      Lo 80     Hi 80     Lo 95    Hi 95
# 2019.0000       3.847774 -0.3360022  8.031550 -2.550759 10.24631
# 2019.0027       3.945763 -1.0931919  8.984718 -3.760654 11.65218
# 2019.0055       4.089672 -1.5758721  9.755217 -4.575030 12.75437
# 2019.0082       3.921195 -2.3278742 10.170265 -5.635932 13.47832

jumper <- fread('jumper.txt', header = T, sep='\t')

jumper$date <- as.factor(jumper$date)
jumper$index <- as.numeric(jumper$index)

ts.jumper <- ts(jumper[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.jumper))

model7 <- auto.arima(ts.jumper)
predicts7 <- forecast(model7, h=4)
predicts7
plot(predicts7)

# Point Forecast      Lo 80     Hi 80     Lo 95    Hi 95
# 2019.0000       3.847774 -0.3360022  8.031550 -2.550759 10.24631
# 2019.0027       3.945763 -1.0931919  8.984718 -3.760654 11.65218
# 2019.0055       4.089672 -1.5758721  9.755217 -4.575030 12.75437
# 2019.0082       3.921195 -2.3278742 10.170265 -5.635932 13.47832

leggings <- fread('leggings.txt', header = T, sep='\t')

leggings$date <- as.factor(leggings$date)
leggings$index <- as.numeric(leggings$index)

ts.leggings <- ts(leggings[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.leggings))

model8 <- auto.arima(ts.leggings)
predicts8 <- forecast(model8, h=4)
predicts8
plot(predicts8)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       38.81341 33.13760 44.48922 30.13300 47.49382
# 2019.0027       40.15200 32.21711 48.08688 28.01664 52.28735
# 2019.0055       38.63230 29.29932 47.96527 24.35874 52.90585
# 2019.0082       35.43341 25.17554 45.69128 19.74535 51.12147

one <- fread('one.txt', header = T, sep='\t')

one$date <- as.factor(one$date)
one$index <- as.numeric(one$index)

ts.one <- ts(one[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.one))

model9 <- auto.arima(ts.one)
predicts9 <- forecast(model9, h=4)
predicts9
plot(predicts9)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       18.37652 11.89720 24.85585 8.467253 28.28579
# 2019.0027       20.22555 12.27100 28.18010 8.060111 32.39099
# 2019.0055       20.22555 11.61699 28.83411 7.059888 33.39121
# 2019.0082       20.22555 11.00927 29.44183 6.130465 34.32063

outer <- fread('outer.txt', header = T, sep='\t')

outer$date <- as.factor(outer$date)
outer$index <- as.numeric(outer$index)

ts.outer <- ts(outer[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.outer))

model10 <- auto.arima(ts.outer)
predicts10 <- forecast(model10, h=4)
predicts10
plot(predicts10)

# Point Forecast    Lo 80    Hi 80     Lo 95    Hi 95
# 2019.0000       19.29138 13.50000 25.08277 10.434219 28.14855
# 2019.0027       19.91364 12.39279 27.43450  8.411489 31.41580
# 2019.0055       20.28633 11.67053 28.90213  7.109606 33.46305
# 2019.0082       20.50954 11.07383 29.94525  6.078869 34.94021

padding <- fread('padding.txt', header = T, sep='\t')

padding$date <- as.factor(padding$date)
padding$index <- as.numeric(padding$index)

ts.padding <- ts(padding[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.padding))

model11 <- auto.arima(ts.padding)
predicts11 <- forecast(model11, h=4)
predicts11
plot(predicts11)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       30.08159 25.35197 34.81121 22.84826 37.31492
# 2019.0027       28.24045 20.98534 35.49556 17.14472 39.33618
# 2019.0055       30.21905 20.95852 39.47959 16.05629 44.38182
# 2019.0082       29.34875 18.74344 39.95405 13.12933 45.56816

pants <- fread('pants.txt', header = T, sep='\t')

pants$date <- as.factor(pants$date)
pants$index <- as.numeric(pants$index)

ts.pants <- ts(pants[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.pants))

model12 <- auto.arima(ts.pants)
predicts12 <- forecast(model12, h=4)
predicts12
plot(predicts12)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       44.32992 32.71570 55.94414 26.56751 62.09233
# 2019.0027       47.02823 33.98388 60.07258 27.07862 66.97784
# 2019.0055       47.95071 34.30742 61.59400 27.08511 68.81632
# 2019.0082       48.26608 34.22119 62.31098 26.78627 69.74590

sapari <- fread('sapari.txt', header = T, sep='\t')

sapari$date <- as.factor(sapari$date)
sapari$index <- as.numeric(sapari$index)

ts.sapari <- ts(sapari[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.sapari))

model13 <- auto.arima(ts.sapari)
predicts13 <- forecast(model13, h=4)
predicts13
plot(predicts13)

# Point Forecast     Lo 80    Hi 80      Lo 95    Hi 95
# 2019.0000       3.694309 -3.976148 11.36477  -8.036643 15.42526
# 2019.0027       3.842856 -5.803640 13.48935 -10.910187 18.59590
# 2019.0055       4.042459 -6.875888 14.96081 -12.655711 20.74063
# 2019.0082       4.238240 -7.777314 16.25379 -14.137963 22.61444

shirt <- fread('shirt.txt', header = T, sep='\t')

shirt$date <- as.factor(shirt$date)
shirt$index <- as.numeric(shirt$index)

ts.shirt <- ts(shirt[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.shirt))

model14 <- auto.arima(ts.shirt)
predicts14 <- forecast(model14, h=4)
predicts14
plot(predicts14)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       15.84632 6.765368 24.92727  1.95820265 29.73444
# 2019.0027       16.56087 5.704186 27.41756 -0.04299746 33.16474
# 2019.0055       16.84869 5.185328 28.51204 -0.98887988 34.68625
# 2019.0082       16.89290 4.735399 29.05040 -1.70039345 35.48619

skirt <- fread('skirt.txt', header = T, sep='\t')

skirt$date <- as.factor(skirt$date)
skirt$index <- as.numeric(skirt$index)

ts.skirt <- ts(skirt[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.skirt))

model15 <- auto.arima(ts.skirt)
predicts15 <- forecast(model15, h=4)
predicts15
plot(predicts15)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       39.09537 24.25161 53.93912 16.39381 61.79693
# 2019.0027       40.55661 25.14705 55.96618 16.98971 64.12351
# 2019.0055       40.73144 25.05936 56.40352 16.76306 64.69981
# 2019.0082       40.75235 24.84885 56.65586 16.43005 65.07466


sweat_shirt <- fread('sweat_shirt.txt', header = T, sep='\t')

sweat_shirt$date <- as.factor(sweat_shirt$date)
sweat_shirt$index <- as.numeric(sweat_shirt$index)

ts.sweat_shirt <- ts(sweat_shirt[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.sweat_shirt))

model16 <- auto.arima(ts.sweat_shirt)
predicts16 <- forecast(model16, h=4)
predicts16
plot(predicts16)

# Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
# 2019.0000       4.263041 -1.269307  9.79539 -4.197956 12.72404
# 2019.0027       4.732747 -1.352015 10.81751 -4.573094 14.03859
# 2019.0055       5.150346 -1.351279 11.65197 -4.793032 15.09372
# 2019.0082       5.521619 -1.303046 12.34628 -4.915806 15.95904

sweater <- fread('sweater.txt', header = T, sep='\t')

sweater$date <- as.factor(sweater$date)
sweater$index <- as.numeric(sweater$index)

ts.sweater <- ts(sweater[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.sweater))

model17 <- auto.arima(ts.sweater)
predicts17 <- forecast(model17, h=4)
predicts17
plot(predicts17)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       43.60900 37.26326 49.95474 33.90403 53.31397
# 2019.0027       42.71115 35.75352 49.66879 32.07037 53.35194
# 2019.0055       42.84834 35.33071 50.36598 31.35111 54.34558
# 2019.0082       43.84480 35.96314 51.72646 31.79084 55.89876

t_shirt <- fread('t_shirt.txt', header = T, sep='\t')

t_shirt$date <- as.factor(t_shirt$date)
t_shirt$index <- as.numeric(t_shirt$index)

ts.t_shirt <- ts(t_shirt[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.t_shirt))

model18 <- auto.arima(ts.t_shirt)
predicts18 <- forecast(model18, h=4)
predicts18
plot(predicts18)

# Point Forecast     Lo 80    Hi 80     Lo 95     Hi 95
# 2019.0000      0.3381376 -5.673607 6.349882 -8.856033  9.532308
# 2019.0027      0.4341706 -5.942569 6.810910 -9.318210 10.186552
# 2019.0055      0.4912439 -5.940867 6.923354 -9.345820 10.328308
# 2019.0082      0.4940604 -5.949231 6.937352 -9.360104 10.348225

trench <- fread('trench.txt', header = T, sep='\t')

trench$date <- as.factor(trench$date)
trench$index <- as.numeric(trench$index)

ts.trench <- ts(trench[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.trench))

model19 <- auto.arima(ts.trench)
predicts19 <- forecast(model19, h=4)
predicts19
plot(predicts19)

# Point Forecast     Lo 80    Hi 80      Lo 95     Hi 95
# 2019.0000    -0.15929029 -3.550552 3.231971  -5.345778  5.027197
# 2019.0027    -0.19398170 -4.918502 4.530539  -7.419512  7.031549
# 2019.0055     0.12655972 -5.758348 6.011468  -8.873630  9.126750
# 2019.0082     0.09269554 -6.819010 7.004401 -10.477846 10.663237

vest <- fread('vest.txt', header = T, sep='\t')

vest$date <- as.factor(vest$date)
vest$index <- as.numeric(vest$index)

ts.vest <- ts(vest[,2], start = c(2017,1), frequency = 365)
plot(decompose(ts.vest))

model20 <- auto.arima(ts.vest)
predicts20 <- forecast(model20, h=4)
predicts20
plot(predicts20)

# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2019.0000       32.20163 24.00429 40.39898 19.66488 44.73839
# 2019.0027       32.16734 22.52607 41.80861 17.42229 46.91239
# 2019.0055       30.23513 19.20605 41.26421 13.36760 47.10265
# 2019.0082       29.82988 17.67436 41.98540 11.23962 48.42014
