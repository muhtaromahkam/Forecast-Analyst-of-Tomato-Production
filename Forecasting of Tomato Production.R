#Mengimport library yang diperlukan
library(forecast)
library(TSA)
library(tseries)
library(readxl)
library(ggplot2)

#Import data dari file lokal
data1=read_excel("C:/Education/Bahan/Produksi Tomat Lambar 5 Tahun.xlsx", col_types = c("numeric"))
data1

#Membuat data menjadi deret waktu
data1.ts=ts(data1,frequency = 12, start = c(2018,1))
data1.ts

#Plotting data awal serta plotting dengan menggunakan garis trend
plot(data1.ts, main="Produksi Tomat Kabupaten Lampung Barat Tahun 2018-2022")
abline(reg=lm(data1.ts~time(data1.ts)))

#Plot ACF dan Plot PACF untuk melihat apakah data stasioner secara korelogram
Acf(data1.ts, main="Produksi Tomat Kabupaten Lampung Barat 2018-2022")
Pacf(data1.ts, main="Produksi Tomat Kabupaten Lampung Barat 2018-2022")

#Uji Stasioner data terhadap ragam
lamda=BoxCox.lambda(data1.ts)
lamda
BoxCox.ar(data1.ts)

#Uji Stasioner data terhadap rata-rata
adf.test(data1.ts)
kpss.test(data1.ts)


#Data didifferencing satu kali
data1.diff1=diff(data1.ts,differences=1)
plot(data1.diff1)

#Menguji kembali stasioneritas dengan menggunakan uji stasioner
adf.test(data1.diff1)
kpss.test(data1.diff1)

#Melihat plot acf dan plot pacf untuk mengidentifikasi model
Acf(data1.diff1, main="Produksi Tomat Kabupaten Lampung Barat 2018-2022")
Pacf(data1.diff1, main="Produksi Tomat Kabupaten Lampung Barat 2018-2022")

#model (0,1,1)
model_1=Arima(data1.ts,order = c(0,1,1))
model_1
model_1$fitted

#model (1,1,0)
model_2=Arima(data1.ts,order = c(1,1,0))
model_2
model_2$fitted

#model (2,1,0)
model_3=Arima(data1.ts,order = c(2,1,0))
model_3
model_3$fitted

#model (1,1,1)
model_4=Arima(data1.ts,order = c(1,1,1))
model_4
model_4$fitted

#model (2,1,1)
model_5=Arima(data1.ts,order = c(2,1,1))
model_5
model_5$fitted

#Estimasi model yang memiliki nilai akurasi terbaik
MAD_1=mean(abs(data1.ts-model_1$fitted))
MAD_1
MAPE_1=mean(abs(data1.ts-model_1$fitted)/data1.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(data1.ts-model_1$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)

MAD_2=mean(abs(data1.ts-model_2$fitted))
MAD_2
MAPE_2=mean(abs(data1.ts-model_2$fitted)/data1.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(data1.ts-model_2$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(data1.ts-model_3$fitted))
MAD_3
MAPE_3=mean(abs(data1.ts-model_3$fitted)/data1.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(data1.ts-model_3$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

MAD_4=mean(abs(data1.ts-model_4$fitted))
MAD_4
MAPE_4=mean(abs(data1.ts-model_4$fitted)/data1.ts,na.rm=TRUE)
MAPE_4
MSE_4=mean(abs(data1.ts-model_4$fitted)^2)
MSE_4
perbandingan_4=c(MAD_4,MAPE_4,MSE_4)
perbandingan_4

MAD_5=mean(abs(data1.ts-model_5$fitted))
MAD_5
MAPE_5=mean(abs(data1.ts-model_5$fitted)/data1.ts,na.rm=TRUE)
MAPE_5
MSE_5=mean(abs(data1.ts-model_5$fitted)^2)
MSE_5
perbandingan_5=c(MAD_5,MAPE_5,MSE_5)
perbandingan_5

#Membuat perbandingan nilai MAD,MAPE,MSE dari model 1-5
perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3,perbandingan_4,perbandingan_5)
perbandingantotal

#Evaluasi parameter model
#Uji Homogen residual
Box.test(model_5$residuals,type = "Ljung")
#Uji Normalitas residual
ks.test(model_5$residuals,"pnorm",mean(model_5$residuals),sd(model_5$residuals))

#peramalan
ramalan_1=forecast::forecast(model_5,h=6)
ramalan_1
plot(ramalan_1)

#hasil ramalan
m=fitted(model_5)^2
m
lines(fitted(model_5)^2,col="red")
legend("topleft",c("Data Aktual","Data Prediksi","Data peramalan 6 Bulan"),
       lty=1,col=c("black","red","blue"), cex=0.5)
ramalan_2=forecast(model_5,h=6)
ramalan_2
plot(ramalan_2)