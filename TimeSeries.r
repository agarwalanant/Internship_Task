
library("rio")
library("ggplot2")
library("forecast")
library("tseries")
library("tidyverse")
library(readxl)
library(zoo)

data <- read.csv("Monthly_data_cmo.csv")
data <- data.frame(data)

data <- data[order(data[,9]),]

data <- subset(data, select =-c(state_name))

data$arrivals_in_qtl = tsclean(ts(data[, c('arrivals_in_qtl')]))
data$min_price = tsclean(ts(data[, c('min_price')]))
data$max_price = tsclean(ts(data[, c('max_price')]))
data$modal_price = tsclean(ts(data[, c('modal_price')]))

data1 <- subset(data, APMC =="Ahmednagar")
# data2015 <- subset(data, Year ==2015)
# data2016 <- subset(data, Year ==2016)

data1 <- subset(data1, Commodity =="Bajri")

data1$arrivals_in_qtl = tsclean(ts(data1[, c('arrivals_in_qtl')]))

data1$min_price = tsclean(ts(data1[, c('min_price')]))

data1$max_price = tsclean(ts(data1[, c('max_price')]))
data1$modal_price = tsclean(ts(data1[, c('modal_price')]))

head(data1)

str(data1)

ggplot() +
geom_line(data= data1 , aes(y = arrivals_in_qtl , x= as.numeric(date)))

ggplot() +
geom_line(data= data1 , aes(y = min_price , x= as.numeric(date)))

ggplot() +
geom_line(data= data1 , aes(y = max_price , as.numeric(date)))

ggplot() +
geom_line(data= data1 , aes(y = modal_price , as.numeric(date)))

count_ma = ts(na.omit(data1$arrivals_in_qtl),frequency = 3)
decomp = stl(count_ma,"periodic")


deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(count_ma,alternative = "stationary")

Acf(count_ma, main='')

Pacf(count_ma, main='')

count_dl = diff(deseasonal_cnt,differences = 1)
plot(count_dl)

adf.test(count_dl,alternative = "stationary")

Acf(count_dl, main='')
Pacf(count_dl, main='')
