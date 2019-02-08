


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

season <- function(source,apmc, commodity,attribute,plot = FALSE,acf_test = FALSE ){
    data1 <- subset(source, APMC == apmc)
    data1 <- subset(data1, Commodity == commodity)
    data1$arrivals_in_qtl = tsclean(ts(data1[, c('arrivals_in_qtl')]))
    data1$min_price = tsclean(ts(data1[, c('min_price')]))
    data1$max_price = tsclean(ts(data1[, c('max_price')]))
    data1$modal_price = tsclean(ts(data1[, c('modal_price')]))

    count_ma = ts(na.omit(data1[[attribute]]),frequency = 3)
    decomp = stl(count_ma,"periodic")
    
    deseasonal_cnt <- seasadj(decomp)
    
    if(plot ==  TRUE){
        plot(decomp) 
    }
    if(acf_test == TRUE){
        print(adf.test(count_ma,alternative = "stationary"))
    }
    count_dl = diff(deseasonal_cnt,differences = 1)
}

 a <- season(source = data, apmc = "Ahmednagar",commodity = "Bajri",attribute = "arrivals_in_qtl",plot = TRUE,acf_test = TRUE)
