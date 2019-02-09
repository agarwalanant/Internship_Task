
library("rio")
library("ggplot2")
library("forecast")
library("tseries")
library("tidyverse")
library(quantmod)
library(readxl)
library(zoo)


data <- read.csv("Monthly_data_cmo.csv")
data <- data.frame(data)
data <- data[order(data[,9]),]
data <- subset(data, select =-c(state_name))
data$APMC <- tolower(data$APMC)
data$Commodity <- tolower(data$Commodity)
data$Month <- tolower(data$Month)
data$FLAG = 0

for(i in 5:8){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

msp_data <- read.csv("CMO_MSP_Mandi.csv")
map_data <- data.frame(msp_data)
msp_data <- msp_data[order(msp_data[,1]),]
msp_data <- subset(msp_data, select = c(commodity, msprice, year))
msp_data$commodity <- tolower(msp_data$commodity)

flag <- data.frame(APMC = character(1),Commodity = character(1),stringsAsFactors=FALSE)

season <- function(source,apmc, commodity,attribute,freq,plot = FALSE){
    data1 <- subset(source, APMC == as.character(apmc))
    data1 <- subset(data1, Commodity == as.character(commodity))
    
    data1$arrivals_in_qtl = tsclean(ts(data1[, c('arrivals_in_qtl')]))
    data1$min_price = tsclean(ts(data1[, c('min_price')]))
    data1$max_price = tsclean(ts(data1[, c('max_price')]))
    data1$modal_price = tsclean(ts(data1[, c('modal_price')]))

    count_ma = ts((data1[[attribute]]),frequency = freq)
    decomp = stl(count_ma,"periodic")
    
    deseasonal_cnt <- seasadj(decomp)
    count_dl = diff(deseasonal_cnt)
    if(plot ==  TRUE){
        plot(decomp) 
    }
        test <- adf.test(count_ma,alternative = "stationary")$p.value
    if(test > 0.05){

    flag <<- rbind(flag, c(apmc,commodity))
   }
    
    data1[[attribute]] = deseasonal_cnt
    return <- list(data1, test,c(apmc,commodity))
    return
}

final <- function (apmc,comm,attribute){
ans1 <- season(source = data, apmc = as.character(apmc),commodity = as.character(comm) ,attribute = attribute,freq=3)
a <- subset(msp_data, commodity == as.character(comm))
a <- subset(a , year > 2013)

ggplot()+
geom_line(data= data.frame(a) , aes(y = msprice , x= as.numeric(year))) +
geom_line(data= data.frame(ans1[1]) , aes(y = as.numeric(min_price) , x= as.numeric(Year)))
    
    }


apmc_f <- c(unique(data$APMC))

    data1 <- subset(data, APMC == apmc_f[1])
    comm_msp <- c(unique(msp_data$commodity))
    comm_data <- c(unique(data1$Commodity))
    comm_f <- intersect(comm_msp,comm_data)

for(i in 1:340){
    data1 <- subset(data, APMC == apmc_f[i])
    comm_msp <- c(unique(msp_data$commodity))
    comm_data <- c(unique(data1$Commodity))
    comm_f <- intersect(comm_msp,comm_data)
    for(j in comm_f){
        final(apmc =apmc_f[i] ,comm = j,attribute = "min_price")
    }
}

flag
