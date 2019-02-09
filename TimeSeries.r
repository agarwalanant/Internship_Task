
#Libraries import
library("rio")
library("ggplot2")
library("forecast")
library("tseries")
library("tidyverse")
library(readxl)
library(zoo)


# Pre-processing of Monthly CMO data
data <- read.csv("Monthly_data_cmo.csv")
data <- data.frame(data)
data <- data[order(data[,9]),]
data <- subset(data, select =-c(state_name))
# converting the data into lower to maintain consistency
data$APMC <- tolower(data$APMC) 
data$Commodity <- tolower(data$Commodity)
data$Month <- tolower(data$Month)

#Dealing with na values in data replacing with mean of the column
for(i in 5:8){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

# Loading and preprocessing the MSP data.
msp_data <- read.csv("CMO_MSP_Mandi.csv")
map_data <- data.frame(msp_data)
msp_data <- msp_data[order(msp_data[,1]),]
msp_data <- subset(msp_data, select = c(commodity, msprice, year))
msp_data$commodity <- tolower(msp_data$commodity) #converting the values in lower case to maintain consistency.

#Dealing with na values in data replacing with mean of the column
for(i in 5:8){
  msp_data[is.na(msp_data[,i]), i] <- mean(msp_data[,i], na.rm = TRUE)
}

#flag data fram to FLAG the APMC and commodities which are not stable i.e p-value > 0.05.
flag <- data.frame(APMC = character(1),Commodity = character(1),stringsAsFactors=FALSE)

#Function to detect and remove and seasonality of a particular APMC, Commodity and Attribute.
season <- function(source,apmc, commodity,attribute,freq,plot = FALSE){
    data1 <- subset(source, APMC == as.character(apmc))
    data1 <- subset(data1, Commodity == as.character(commodity))
    # making the attributes of interest into timeseries
    data1$arrivals_in_qtl = tsclean(ts(data1[, c('arrivals_in_qtl')]))
    data1$min_price = tsclean(ts(data1[, c('min_price')]))
    data1$max_price = tsclean(ts(data1[, c('max_price')]))
    data1$modal_price = tsclean(ts(data1[, c('modal_price')]))

    count_ma = ts((data1[[attribute]]),frequency = freq)
    decomp = stl(count_ma,"periodic")
    #deseasonalizing the selected atttibute
    deseasonal_cnt <- seasadj(decomp)
    count_dl = diff(deseasonal_cnt)
    if(plot ==  TRUE){
        plot(decomp) #plottnig the decomp (plotting doesn't works with loops in R)
    }
        test <- adf.test(count_ma,alternative = "stationary")$p.value #performinf ADF test for stability
    if(test > 0.05){

    flag <<- rbind(flag, c(apmc,commodity)) # if the attribute is unstable appending to the flag data frame
   }
    
    data1[[attribute]] = deseasonal_cnt #appending the deseasonalized data back to the original data.
    return <- list(data1, test,c(apmc,commodity))
    return
}

#Final driver function for analysis of data.
final <- function (apmc,comm,attribute){
ans1 <- season(source = data, apmc = as.character(apmc),commodity = as.character(comm) ,attribute = attribute,freq=3)
a <- subset(msp_data, commodity == as.character(comm))
a <- subset(a , year > 2013) # matching year value with CMO data.

ggplot()+ # plotting doesn't work with loops
geom_line(data= data.frame(a) , aes(y = msprice , x= as.numeric(year))) +
geom_line(data= data.frame(ans1[1]) , aes(y = as.numeric(min_price) , x= as.numeric(Year)))
    
    }


#Extractnig out Unique APMCs from the data.
apmc_f <- c(unique(data$APMC))

for(i in 1:340){
    #for the particular APMC considering only those Commosity which are present in both datasets as there is inconsistency
    data1 <- subset(data, APMC == apmc_f[i])
    comm_msp <- c(unique(msp_data$commodity))
    comm_data <- c(unique(data1$Commodity))
    comm_f <- intersect(comm_msp,comm_data)
    for(j in comm_f){
        final(apmc =apmc_f[i] ,comm = j,attribute = "min_price")
    }
}

flag #Displaying the flagged attributes
