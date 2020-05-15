

library(dplyr)
library(zoo)
library(plotly)
library(stringr)
setwd("C:/Users/Dell/Desktop/FL2019/FINPrac")


## Feed
feed = function(moneyness){
  upper = toString(100+moneyness);
  lower = toString(100-moneyness);
  upperindex = as.character(paste("X",upper,".Last",sep=""))
  lowerindex = as.character(paste("X",lower,".Last",sep=""))
  
  data <- read.csv(file="SPX_1M.csv", header=TRUE,stringsAsFactors = FALSE)
  data = data[!is.na(data$X100.Last),]
  ClosingPrice = data[c("Weekday","Date",upperindex,lowerindex,"SPX.Last")]
  ## as.date ------- requires editting
  ClosingPrice['Date']<-as.Date(ClosingPrice$Date,format="%Y/%m/%d")
  ### Read in 1M T-bill from FRED and append to existing data
  rate <- read.csv(file="DGS1MO.csv", header=TRUE,stringsAsFactors = FALSE)
  rate['DATE']<-as.Date(rate$DATE,format="%Y-%m-%d")
  ClosingPrice<-merge(ClosingPrice,rate,by.x='Date',by.y='DATE',all.x=TRUE)
  colnames(ClosingPrice)[colnames(ClosingPrice)=="DGS1MO"] <- "rate"
  return (ClosingPrice);
}

## setClass("Strangle", slots = list(date="character", strike = "numeric", quant = "numeric"));
## setClass("Portfolio", slots = list(wealth="numeric"), contains="Strangle");

## this feed extracts data from feed function
maturity = function(Portfolio,date,feed,wealth){
  tenor = 30;
  for (d in 1:nrow(Portfolio)){
    staringDate = as.Date(Portfolio$Date[d],format="%Y/%m/%d");
    if(startingDate == date - 30){
      wealth = wealth - max(0,Portfolio[d]["lowerstrike"]-feed[d]['SPX.Last'])
                -max(0,feed[d]['SPX.Last'] - Portfolio[d]["upperstrike"]) ## this is the price in
      Portfoio = Portfoio[-d,]
    }
  }
  return;
}


## strategy codes(function) add in this class, output signal with long/short and quant
setClass("Strategy", slots = list(signal = "numeric"));

theStrategy = function(feed){
  value = "01-01"
  for(t in 1:nrow(feed)){
    if(grepl(feed[t,1],value)){
      strategy1.signal = 1;
      feed$signal[t] = 1;
    }else{
      strategy1.singal = -1;
      feed$signal[t] = -1;
    }
  }
  return;
}


##Black Scholes 
Call <- function(S, K, r, T, sigma) {
  d1  <-  (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T)
  return (S * pnorm(d1)  - K*exp(-r*T)*pnorm(d2))
}

Put <- function(S, K, r, T, sigma) {
  d1  <-  (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T)
  return  (-S * pnorm(-d1) + K*exp(-r*T)*pnorm(-d2))
}


## sell all, but 1
sell = function(Portfolio, date, feed, wealth,impliedVolSurf){
  S = feed[which(feed$Date==date)][5]
  r = feed[which(feed$Date==date)][6]
  feed$totalWealth = wealth + call(S,upperK,r,remainMty,impliedVolcall) - feed$SPX.Last[d] + put(S, lowerK, r, remainMty,impliedVolput)-feed$SPX.Last[d];
  wealth = feed$totalWealth;
  ## S is the SPC_Last, r is the Rf, 
  upperstrike=as.numeric(str_extract(names(feed)[3]))
  lowerstrike=as.numeric(str_extract(names(feed)[4]))
  Portfolio<-rbind(Portfolio,as.character(date),upperstrike,lowerstrike,1);
  return;
}#####
  

buy = function(Portfolio, date, feed, wealth,impliedVolSurf){
  S = feed[which(feed$Date==date)][5]
  r = feed[which(feed$Date==date)][6]
  feed$totalWealth = wealth - call(S,upperK,r,remainMty,impliedVolcall) - put(S, lowerK, r, remainMty,impliedVolput);
  wealth = feed$totalWealth[d]
  Portfolio = Portfolio[-d,]
    ##for t in 1:nrow(Portfolio){
  ##upperK = Portfolio[t]["upperStrike"]
  ##lowerK = Portfolio[t]["lowerStrike"]
  ##remainMty = Portfolio[t]["date"] + 30 - date;
  ##impliedVolcall = impliedVolSurf(S/upperK,remainMty)
  ##impliedVolput = impliedVolSurf(S/lowerK,remainMty)
  ##wealth = wealth - call(S,upperK,r,remainMty,impliedVolcall) - put(S, lowerK, r, remainMty,impliedVolput);
  ##}
  return;
}


## main function

startingDate = as.Date("2008/01/01",format="%Y/%m/%d");
endDate = as.Date("2008/05/31",format="%Y/%m/%d");
timeDiffernece = as.numeric(endDate-startingDate)
theupperstrike = 110
thelowerstrike = 90
tradingQuant  = 1
Portfolio = data.frame(matrix(NA,nrow=timeDiffernece,ncol=4))
Portfolio[1,1] = startingDate
Portfolio
for(i in 1:nrow(Portfolio)){
  Portfolio[i,1] = startingDate + 1
  Portfolio[i,2] = theupperstrike
  Portfolio[i,3] = thelowerstrike
  Portfolio[i,4] = tradingQuant
}

##Portfolio[1] = data.frame(date = startingDate,upperstrike = theupperstrike,lowerstrike = thelowerstrike, quant = tradingQuant)
##Portfolio = data.frame[matrix(ncol=timeDiffernece, nrow=4)]
##Portfolio = Portfolio[-1,]

wealth = 50000;
feed = feed(moneyness=10);
feed = cbind(feed,signal=0);
feed = cbind(feed,totalWealth=wealth);

for (d in 1:nrow(feed)){
  date = feed[d,1]
  maturity(Portfolio,feed$Date[d],feed,wealth,impliedVolSurf);
  ## strategy 
  theStrategy(feed);
  if(feed$signal[d]==1){
    buy(Portfolio, date, feed, wealth,impliedVolSurf)
  } 
  else{
    sell(Portfolio, date, feed, wealth)
  }
}
