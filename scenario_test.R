setwd("D:\\Study\\Ascension Practicum\\1118")
library("readxl")
source("bus_day_count.R")
library("xlsx")
## Original Main is stored in Main_Copy
Main=data.frame(Main_copy)
#Main=Main[Main$date>=as.Date('2018-01-01'),]

#position <- read_excel("Data.xlsx",sheet = "Position")
#prm <- read_excel("Data.xlsx",sheet = "Premium")
#cash <- read_excel("Data.xlsx",sheet = "Cashflow")

position <- data.frame()
prm <- data.frame()
cash <- data.frame()

# Reset long indicator
Main$long_signal=FALSE

for (i in 1:length(Main$date)){
  if (Main$date[i] %in% first_business_days){
    ## If no remaining positions, initiate new short positions
    if (dim(position)[1] == 0){
      Main$short_signal = TRUE
      port_ID <- paste(strftime(Main$date[i], "%m"),strftime(Main$date[i], "%d"),strftime(Main$date[i], "%y"),upper,lower,sep = "")
      put_notional=Main$last_spx[[i]]
      call_strike = (1+moneyness/100)*Main$last_spx[i]
      put_strike = (1-moneyness/100)*Main$last_spx[i]
      if (Main$date[i]==as.Date('2019-08-01')){
        expiry_date = as.Date('2019-09-02')
      } else {
        expiry_date = as.Date(ifelse(is.null(first_business_days[as.numeric(match(Main$date[i], first_business_days)) + 1][[1]]),"",c(first_business_days[as.numeric(match(Main$date[i], first_business_days)) + 1][[1]])), origin="1970-01-01")
      }
      position <- rbind(position, data.frame(port_ID = port_ID,
                                             long_position = 0,
                                             short_position = 1,
                                             net_position = -1,
                                             put_notional=put_notional,
                                             call_strike = call_strike,
                                             put_strike = put_strike,
                                             expiry_date = expiry_date,
                                             enter_price = Call(put_notional, call_strike, Main$rate[i]/100, as.numeric(expiry_date-Main$date[[i]])/360, Main$iv_call[i]/100)+Put(put_notional, put_strike, Main$rate[i]/100, as.numeric(expiry_date-Main$date[i])/360, Main$iv_put[i]/100)
                                             #quit_price = ""#Put(S,K,r,T,sigma) - Call(S,K,r,T,sigma)
      ))
    }else{
      ## Else, first remove remaining position at first business day of next month
      Main$long_signal[i] = TRUE
      # Since the port ID will not be updated until next month start, it's safe to use the existing Port_ID to update positions
      # Move this trade to the PRM df
      premium_expire = position[position$port_ID==port_ID,'enter_price']-max(c(0,Main$last_spx[[i]]-position[position$port_ID==port_ID,'call_strike'],position[position$port_ID==port_ID,'put_strike']-Main$last_spx[[i]]))
      days_within = as.numeric(Main$date[i] - as.Date(paste(substr(port_ID, start = 1, stop = 2),substr(port_ID, start = 3, stop = 4),substr(port_ID, start = 5, stop = 6),sep = "/"),format = "%m/%d/%y"))
      prm <- rbind(prm, data.frame(port_ID = port_ID,
                                   trade_date = Main$date[i],
                                   premium_early_exit = 0,
                                   premium_expire = premium_expire, 
                                   days_within = days_within,
                                   put_notional=put_notional,
                                   early_exit_profit_pct=0,
                                   expire_profit_pct=premium_expire/put_notional
      ))
      position <- position[!(port_ID == port_ID),] # clr the positions
      
      ## Initiate new short positions
      Main$short_signal = TRUE
      port_ID <- paste(strftime(Main$date[i], "%m"),strftime(Main$date[i], "%d"),strftime(Main$date[i], "%y"),upper,lower,sep = "")
      put_notional=Main$last_spx[[i]]
      call_strike = (1+moneyness/100)*Main$last_spx[i]
      put_strike = (1-moneyness/100)*Main$last_spx[i]
      ## Ad-hoc detect if it is 2019-08-01, which is the first biz day of the last month
      if (Main$date[i]==as.Date('2019-08-01')){
        expiry_date = as.Date('2019-09-02')
      } else {
        expiry_date = as.Date(ifelse(is.null(first_business_days[as.numeric(match(Main$date[i], first_business_days)) + 1][[1]]),"",c(first_business_days[as.numeric(match(Main$date[i], first_business_days)) + 1][[1]])), origin="1970-01-01")
      }
      position <- rbind(position, data.frame(port_ID = port_ID,
                                             long_position = 0,
                                             short_position = 1,
                                             net_position = -1,
                                             put_notional=put_notional,
                                             call_strike = call_strike,
                                             put_strike = put_strike,
                                             expiry_date = expiry_date,
                                             enter_price = Call(put_notional, call_strike, Main$rate[i]/100, as.numeric(expiry_date-Main$date[[i]])/360, Main$iv_call[i]/100)+Put(put_notional, put_strike, Main$rate[i]/100, as.numeric(expiry_date-Main$date[i])/360, Main$iv_put[i]/100)
                                             #quit_price = ""#Put(S,K,r,T,sigma) - Call(S,K,r,T,sigma)
      ))   
    }
  }
  else{
    ## If not 1st biz day, no expire/new position, only check if early exit
    
    k = i + 2
    if (k <= length(Main$date)){
      Main$long_indicator[k] = Main$last_spx[k]/Main$last_spx[k-1]
      if ((Main$long_indicator[k] >1.015) && (Main$long_indicator[k-1] >1.015) && (Main$date[k]<expiry_date) && port_ID != "") {
        #print("not natural")
        Main$long_signal[k] = TRUE
        # Since the port ID will not be updated until next month start, it's safe to use the existing Port_ID to update positions
        # Move this trade to the PRM df
        #if (expiry_date==as.Date('2019-09-02')){
        #  premium_expire=0
        #} else {
        #  premium_expire = position[position$port_ID==port_ID,'enter_price']-max(c(0,Main[Main$date==expiry_date,'last_spx']-position[position$port_ID==port_ID,'call_strike'],position[position$port_ID==port_ID,'put_strike']-Main[Main$date==expiry_date,'last_spx']))
        #}
        days_within_expire = as.numeric(expiry_date - as.Date(paste(substr(port_ID, start = 1, stop = 2),substr(port_ID, start = 3, stop = 4),substr(port_ID, start = 5, stop = 6),sep = "/"),format = "%m/%d/%y"))
        premium_early_exit = position[position$port_ID==port_ID,'enter_price']-(Call(Main$last_spx[k], position[position$port_ID==port_ID,'call_strike'], Main$rate[k]/100, as.numeric(expiry_date-Main$date[k])/360, Main$iv_call[k]/100)+Put(Main$last_spx[k], position[position$port_ID==port_ID,'put_strike'], Main$rate[k]/100, as.numeric(expiry_date-Main$date[k])/360, Main$iv_put[k]/100))
        days_within_early_exit = as.numeric(Main$date[k] - as.Date(paste(substr(port_ID, start = 1, stop = 2),substr(port_ID, start = 3, stop = 4),substr(port_ID, start = 5, stop = 6),sep = "/"),format = "%m/%d/%y"))
        prm <- rbind(prm, data.frame(port_ID = port_ID,
                                     trade_date = Main$date[k],
                                     premium_early_exit = premium_early_exit,
                                     premium_expire = 0,
                                     days_within = days_within_early_exit,
                                     put_notional=put_notional,
                                     early_exit_profit_pct=premium_early_exit/put_notional,
                                     expire_profit_pct=0
                                    ))
        position <- position[!(port_ID == port_ID),] # clr the positions
        port_ID = ""
      }
    }
  }
  # Calculate cash/market value;
  if (Main$date[i] %in% first_business_days){
    # Result has already been included in the prm dataset
    market_value=0
  } else {
    if (port_ID==''){
      market_value=0
    } else {
      market_value=position[position$port_ID==port_ID,'enter_price']-(Call(Main$last_spx[i], position[position$port_ID==port_ID,'call_strike'], Main$rate[i]/100, as.numeric(position[position$port_ID==port_ID,'expiry_date']-Main$date[i])/360, Main$iv_call[i]/100)+Put(Main$last_spx[i], position[position$port_ID==port_ID,'put_strike'], Main$rate[i]/100, as.numeric(position[position$port_ID==port_ID,'expiry_date']-Main$date[i])/360, Main$iv_put[i]/100))
    }
  }
  cash <- rbind(cash, data.frame(Port_ID = port_ID,
                                 date = Main$date[i],
                                 earned_cash = sum(prm$premium_early_exit,prm$premium_expire),
                                 market_value = market_value,
                                 total_value = sum(prm$premium_early_exit,prm$premium_expire,market_value)
  ))
}

plot(cash$date,cash$total_value,type='l')
write.xlsx(position,'position.xlsx')
write.xlsx(prm,'prm.xlsx')
write.xlsx(cash,'cash.xlsx')
