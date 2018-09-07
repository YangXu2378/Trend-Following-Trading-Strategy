#1. read data
jy = read.table(file ="JY-5min.txt", sep = ",")
xb = read.table(file = "XB-5min.txt",sep = ",")
View(jy)
jy_date = jy$V1
jy_time = jy$V2
jy_open = jy$V3
jy_close = jy$V6
jy_high = jy$V4
jy_low = jy$V5
jy_vol = jy$V7
jy_count = jy$V8
xb_date = xb$V1
xb_time = xb$V2
xb_open = xb$V3
xb_close = xb$V6
xb_high = xb$V4
xb_low = xb$V5
xb_vol = xb$V7
xb_count = xb$V8

#calculate drawdown
drwadown = function (portfolio){
  n = 
}
# MAX

run_max = function(tau, high){
  
  high = rep(1:8,100)
  tau = 8
  n = length(high)
  runningMax = rep(0,n)
  runningMax[tau] =  max(high[1:tau])
  new_tau = tau+1
  for (i in new_tau :n) {
    if (high[i] >= runningMax[i-1]){
      runningMax[i] = high[i]
    } 
    else if (runningMax[i-1] == high[i-tau])
    {
    index = i-tau+1
    runningMax[i] = max(high[index:i])
    else{
      runningMax[i]= runningMax[i-1]
    }
  }
}


### MIN
run_min = function(tau, low){
  
  low = rep(1:8,100)
  tau = 8
  n = length(low)
  runningMin = rep(0,n)
  runningMin[tau] =  min(low[1:tau])
  new_tau = tau+1
  for (i in new_tau :n) {
    if (high[i] <= runningMin[i-1]){
      runningMax[i] = high[i]
    } 
    else if (runningMin[i-1] == low[i-tau]) {
    index = i-tau+1
    runningMax[i] = max(high[index:i])
    #}
    else{
      runningMin[i]= runningMin[i-1]
    }
  }
}

###signal

signal = function(tau,runningMax,runningMin, high,low){
  low = rep(1:8,100)
  max = rep(1:5,100)
  tau = 8
  n = length(low)
  runningMin = rep(0,n)
  runing_max = rep(0,n)
  n = length(max)
  signal = rep(0, n)
  
  for( i in (tau+1):n){
    if (high[i] > runningMax[i-1]){
      if(low[i] < runningMin [ i-1]){
        signal[i] = 0
      }
      else{
        signal[i] = 1
      }
    }
    else if(low[i] <= runningMin[i-1]){
      signal[i] = -1
    }
  }
  return(signal)
}

####trade

trade = function(tau, stpPct, signal, runningMax, runningMin, open, high, low, close){
  close = rep(1:5,100)
  open = rep(1:5,100)
  high = rep(2:6,100)
  low = rep(1:8,100)
  max = rep(1:5,100)
  stpPct = rep(2:6,100)
  tau = 8
  n = length(low)
  runningMin = rep(0,n)
  runing_max = rep(0,n)
  n = length(max)
  signal = rep(0, n)
  trade = rep(0,n)
  price = rep(1:5,n)
  currentPos = 0
  prevPeak = 0
  prevTrough = 0
  i = 10
  
  for(i in (tau+1):(n-1)){
   if( currentPos==0){
     if (signal[i] == 1){
       trade[i] = 1
       currentPos = 1
       a = runningMax[i-1]
       b = open[i]
       price[i] = max(a,b)
       #price[i] = max(runningMax[i‐1], open[i])
       prevPeak = price[i]
     } 
     if (signal[i] == -1) {
       trade[i] = -1
       currentPos = -1
       a = runningMin[i-1]
       b = open[i]
       price[i] = min(a,b)
       #price[i] = min(runningMin[i‐1],open[i])
       prevTrough = price[i]
     }
   }
    else if(currentPos == 1){
      if (high[i-1] > prevPeak){
        prevPeak = high[i-1]
      }
        else if (low[i] < (1-stpPct) * prevPeak){
          trade[i] = -1
          price[i] = min(open[i], (1-stpPct)*prevPeak)
          currentPos = 0
        }
    }
    else if (currentPos == -1){
      if (low[i-1] < prevTrough){
        prevTrough = low[i-1]
      }
        else if (high[i-1] > (1 + stpPct) * prevTrough) {
          trade[i] = 1
          price[i] = max(open[i], (1+stpPct)*prevTrough)
          currentPos = 0
        }
      }
  }
  
  if (currentPos == 1){
    trade[n] = -1
    price[n] = close[n]
  }
  else if (currentPos == -1){
    trade[n] = 1
    price[n] = close[n]
  }
}



