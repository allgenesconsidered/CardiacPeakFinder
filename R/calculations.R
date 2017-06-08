

calcualteBPM <- function(listInt, listTime){ # Number of peaks per second
  peaks = findPeaks(listInt)
  time = listTime[peaks[length(peaks)]] - listTime[peaks[1]]
  return(length(peaks)/time * 1000 * 60)
}

calcualteT50 <- function(listTime, peaks, mids, Downstroke = T){
  t50s = c()
  if(!Downstroke) peaks = peaks[-1]
  for(i in 1:length(peaks)){
    if(!is.na(peaks[i]) && !is.na(mids[i])){
      if(Downstroke) t50s = c(t50s, listTime[mids[i]] - listTime[peaks[i]])
      else t50s = c(t50s, listTime[peaks[i]] - listTime[mids[i]])
    }
  }
  return(t50s)
}

calculateVelocity <- function(listInt, listTime, midsIndex){
  vel = c()
  for(i in midsIndex){
    x = c(listInt[i], listInt[i+1])
    y = c(listTime[i], listTime[i+1])
    vel = c(vel, diff(x)/diff(y))
  }
  return(vel)
}

indexesToValues <- function(list, indexes){
  values = c()
  for(i in indexes){
    values = c(values, list[i])
  }
  return(values)
}

findVmax <- function(listInt, listTime, peaks=findPeaks(listInt), mins=findMins(listInt), Decay=TRUE, returnIndexes=FALSE){
  vmax_indexes = c()
  if(Decay){
    lIndex = peaks
    rIndex = mins
  } else{
    lIndex = mins
    rIndex = peaks[-1]
  }
  for(i in 1:length(lIndex)){
    if(!is.na(lIndex[i]) && !is.na(rIndex[i])){
      range = listInt[lIndex[i]:rIndex[i]]
      vmax =  which(abs(diff(range))==max(abs(diff(range))) )
      vmax_indexes = c(vmax_indexes, vmax + lIndex[i])
    }
  }
  if(returnIndexes){return(vmax_indexes)}
  return(calculateVelocity(listInt, listTime, vmax_indexes))
}
