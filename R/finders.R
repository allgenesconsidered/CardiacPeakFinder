findPeaks <- function(list, alpha = 0.01){
  peaks = c()
  pRange = length(list)*alpha
  currentMax = 0
  if(list[1] > mean(list)) findingPeak = F
  else findingPeak = T
  for(i in 1:length(list)){ # for every element (which we'll call i) in the list
    if (findingPeak){
      if(list[i] < mean(list)) next
      else if(list[i] > currentMax) currentMax = list[i]
      else if(list[i] < currentMax){
        j =
          peaks = c(peaks, which(list == max(list[(i-pRange):(i+pRange)]))) # Take the top of the peak, incase the peak is noisy
        findingPeak = F
      }
    } else {
      if(list[i] > mean(list)) next
      else {
        findingPeak = T
        currentMax = 0
      }
    }
  }
  return(peaks)
}

findMins <- function(list, peaks = findPeaks(list)){
  mins = c()
  for(i in 1:(length(peaks))){
    peak1 = peaks[i]
    peak2 = peaks[i+1]
    if(!is.na(peak2)){
      getRange = list[peak1:peak2]
      mins = c(mins, (which(getRange == min(getRange))+peak1))
    } else {
      return(mins)
    }
  }
  return(mins)
}


findMids <- function(list, peaks = findPeaks(list), mins = findMins(list), left = T){
  # Gets the middle value (mean of peak intensity and minmum).
  # left : A boolean whether to take the left mid (for calculating decay time). False for right mid (peak time).
  mids = c()
  if(left){
    lIndex = peaks
    rIndex = mins
  } else{
    lIndex = mins
    rIndex = peaks[-1]
  }
  for(i in 1:length(lIndex)){
    if(!is.na(lIndex[i]) && !is.na(rIndex[i])){
      range = list[lIndex[i]:rIndex[i]]
      midVal = (list[lIndex[i]] + list[rIndex[i]])/2
      mids = c(mids, (which(abs(range - midVal) == min(abs(range - midVal)))) + lIndex[i])
    }
  }
  return(mids)
}
