if(!file.exists("./data/CaHandUT1.csv")){
  dat <- read.csv("./data/Experiment-03.csv", stringsAsFactors = F)
  dat <- dat[-1,]
  dat <- dat[,c(1, grep("IntensityMean", colnames(dat)))]
  colN <- c()
  for(i in 2:ncol(dat)) colN <- c(colN, paste0('R',i-1))
  colnames(dat) <- c('Time', colN)
  dat$Time <- as.numeric(dat$Time)
  write.csv(dat, "./data/CaHandUT1.csv", row.names = F)
} else {
  dat <- read.csv("./data/CAHandUT1.csv")
}



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

getTimeMids <- function(list, peaks = findPeaks(list), mins = findMins(list)){
  mids = c()
  for(i in 1:length(peaks)){
    if(!is.na(peaks[i]) && !is.na(mins[i])){
      mids = c(mids, ceiling((peaks[i] + mins[i])/2))
    }
  }
  return(mids)
}

getIntensityMids <- function(list, peaks = findPeaks(list), mins = findMins(list), left = T){
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

detectError <- function(listTime){
  nErrors = 0
  for(i in 1:(length(listTime)-1)){
    incriment = listTime[i+1] - listTime[i]
    if(incriment > 20){
      cat('Error detected at Index ',i,'. Program stalled for ',
          incriment, ' miliseconds!', sep = '')
      nErrors = nErrors + 1
    }
  }
  if(nErrors == 0){
    cat("No measurement errors detected.")
  } else cat (nErrors, " detected.", sep = '')
}


########################
####### Testing ########
########################

detectError(dat$Time)

for( i in 3){
  time = dat$Time
  sample = dat[,i]
  title =  paste0('Untreated CM 122 2.0mM : ', i-1)

  plot(time, sample, type = 'l', col = 'blue',
       main = title, ylab = "Intensity",
       xlab = "Time (miliseconds)")
  abline(h = mean(sample), col = 'grey')
  peaks = findPeaks(sample)
  mins = findMins(sample, peaks)
  for(i in peaks){
    points(x = dat[i,1], y = sample[i], col = 'green', pch = '*')
  }
  for(i in mins){
    points(x = dat[i,1], y = sample[i], col = 'red', pch = '*')
  }
  for(i in getIntensityMids(sample, peaks, mins)){
    points(x = dat[i,1], y = sample[i], col = 'purple', pch = 4)
  }
  for(i in getIntensityMids(sample, peaks, mins, left = F)){
    points(x = dat[i,1], y = sample[i], col = 'orange', pch = 4)
  }
}
