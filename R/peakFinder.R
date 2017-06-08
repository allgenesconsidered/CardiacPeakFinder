source('./R/calculations.R')
source('./R/errorChecking.R')
source('./R/experiment.R')

#' A funciton to identify peaks in beating GcAMP cells.
#'
#' @param list  : A list of GcAMP intensity values coorisponding to an experiemntal run.
#' @param alpha : The % of the data to scan in both directions to find peaks. Defaults to 1%.
#' @param returnValue : Boolean whether or not to return indexes or intensity values.
#' @return  A vector of either indexes or intensity values coorisponding to peaks
findPeaks <- function(list, alpha = 0.01, returnValue = F){
  peaks = c()
  pRange = length(list) * alpha
  currentMax = 0
  if(list[1] > mean(list)) findingPeak = F # If the data starts above the mean
  else findingPeak = T
  for(i in 1:length(list)){ # for every element (which we'll call i) in the list
    if (findingPeak){
      if(list[i] < mean(list)) next
      else if(list[i] > currentMax) currentMax = list[i]
      else if(list[i] < currentMax){
        peaks = c(peaks, which(list == max(list[(i-pRange):(i+pRange)])))
        # ^Take the max of the area, incase the peak is noisy
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
  if(returnValue) return(indexesToValues(list, peaks))
  else return(peaks)
}

#' A funciton to identify minimal values between peaks (troughs).
#' @param list  : A list of GcAMP intensity values coorisponding to an experiemntal run.
#' @param peaks : Index values coorisponding to peaks. Will run findPeaks() if nothing given.
#' @param returnValue : Boolean whether or not to return indexes or intensity values.
#' @return Returns a vector of either indexes or intensity values coorisponding to minimal values. It will
#'   only look for minimal values between peaks, so nothing should be showing up before the index of
#'   the first peak.
findMins <- function(list, peaks = findPeaks(list), returnValue = F){
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
  if(returnValue) return(indexesToValues(list, mins))
  else return(mins)
}

#' A funciton to identify midpoints (T50) between peaks and troughs. Only the mid points between .
#' Argunments :
#' @param list  : A list of GcAMP intensity values coorisponding to an experiemntal run.
#' @param peaks : Index values coorisponding to peaks. Will run findPeaks() if nothing given.
#' @param mins : Intex values coorisponding to troughs. Will run findMins() if nothing given.
#' @return Returns a vector of indexes coorisponding to T50 values. It will only look for T50
#'   values between peaks and troughs, so nothing should be showing up before the index of
#'   the first peak or after the index of the last peak.
findMids <- function(listInt, peaks = findPeaks(listInt), mins = findMins(listInt), Downstroke = T, midPoint = 0.5){
  mids = c()
  if(Downstroke){
    lIndex = peaks
    rIndex = mins
  } else{
    lIndex = mins
    rIndex = peaks[-1]
  }
  for(i in 1:length(lIndex)){
    if(!is.na(lIndex[i]) && !is.na(rIndex[i])){
      range = listInt[lIndex[i]:rIndex[i]]
      midVal = ceiling((listInt[lIndex[i]] + listInt[rIndex[i]])*midPoint)
      mids = c(mids, (which(abs(range - midVal) == min(abs(range - midVal)))) + lIndex[i])
    }
  }
  return(mids)
}

##############################
########## Testing ###########
##############################

runTestGraph <- function(dat = read.csv("./data/CAHandUT1.csv")){
  time = dat[,1]
  detectTimeErrors(time)

  for( i in 2:ncol(dat)){
    sample = dat[,i]
    title =  paste0('Test on ', colnames(dat)[i])

    plot(time, sample, type = 'l', col = 'steelblue',
         main = title, ylab = "Intensity (AU)",
         xlab = "Time (miliseconds)")
    abline(h = mean(sample), col = 'grey')
    peaks = findPeaks(sample)
    mins = findMins(sample, peaks)
    for(i in peaks){
      points(x = dat[i,1], y = sample[i], col = 'forestgreen', pch = 16)
    }
    for(i in mins){
      points(x = dat[i,1], y = sample[i], col = 'red', pch = 16)
    }
    for(i in findMids(sample, peaks, mins)){
      points(x = dat[i,1], y = sample[i], col = 'purple', pch = 16)
    }
    for(i in findMids(sample, peaks, mins, Downstroke = F)){
      points(x = dat[i,1], y = sample[i], col = 'orange', pch = 16)
    }
    for(i in findVmax(sample, time, peaks, mins, returnIndexes = T)){
      points(x = time[i], y = sample[i], col = 'gold1', pch = 16)
    }
    for(i in findVmax(sample, time, peaks, mins, Decay = F, returnIndexes = T)){
      points(x = time[i], y = sample[i], col = 'blue', pch = 16)
    }
  }
}

analyzeExperiment <- function(dat){

  if(colnames(dat)[1] != "Time"){
    return('Error: Please name the fist column Time')
  }
  time = dat[,1]
  detectTimeErrors(time)
  output = data.frame(Peaks_Ave = numeric(0),
                      Mins_Ave = numeric(0),
                      FoverFn_Ave = numeric(0),
                      DownstrokeT50_Ave = numeric(0),
                      DownstrokeVel_Ave = numeric(0),
                      UpstrokeT50_Ave = numeric(0),
                      UpstrokeVel_Ave = numeric(0),
                      BPM = numeric(0)
                      )
  for( i in 2:ncol(dat)){
    #cat('Checking sample', colnames(dat)[i],'\n')
    sample = dat[,i]
    test = fullTest(sample)
    if(is.logical(test)){
      if(test) next
    } else if(is.vector(test)){
      sample <- test
    }
    peaks = findPeaks(sample)
    peak = mean(indexesToValues(sample, peaks))
    mins = findMins(sample)
    min = mean(indexesToValues(sample, mins))
    midsDown = findMids(sample)
    midsUp = findMids(sample, Downstroke = F)

    DownstrokeT50 = calcualteT50(time, peaks, midsDown)
    VmaxUp = findVmax(sample, time, peaks, mins, Decay = F)
    UpstrokeT50 = calcualteT50(time, peaks, midsUp, Downstroke = F)
    VmaxDecay = findVmax(sample, time, peaks, mins)

    bpm = calcualteBPM(sample, time)
    output <- rbind(output , c(peak , min, (peak/min), mean(UpstrokeT50),
                               mean(DownstrokeT50), mean(VmaxUp), mean(VmaxDecay) ,bpm))
  }
  colnames(output) <- c('Peak (AU)','Min (AU)', 'F/Fn (Amplitude)', 'Upstroke T50 (ms)',
                        'Downstroke T50 (ms)', 'Vmax Up', 'Vmax Decay', 'BPM')
  return(round(output,3))
}

