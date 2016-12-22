##############################
########## Finders ###########
##############################


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
findMids <- function(list, peaks = findPeaks(list), mins = findMins(list), Downstroke = T){
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
      range = list[lIndex[i]:rIndex[i]]
      midVal = (list[lIndex[i]] + list[rIndex[i]])/2
      mids = c(mids, (which(abs(range - midVal) == min(abs(range - midVal)))) + lIndex[i])
    }
  }
  return(mids)
}

##############################
######## Calculations ########
##############################

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

###############################
####### Error Checking ########
###############################

checkTime <- function(listTime){
  return(all(diff(listTime) >= 0))
}

detectTimeErrors <- function(listTime){
  nErrors = 0
  for(i in 1:(length(listTime)-1)){
    incriment = listTime[i+1] - listTime[i]
    if(incriment > 20){
      cat('Timing Error detected at Index ',i,'. Program stalled for ',
          incriment, ' miliseconds!\n', sep = '')
      nErrors = nErrors + 1
    }
  }
  if(nErrors == 0){
    cat("No timing errors detected.\n")
  } else cat (nErrors, " time detected.", sep = '')
}

detectRunErrors <- function(list){
  for( i in 1:length(list)){
    if(list[i] < 75 || list[i] > 700){
      cat("Caution: Erronious intensity detected at index ", i ,'.\n', sep = '')
      return(i)
    }
  }
  return(F)
}

attemptPatch <- function(list, index){
  tryCatch({
    list[index] = mean(list[index + 1], list[index - 1])
    cat("Patching Sucessfull!\n")
    return(list)
  }, error = function(e) {
    cat("Patching failed.\n")
    print(e)
    if(errorPrompt()){
      cat("Sample skipped\n")
      return(T)
    } else {
      cat("Error not skipped\n")
      return(F)
    }
  })
    return(NA)
}

errorPrompt <- function(){
  while(T){
    responce <- readline(prompt="Include in analysis? [y/n]: ")
    if(responce == 'n') return(T)
    else if(responce =='y') return(F)
  }
}

fullTest <- function(list){
  test <- detectRunErrors(list)
  if(is.numeric(test)){
    cat(prompt="Attempting fix.\n")
    fix = attemptPatch(list, test)
    return(fix)
  } else return(test)
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
         main = title, ylab = "Intensity",
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
    DownstrokeVelocity = calculateVelocity(sample, time, midsDown)
    UpstrokeT50 = calcualteT50(time, peaks, midsUp, Downstroke = F)
    UpstrokeVelocity = calculateVelocity(sample, time, midsUp)
    bpm = calcualteBPM(sample, time)
    output <- rbind(output , c(peak , min, (peak/min),
                               mean(DownstrokeT50), mean(DownstrokeVelocity), mean(UpstrokeT50),
                              mean(UpstrokeVelocity) ,bpm))
  }
  colnames(output) <- c('Peak (AU)','Min (AU)', 'F/Fn (Amplitude)',
                        'Upstroke T50 (ms)', 'Upstroke Velocity', 'Downstroke T50 (ms)',
                        'Downstroke Velocity', 'BPM')
  return(round(output,3))
}

