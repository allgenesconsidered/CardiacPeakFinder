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
  midPoint = 1 - midPoint
  # For downstrokes, remove first peak so that the first-most index is a trough.
  if(!Downstroke) peaks = peaks[-1]
  for(i in 1:length(peaks)){
    if(!is.na(peaks[i]) && !is.na(mins[i])){
      if(!Downstroke) {
        range = listInt[mins[i]:peaks[i]] # List of values
        cat(range)
        modifier = mins[i]
      } else {
        range = listInt[peaks[i]:mins[i]] # List of values
        modifier = peaks[i]
      }
      midVal = ceiling(abs(listInt[peaks[i]] - listInt[mins[i]]) * midPoint) + listInt[mins[i]]

      mids = c(mids, (which(abs(range - midVal) == min(abs(range - midVal))))[1] + modifier)
    }
  }
  return(mids)
}
