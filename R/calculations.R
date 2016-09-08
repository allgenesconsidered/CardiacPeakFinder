#' Calculate Beats Per Minute
#'
#' Calulates the BPM per sample
#' @name calculateBPM
#' @param listInt List of GcAMP measurments
#' @param listTime List of time measuments
#' @export
#' @examples
#' x <- calculateBPM(dat$R1, dat$Time)
calcualteBPM <- function(listInt, listTime){ # Number of peaks per second
  peaks = findPeaks(listInt)
  time = listTime[peaks[length(peaks)]] - listTime[peaks[1]]
  return(length(peaks)/time * 1000 * 60)
}

#' Calculate T50 Upstroke or Downstroke
#'
#' Calulates the T50 per sample
#' @name calculateT50
#' @param listTime List of time measuments
#' @param left Boolean, whether to take the t50 from the left (decay/downstroke) or the right (peak/upstroke)
#' @export
#' @examples
#' x<- calculateBPM(dat$R1, dat$Time)
calcualteT50 <- function(listTime, peaks, mids, left = T){
  t50s = c()
  if(!left) peaks = peaks[-1]
  for(i in 1:length(peaks)){
    if(!is.na(peaks[i]) && !is.na(mids[i])){
      if(left) t50s = c(t50s, listTime[mids[i]] - listTime[peaks[i]])
      else t50s = c(t50s, listTime[peaks[i]] - listTime[mids[i]])
    }
  }
  return(t50s)
}
