
#' Check that the vector looks like a time vector,
#' by checking that each value constently increases
#' from every value to the next.
checkTime <- function(listTime){
  return(all(diff(listTime) >= 0))
}

checkTimeUnits <- function( listTime, time_units, time_override){
  delta = mean(diff(listTime))
  if(delta < 1 && time_units %in% c('m','miliseconds') && !time_override){
    stop(paste('Time measurments are', round(delta, digits = 4), 'units appart. This',
               'does not look like miliseconds. Aborting. Change time_units to',
                '\'seconds\' or change time_override to True to ignore this message.\n'))
  }
  if(time_units %in% c('s', 'seconds')) {
    listTime = listTime * 1000
    }
  return(listTime)
}

detectTimeErrors <- function(listTime, alpha){
  nErrors = 0
  ran = mean(diff(listTime)) * alpha

  for(i in 1:(length(listTime)-1)){
    incriment = listTime[i+1] - listTime[i]
    if(incriment > ran){
      cat('Timing Error detected at Index ',i,'. Run stalled for ',
          incriment, ' miliseconds!\n', sep = '')
      nErrors = nErrors + 1
    }
  }
  return(nErrors)
}

fullTimeTest <- function(listTime, alpha){
  if(!checkTime(listTime)){
    cat('Error: Column specified does not look like the time measuement.')
    return(F)
  }
  nErrors = detectTimeErrors(listTime, alpha)
  if(nErrors != 0){
    warning(paste(nErrors, " time errors detected, the analysis may have stalled.", sep = ''))
  }
  return(T)
}

errorPrompt <- function(){
  while(T){
    responce <- readline(prompt="Include in analysis? [y/n]: ")
    if(responce == 'n') return(T)
    else if(responce =='y') return(F)
  }
}

