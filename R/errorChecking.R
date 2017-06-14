
#' Check that the vector looks like a time vector,
#' by checking that each value constently increases
#' from every value to the next.
checkTime <- function(listTime){
  return(all(diff(listTime) >= 0))
}

detectTimeErrors <- function(listTime, alpha){
  nErrors = 0
  ran = mean(diff(listTime)) * alpha

  for(i in 1:(length(listTime)-1)){
    incriment = listTime[i+1] - listTime[i]
    if(incriment > ran){
      cat('Timing Error detected at Index ',i,'. Program stalled for ',
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
  if(nErrors == 0){
    cat("No timing errors detected.\n")
    return(T)
  } else cat (nErrors, " time errors detected.", sep = '')
  return(F)
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
