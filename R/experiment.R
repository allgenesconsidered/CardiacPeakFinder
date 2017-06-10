source('./R/calculations.R')
source('./R/errorChecking.R')

#' Create a new instance of an experiment object, which will initially hold the
#' raw data from the experiment. The 'Results' attribute will be filled in later
#' as the program runs. The idea is that each object will hold both the raw
#' data in the case of reanalysis, and the analysis results for easy comparison
#' between other experiment objects.
#'
#' Objects will have 4 attibutes:
#' names : Names for each column.
#' time : A numeric vector containing the timing information (a ms for each
#' measured value)
#' data : A matrix containing the intenity values for each measured point
#' results : a list containing all measured data
create_new_experiment <- function(){
  new_experiment <- list(
    names = vector(mode='character'),
    time = vector(mode='numeric'),
    data = data.frame(),
    peaks = list(),
    mins = list(),
    midsDown = list(),
    midsUp = list(),
    midsDown85= list(),
    midsUp85 = list(),
    midsDown90= list(),
    midsUp90 = list(),
    results = NULL
  )
  class(new_experiment) <- 'experiment'
  return(new_experiment)
}


#' greps the mean intensity values, or whatever othe values you want.
subset_intensity_data <- function(exp_dataset, grep_keyword='IntensityMean'){
  output <- exp_dataset[,grep(grep_keyword,colnames(exp_dataset))]
  return(output)
}

smoothData <- function(x,n){
  for(i in 1:ncol(x)){
    x[,i] = moving_average(x[,i],n)
  }
  return(x)
}

#' Calculate moving averages to remove noise
moving_average <- function(x,n){
  return(filter(x,rep(1/n,n), sides=2))
}


#' Print function for experiement object
print.experiment <- function(exp_obj){
  cat('Data names :',exp_obj$name,'\n')
  cat('Experimental object with', length(exp_obj$time),
      'measurments and', ncol(exp_obj$data) ,'positions.\n')
}
