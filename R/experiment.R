
#' Create a new instance of an experiment object, which will initially hold the
#' raw data from the experiment. The 'Results' attribute will be filled in later
#' as the program runs. The idea is that each object will hold both the raw
#' data in the case of reanalysis, and the analysis results for easy comparison
#' between other experiment objects.
#'
#' Objects will have 4 attibutes:
#' name : The name of the object
#' time : A numeric vector containing the timing information (a ms for each
#' measured value)
#' data : A matrix containing the intenity values for each measured point
#' results : a list containing all measured data
create_new_experiment <- function(){
  v <- vector(mode='numeric')
  new_experiment <- list(
    name = NULL,
    time = vector(mode='numeric'),
    data = data.frame(),
    results = list(
      peak = v, min = v, bpm = v,
      FFn = v, rT50 = v, rVel = v,
      lT50 = v, lVel = v
    )
  )
  class(new_experiment) <- 'experiment'
  return(new_experiment)
}

print.experiment <- function(experiment_obj){
  cat('Name :',experiment_obj$name,'\n')
  cat('Experimental object with', length(experiment_obj$time),
      'measurments and', ncol(experiment_obj$data) ,'positions.\n')
}



