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
  v <- vector(mode='numeric')
  new_experiment <- list(
    names = vector(mode='character'),
    time = vector(mode='numeric'),
    data = data.frame(),
    results = list(
      peak = v, min = v, bpm = v,
      FFn = v, dT50 = v, VmaxDown = v,
      uT50 = v, VmaxUp = v
    )
  )
  class(new_experiment) <- 'experiment'
  return(new_experiment)
}


#' A funciton to handle loading in data from a Zies reading.
#' Designed to remove the empty "Marker" column, any empty rows after
#' importing into R, and removing rows that do not contain mean
#' intensity values
read_zies_data <-function(path_to_csv, time_index=1, grep_keyword='IntensityMean'){
  raw_dat <- read.csv(path_to_csv, header = T, stringsAsFactors = F)
  raw_dat <- raw_dat[-1,-2] # Remove empty
  time <- as.numeric(raw_dat[,time_index])
  return(cbind(time,subset_intensity_data(raw_dat, grep_keyword)))
}

#' Converts a cleaned CSV to an experiment object.
dataframe_to_experiment <- function(dataframe, timeIndex = 1){
  experiment = create_new_experiment()
  stopifnot(fullTimeTest(dataframe[,timeIndex]))
  experiment$time = dataframe[,timeIndex]
  dat_no_time = dataframe[,-timeIndex]
  for(i in 1:ncol(dat_no_time)){
    test = fullTest(dat_no_time[,i])
    if(is.logical(test)){
      if(test) next
    } else if(is.vector(test)){
      dat_no_time[,i] = test
    }
  }
  experiment$data = dat_no_time
  experiment$names = colnames(dat_no_time)

  return(experiment)
}

#' greps the mean intensity values, or whatever othe values you want.
subset_intensity_data <- function(exp_dataset, grep_keyword='IntensityMean'){
  output <- exp_dataset[,grep(grep_keyword,colnames(exp_dataset))]
  return(output)
}

#' Calculate moving averages to remove noise
moving_average <- function(x,n=5){
  return(filter(x,rep(1/n,n), sides=2))
}


#' Print function for experiement object
print.experiment <- function(exp_obj){
  cat('Name :',exp_obj$name,'\n')
  cat('Experimental object with', length(exp_obj$time),
      'measurments and', ncol(exp_obj$data) ,'positions.\n')
}

#' Object to remove
returnResults <- function(exp_obj){
  UseMethod('returnResults')
}

returnResults.experiment <- function(exp_obj){

  res = exp_obj$results

  output = data.frame(Peaks_Ave = res$peak,
                      Mins_Ave = res$min,
                      FoverFn_Ave = res$FFn,
                      UpstrokeT50_Ave = res$uT50,
                      DownstrokeT50_Ave = res$dT50,
                      UpstrokeVel_Ave = res$VmaxUp,
                      DownstrokeVel_Ave = res$VmaxDown,
                      BPM = res$bpm
  )
  colnames(output) <- c('Peak (AU)','Min (AU)', 'F/Fn (Amplitude)',
                        'Upstroke T50 (ms)', 'Upstroke Velocity', 'Downstroke T50 (ms)',
                        'Downstroke Velocity', 'BPM')
  return(output)
}

plotSingleMeasurment <- function(exp_obj, i) {
  sample = exp_obj$data[,i]
  title =  paste0('Test on ', colnames(exp_obj$data)[i])

  plot(exp_obj$time, sample, type = 'l', col = 'steelblue',
       main = title, ylab = "Intensity",
       xlab = "Time (miliseconds)")
  abline(h = mean(sample), col = 'grey')
  peaks = findPeaks(sample)
  mins = findMins(sample, peaks)
  for(i in peaks){
    points(x = exp_obj$time[i], y = sample[i], col = 'forestgreen', pch = 16)
  }
  for(i in mins){
    points(x = exp_obj$time[i], y = sample[i], col = 'red', pch = 16)
  }
  for(i in findMids(sample, peaks, mins)){
    points(x = exp_obj$time[i], y = sample[i], col = 'purple', pch = 16)
  }
  for(i in findMids(sample, peaks, mins, Downstroke = F)){
    points(x = exp_obj$time[i], y = sample[i], col = 'orange', pch = 16)
  }
}
