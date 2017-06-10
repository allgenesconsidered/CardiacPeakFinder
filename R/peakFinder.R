source('./R/calculations.R')
source('./R/errorChecking.R')
source('./R/experiment.R')
source('./R/finders.R')


#' A funciton to handle loading in data from a Zies reading.
#' Designed to remove the empty "Marker" column, any empty rows after
#' importing into R, and removing rows that do not contain mean
#' intensity values
readZiessData <-function(path_to_csv, time_index=1, grep_keyword='IntensityMean'){
  raw_dat <- read.csv(path_to_csv, header = T, stringsAsFactors = F)
  raw_dat <- raw_dat[-1,-2] # Remove empty
  time <- as.numeric(raw_dat[,time_index])
  return(cbind(time,subset_intensity_data(raw_dat, grep_keyword)))
}

#' Converts a cleaned CSV to an experiment object.
dataframeToExperiment <- function(dataframe, timeIndex = 1, smooth_fxn = FALSE,
                                  smooth_n = 20){
  experiment = create_new_experiment()

  if(smooth_fxn) dataframe[,-timeIndex] <- smoothData(dataframe[,-timeIndex],20)
  dataframe <- dataframe[complete.cases(dataframe),]

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

#' Wrapper fuction for analyzing the experimental data.
analyzeExperiment <- function(ex_obj, round_diget = 3){

  if(class(ex_obj) != "experiment"){
    return('Error: Convert to experiment object first.')
  }


  output = data.frame()
  for( i in 1:ncol(ex_obj$data)){
    n = ex_obj$names[i]
    sample = ex_obj$data[,i]

    ex_obj$peaks[[n]] = findPeaks(sample)
    peak_ave = mean(indexesToValues(sample, ex_obj$peaks[[n]]))
    ex_obj$mins[[n]] = findMins(sample, ex_obj$peaks[[n]])
    min_ave = mean(indexesToValues(sample, ex_obj$mins[[n]]))
    ex_obj$midsDown[[n]] = findMids(sample, ex_obj$peaks[[n]], ex_obj$mins[[n]])
    ex_obj$midsUp[[n]] = findMids(sample, ex_obj$peaks[[n]], ex_obj$mins[[n]], Downstroke = F)

    ex_obj$midsDown85[[n]] = findMids(sample, ex_obj$peaks[[n]], ex_obj$mins[[n]],midPoint = 0.85)
    ex_obj$midsUp85[[n]] =findMids(sample, ex_obj$peaks[[n]], ex_obj$mins[[n]], Downstroke = F,
                                   midPoint = 0.85)
    ex_obj$midsDown90[[n]] = findMids(sample, ex_obj$peaks[[n]], ex_obj$mins[[n]],midPoint = 0.9)
    ex_obj$midsUp90[[n]] =findMids(sample, ex_obj$peaks[[n]], ex_obj$mins[[n]], Downstroke = F,
                                    midPoint = 0.9)

    DownstrokeT50 = calcualteT50(ex_obj$time, ex_obj$peaks[[n]], ex_obj$midsDown[[n]])
    VmaxUp = findVmax(sample, ex_obj$time, ex_obj$peaks[[n]], ex_obj$mins[[n]], Decay = F)
    UpstrokeT50 = calcualteT50(ex_obj$time, ex_obj$peaks[[n]], ex_obj$midsUp[[n]], Downstroke = F)
    VmaxDecay = findVmax(sample, ex_obj$time, ex_obj$peaks[[n]], ex_obj$mins[[n]])

    DownstrokeT85 = calcualteT50(ex_obj$time, ex_obj$peaks[[n]], ex_obj$midsDown85[[n]])
    UpstrokeT85 = calcualteT50(ex_obj$time, ex_obj$peaks[[n]], ex_obj$midsUp85[[n]], Downstroke = F)
    DownstrokeT90 = calcualteT50(ex_obj$time, ex_obj$peaks[[n]], ex_obj$midsDown90[[n]])
    UpstrokeT90 = calcualteT50(ex_obj$time, ex_obj$peaks[[n]], ex_obj$midsUp90[[n]], Downstroke = F)

    bpm = calcualteBPM(sample, ex_obj$time)
    output <- rbind(output , c(peak_ave , min_ave, (peak_ave/min_ave), mean(UpstrokeT50),
                               mean(DownstrokeT50), mean(UpstrokeT85), mean(DownstrokeT85),
                               mean(UpstrokeT90), mean(DownstrokeT90), mean(VmaxUp),
                               mean(VmaxDecay) ,bpm))
  }
  colnames(output) <- c('Peak (AU)','Min (AU)', 'F/Fn (Amplitude)', 'Upstroke T50 (ms)',
                        'Downstroke T50 (ms)', 'Upstroke T85 (ms)', 'Downstroke T85 (ms)',
                        'Upstroke T90 (ms)', 'Downstroke T90 (ms)','Vmax Up', 'Vmax Decay', 'BPM')
  ex_obj$results <- round(output, round_diget)
  return(ex_obj)
}

runTestGraph <- function(exp_obj, name_of_column = exp_obj$names) {
  plot_indexes = which(exp_obj$names %in% name_of_column)
  if(length(plot_indexes)==0) return("No columns selected, check spelling.")
  for(i in plot_indexes){
    sample = exp_obj$data[,i]
    title =  paste0('Values on ', exp_obj$names[i])

    plot(exp_obj$time, sample, type = 'l', col = 'steelblue',
         main = title, ylab = "Intensity", xlab = "Time (miliseconds)")
    for(j in exp_obj$peaks[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#4daf4a', pch = 21)
    }
    for(j in exp_obj$mins[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#e41a1c', pch = 21)
    }
    for(j in exp_obj$midsUp[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#984ea3', pch = 24)
    }
    for(j in exp_obj$midsDown[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#984ea3', pch = 25)
    }
    for(j in exp_obj$midsUp85[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#ff7f00', pch = 24)
    }
    for(j in exp_obj$midsDown85[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#ff7f00', pch = 25)
    }
    for(j in exp_obj$midsUp90[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#377eb8', pch = 24)
    }
    for(j in exp_obj$midsDown90[i]){
      points(x = exp_obj$time[j], y = sample[j], bg = '#377eb8', pch = 25)
    }
  }
}
