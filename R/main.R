source('./R/calculations.R')
source('./R/errorChecking.R')
source('./R/experiment.R')
source('./R/finders.R')

#' A funciton to handle loading in data from a Zies reading.
#'
#' Designed to remove the empty "Marker" column, any empty rows after
#' importing into R, and removing rows that do not contain mean
#' intensity values
#'
#' @param path_to_csv A csv file containing data gathered from a Ziess experiment.
#' @param time_index The index of the column containing all of the 'Time' data.
#' @param grep_keyword The keyword passed to grep to fetch the colums for your
#' particular experiment. By default, \code{grep_keyword} is set to 'IntensityMean', fetching
#' columns containing mean intensity values.
#' @param time_units A string. Should be set to "seconds" or "miliseconds". Default to miliseconds.
#'
#' @return A formated dataframe.
#'
#' @export
readZiessData <- function(path_to_csv, time_index=1, grep_keyword='IntensityMean', time_units = "miliseconds"){

  if(!time_units %in% c("miliseconds", "seconds", "m", "s"))
    stop("Error: time_units must be either m, s, miniseconds, or seconds")
  raw_dat <- read.csv(path_to_csv, header = T, stringsAsFactors = F)
  raw_dat <- raw_dat[-1,-2] # Remove empty
  Time <- as.numeric(raw_dat[,time_index])
  if(time_units %in% c("s", "seconds")) Time <- Time * 1000
  return(cbind(Time,subset_intensity_data(raw_dat, grep_keyword)))
}

#' Trim seconds off the begining or end of a run
#'
#' Removes data from either end of a run. The value passed to \code{trim_start} will remove values
#' from the left until the time matches \code{trim_start}. The value passed to \code{trim_end} will remove values
#' from the right until the time matches \code{trim_end}. For instance, if my time was the vector
#' \code{c(1,2,3,4,5,6,7)} and I passed \code{trim_start = 2} \code{trim_end = 5}, the output would be
#' \code{c(2,3,4,5)}. Passing only \code{trim_start = 3} would output \code{c(3,4,5,6,7)}
#'
#' @param raw_data A dataframe containing the data you wish to plot.
#' @param time_index The index of the column containing all of the 'Time' data.
#' @param trim_start Time value to trim to, starting from the left. Default is no trimming.
#' @param trim_end Time value to trim to, starting from the right. Default is no trimming.
#'
#' @export
trimData <- function(raw_data, time_index,
                     trim_start = raw_data[,time_index][1], trim_end = raw_data[,time_index][nrow(raw_data)]){

  if(! raw_data[,time_index][1] <= trim_start && trim_start <= raw_data[,time_index][length(raw_data)]) stop("trim_start out of range.")
  if(! raw_data[,time_index][1] <= trim_end && trim_end <= raw_data[,time_index][length(raw_data)]) stop("trim_end out of range.")

  closest_match_start = which(abs(raw_data[,time_index] - trim_start) ==
                                min(abs(raw_data[,time_index] - trim_start)))[1]
  closest_match_end = which(abs(raw_data[,time_index] - trim_end) ==
                                  min(abs(raw_data[,time_index] - trim_end)))[1]
  print(closest_match_end)
  return(raw_data[closest_match_start:closest_match_end,])
}


#' A generic funciton to plot intensities over time The data should be cleaned, either
#' maually or with \code{\link{readZiessData}} so that only the time and the data you
#' care about are left. \code{plotRawData} will iterate through the rows, printing
#' @param raw_data A dataframe containing the data you wish to plot.
#' @param time_index The index of the column containing all of the 'Time' data.
#' @return None
#'
#' @export
plotRawData <- function(raw_data, time_index=1){
  time = names(raw_data)[time_index]

  for(i in names(raw_data)){
    if(i == time) next
    plot(raw_data[[time]], raw_data[[i]], main=i, type = 'l',
        xlab = "Time", ylab = "Intensity (A.U.)")
    abline(h = mean(raw_data[[i]]))
  }
}

#' Converts a cleaned CSV to an rcamp object.
#'
#' The rcamp object (an S3 object) will hold all the raw data as well as results. This
#' function checks to make sure the following are true:
#'
#' * No NA's are contained in the data frame.
#'
#' * The time_index is assigned correctly, and no timeing errors are detected. Timing
#' errors are defined as gaps in recording that exceed some threshold (defined as the
#' median time multiplied by some alpha).
#'
#' * There are no extranious intensity values.
#'
#' The resulting object can be passed to the other funcitons in this program.
#' @param raw_data A dataframe containing the data you wish to turn into an experiment
#' object. You are welcome to subset the data before passing it in (ie \code{raw_data[,-c(2,5)]}
#' to exclude the second and third column).
#' @param time_index The index of the column containing all of the 'Time' data.
#' @param smooth_fxn A Boolean (default \code{False}). Do you want the data, which may or may
#' not be noisy, to be smoothed using a running average. Recommended.
#' @param smooth_n A number to be passed to the running average function. n is the number
#' of points to average at each window. Default is 20.
#' @param timing_alpha A number to pass to the timing checker. This alpha is multiplied to the
#' median of the differnces of each time measument. If a time measurment exceeds this threshold,
#' an error is produced. The microscope may have stalled at this point, and furhter investigation is
#' needed to check the data. Default is 2.
#' @return An rcamp object.
#'
#' @export
dataframeToRCaMP <- function(raw_data, time_index = 1, smooth_fxn = FALSE,
                                  smooth_n = 20, timing_alpha = 2, time_units = "miliseconds", time_override = F){
  rcamp = create_new_rcamp()

  if(smooth_fxn) raw_data[,-time_index] <- smoothData(raw_data[,-time_index],20)
  raw_data <- raw_data[complete.cases(raw_data),]

  stopifnot(fullTimeTest(raw_data[,time_index], timing_alpha))
  raw_data[,time_index] = checkTimeUnits(raw_data[,time_index], time_units, time_override)
  rcamp$time = raw_data[,time_index]
  dat_no_time = raw_data[,-time_index]

  rcamp$data = dat_no_time
  rcamp$names = colnames(dat_no_time)

  return(rcamp)
}


#' Analyzes the experimental data.
#'
#' \code{analyzeExperiment} takes in an rcamp object, and
#' returns physiological relivant data. This includes:
#'
#' * Peaks
#' * Troughs (mins)
#' * T50, T85, and T90
#' * Vmax for upstrokes and downstrokes.
#'
#' TODO Pass the peakfinder alpha to this function.
#'
#' @param ex_obj The experimental object generated by \code{\link{dataframeToRCaMP}}.
#' @param round_diget How to round the results digets. Default is 3.
#'
#' @return An rcamp object with a filled results table.
#'
#' @export
analyzeExperiment <- function(ex_obj, round_diget = 3){

  if(class(ex_obj) != "rcamp"){
    return('Error: Convert to rcamp object first.')
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


#' Graphs peak calls, troughs (mins), and T50, T85, and T90 reference points,
#' color coaded and coded based on upstroke or downstroke values.
#'
#' @param rcamp_obj The experimental object generated by
#' \code{\link{dataframeToRCaMP}} and analyzed in \code{\link{analyzeExperiment}}.
#' @param name_of_column A list or a string that matches the column names you wish to
#' graph. Defalts to all columns.
#'
#' @return None
#'
#' @export
runTestGraph <- function(rcamp_obj, name_of_column = rcamp_obj$names) {
  plot_indexes = which(rcamp_obj$names %in% name_of_column)
  if(length(plot_indexes)==0) return("No columns selected, check spelling.")
  for(i in plot_indexes){
    sample = rcamp_obj$data[,i]
    title =  paste0('Values on ', rcamp_obj$names[i])

    graphics::plot(rcamp_obj$time, sample, type = 'l', col = 'steelblue',
         main = title, ylab = "Intensity", xlab = "Time (miliseconds)")
    for(j in rcamp_obj$peaks[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#4daf4a', pch = 21)
    }
    for(j in rcamp_obj$mins[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#e41a1c', pch = 21)
    }
    for(j in rcamp_obj$midsUp[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#984ea3', pch = 24)
    }
    for(j in rcamp_obj$midsDown[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#984ea3', pch = 25)
    }
    for(j in rcamp_obj$midsUp85[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#ff7f00', pch = 24)
    }
    for(j in rcamp_obj$midsDown85[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#ff7f00', pch = 25)
    }
    for(j in rcamp_obj$midsUp90[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#377eb8', pch = 24)
    }
    for(j in rcamp_obj$midsDown90[i]){
      graphics::points(x = rcamp_obj$time[j], y = sample[j], bg = '#377eb8', pch = 25)
    }
  }
}
