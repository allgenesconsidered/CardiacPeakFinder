source('./pf/peakFinder.R')

load_csv_to_experiment <- function(path, name = tools::file_path_sans_ext(path)){
  dat = read.csv(path, stringsAsFactors = F)

  experiment = dataframe_to_experiment(dat, name)
  return(experiment)
}

dataframe_to_experiment <- function(dataframe, name, timeIndex = 1){
  if(!checkTime(dataframe[,timeIndex])){
    return('Error: Column specified does not look like the time measuement.')
  } else detectTimeErrors(dataframe[,timeIndex])
  experiment = create_new_experiment()
  experiment$time = dataframe[,timeIndex]
  experiment$data = dataframe[,-timeIndex]
  experiment$name = name

  return(experiment)
}
