source('./R/peakFinder.R')

example <- read.csv('data/CaHandUT1.csv')
example.object <- dataframeToExperiment(dataframe =  example, timeIndex = 1)
example.object <- analyzeExperiment(example.object)
runTestGraph(example.object, "R3")
