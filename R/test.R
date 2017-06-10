source('./R/peakFinder.R')

example <- read.csv('data/CaHandUT1.csv')
example.object <- dataframeToExperiment(dataframe =  example, timeIndex = 1,
                                        smooth_fxn = T ,smooth_n = 50)
example.object <- analyzeExperiment(example.object)
runTestGraph(example.object, "R2")
