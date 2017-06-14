library(cardiacpeakfinder)

example1 <- read.csv('data/CaHandUT1.csv')
example.object1 <- dataframeToExperiment(dataframe =  example1, timeIndex = 1,
                                        smooth_fxn = T ,smooth_n = 50)
example.object1 <- analyzeExperiment(example.object1)
runTestGraph(example.object1, "R2")

example2 <- readZiessData('data/2f2.csv')
colnames(example2) <- c('Time','R1','R2','R3','R4','R5','R6','R7','R8',
                       'R9','R10','R11','R12','R13','R14','R15')
example.object2 <- dataframeToExperiment(dataframe =  example2, timeIndex = 1,
                                        smooth_fxn = T ,smooth_n = 50)
example.object2 <- analyzeExperiment(example.object2)
runTestGraph(example.object2)

anna.data <- readZiessData('./Data/2l1.csv')
colnames(anna.data) <- c('Time','R1','R2','R3','R4','R5','R6','R7','R8',
                        'R9','R10','R11','R12')
anna.object <- dataframeToExperiment(dataframe =  anna.data, timeIndex = 1,
                                         smooth_fxn = T ,smooth_n = 50)
anna.object <- analyzeExperiment(anna.object)
runTestGraph(anna.object)


pacing <- readZiessData('data/CMfCF_5.17_5.20.17_pacingsweep.csv')
colnames(pacing) <- c('Time','R1','R2','R3','R4','R5','R6','R7','R8',
                        'R9','R10','R11','R12','R13','R14','R15','R16',
                        'R17','R18','R19','R20')
seg <- c(10,20,30,40,50,60)

seg_index <- c(0)
for(t in seg){
  seg_index = c( seg_index, which(abs(pacing$Time - t) == min(abs(pacing$Time - t)))[1])
}
seg_index <- c(seg_index, nrow(pacing))

seg_y <- c(173.2,233.4,259.7,233.4,173.2,224.5,259.7)

pacing.object <- dataframeToExperiment(dataframe =  pacing, timeIndex = 1,
                                         smooth_fxn = T ,smooth_n = 50, timing_alpha = 500)
pacing.object <- analyzeExperiment(pacing.object)
runTestGraph(pacing.object)

for(i in names(pacing)){
  if(i == "Time") next
  plot(pacing$Time, pacing[[i]], main=i, type = 'l')
  abline(h = mean(pacing[[i]]))
  for(li in 1:(length(seq_index)-1)){
    lines(x =  c(seq_index[li],seq_index[li+1]), y = c(seg_y[i], seg_y[i]))
  }
}
