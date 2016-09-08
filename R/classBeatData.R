
BeatData <- setClass('Beat Data',
    slots = list(
      peaks = 'vector', #findPeaks(flour),
      mins = 'vector', #findMins(flour, peaks),
      leftMid = 'vector', #findMids(flour, peaks, mins, left = T),
      rightMid = 'vector' #findMids(flour, peaks, mins, left = F)
    )
)


setGeneric(name="setPeaks",
           def=function(theObject,position)
           {
             standardGeneric("setPeaks")
           }
)

setMethod(f="setPeaks",
          signature="Agent",
          definition=function(theObject,position)
          {
            theObject@Peaks <- position
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the Peaks
setGeneric(name="getPeaks",
           def=function(theObject)
           {
             standardGeneric("getPeaks")
           }
)

setMethod(f="getPeaks",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@Peaks)
          }
)
