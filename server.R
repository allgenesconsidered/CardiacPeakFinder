library(shiny)
source('./R/peakFinder.R')

function(input, output) {
  analyzed <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    dat = read.csv(inFile$datapath, header=input$header, sep=input$sep)
    analyzeExperiment(dat)
  })
  output$contents <- renderTable({
    analyzed()
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.time(), ' cardiac_output.csv', sep='')
    },
    content = function(file) {
      write.csv(analyzed(), file, row.names = F)
    }
  )
}
