library(shiny)
source('./R/peakFinder.R')

LIST_OF_FILES <- c()

function(input, output) {

  output$ = renderPlot({p})
  output$plotgraph2 = renderPlot({pt2()})

  analyzedMulti <- reactive({
    inFile <- input$multiFile

    if (is.null(inFile))
      return(NULL)
    dat = read.csv(inFile$datapath, sep = input$fileSepMulti , header=input$headerMulti, stringsAsFactors = F)
    analyzeExperiment(dat)
  })

  output$title <- renderText({
    input$multiFile$name
  })

  output$tables <- renderTable({
    analyzedMulti()
  })

  output$fileList <- renderUI({
    selectInput("fileList", "Choose Option:", choices = NULL, multiple = T)
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
