library(shiny)

fluidPage(
  titlePanel("Cardiac Peak Analyzer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Load in data from a microscopy timecourse and get a readout of
               relevant cardiac data. "),
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator Type',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      downloadButton('downloadData', 'Download Analysis')
    ),
    mainPanel(
      tabsetPanel(
        #First Panel
        tabPanel("About",
  				HTML('<p>This application was developed for analysis of microscopy based
  				     timecorse data of beating aggregates. The program will load in data
  				    and return a dataframe of cardiac specific measurments. </p>')
  			),
  			# Data upload tab
  			tabPanel("Data upload", tableOutput("contents")
  			),
  			id='Tabs')
    )
  )
)
