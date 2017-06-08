library(shiny)

fluidPage(
  titlePanel("Cardiac Peak Analyzer"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition="input.tabs=='Graph peak calls'",
        helpText("Load in data from a microscopy timecourse to see how the algorithm calls
                 peaks and midpoints. "),
        h4("Enter data to graph results"),
        fileInput("singleFile", "Upload delimited text file:", multiple = FALSE),
        checkboxInput('headerSingle', 'Header', TRUE),
        radioButtons("fileSepSingle", "Delimiter:", list("Comma"=',',"Tab"='\t',"Semicolon"=';'))
      ),
      conditionalPanel(
        condition="input.tabs=='Compare multiple experiments'",
        helpText("Load in data from a microscopy timecourse and get a readout of
                 relevant cardiac data. "),
        h4("Enter data to compare"),
        fileInput("multiFile", "Upload delimited text file:", multiple = TRUE),
        checkboxInput('headerMulti', 'Header', TRUE),
        radioButtons("fileSepMulti", "Delimiter:", list("Comma"=',',"Tab"='\t',"Semicolon"=';')),
        downloadButton('downloadData', 'Download Analysis')
        ),
      conditionalPanel(
        condition="input.tabs=='About'",
        h4('About')
        )
    ),
    mainPanel(
      tabsetPanel(
        # Show plots from single experiment
        tabPanel("Graph peak calls",
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2")),
                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot3"), plotOutput("plot4")),
                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot5"), plotOutput("plot6"))
                 )
        ),
        # Upload multiple data
        tabPanel("Compare multiple experiments",
                 textOutput('title'),
                 tableOutput("tables")
        ),
        # About panel
        tabPanel("About",
  				HTML('<p>This application was developed for analysis of microscopy based
  				     timecorse data of beating aggregates. The program will load in data
  				    and return a dataframe of cardiac specific measurments. </p>')
  			),
  			id='tabs')
    )
  )
)
