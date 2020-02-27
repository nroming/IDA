library(shiny)
library(IDA)

# Define UI for application that plots a data set
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Filters"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
      selectInput("dsource", "Source:",
                choices = unique(idata$source_id)),
      uiOutput("modelSelection"),
      uiOutput("scenSelection"),
      uiOutput("varSelection"),
      uiOutput("regionSelection"),
    
      numericInput("obs", "Number of observations to view:", 10),
      checkboxInput("interp",label="interpolate",value=FALSE),
     
   #   checkboxInput("re-map", label="re-map"),
  #    checkboxInput("join data",label = "join data"),
    #  actionButton("interpButton", label = "Interpolate"),
      
      actionButton("tableButton", label = "Show"),
      #actionButton("externalButton", label = "External"),
      downloadButton("downloadData", "Download"),
   downloadButton("downloadFigure", "Save plot"),
    downloadButton("print_all",label="print all")
  ),

  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(

    tableOutput("view"),
    
    plotOutput("graph_spatial")#, click = "plot_click")
  )
))
