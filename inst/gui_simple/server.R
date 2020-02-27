library(shiny)
library(IDA)
library(reshape2)
library(lattice)
library(ggplot2)
library(openxlsx)
library(readxl)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
    output$modelSelection <- renderUI({
    cur_dat<-datasetInput()
    selectInput("dmod", "Model:",
               c("all",as.character(unique(cur_dat$model))), multiple = TRUE,selected = NULL)
    })
    output$scenSelection <- renderUI({
    cur_dat<-datasetInput()
    selectInput("dscen", "Scenario:",
               c("all", as.character(unique(cur_dat$scenario))), multiple = TRUE,selected = NULL)
 })
   output$regionSelection <- renderUI({
   cur_dat<-datasetInput()
   selectInput("dreg", "Region:",
               c("all", as.character(unique(cur_dat$spatial))), multiple = TRUE, selected = NULL)
 })

 output$varSelection <- renderUI({
   cur_dat<-datasetInput()
   selectInput("dvar", "Variable:", as.character(unique(cur_dat$variable)), multiple = TRUE, selected = NULL)
 })
 #Warning: Error in $: object of type 'closure' is not subsettable means you are
 #calling datasetInput with $.


  # Return the requested dataset
    datasetInput <- reactive({
         filter(idata, source_id == input$dsource)})

    datasetchosen_in <- reactive({
  #  cur_dat<-datasetInput()
    if (input$dreg == "all"){
         filter(idata, source_id == input$dsource,
              model == input$dmod,
              scenario %in% input$dscen,
              variable == input$dvar)
    }else if ("all" %in% input$dmod ){
      filter(idata, source_id == input$dsource,
             scenario %in% input$dscen,
             variable == input$dvar,
             spatial %in% input$dreg)
    }else if ("all" %in% input$dscen){# == "all"){
      filter(idata, source_id == input$dsource,
             model == input$dmod,
             variable == input$dvar,
             spatial %in% input$dreg)
      }else if ((is_empty(input$dscen)))
      {
        filter(idata, source_id == input$dsource)
      }else{
        filter(idata, source_id == input$dsource,
             model == input$dmod,
             scenario %in% input$dscen,
             variable == input$dvar,
            spatial %in% input$dreg
        )}

  })

    datasetchosen <- reactive({
      cur_dat <- datasetchosen_in()
      if(input$interp == 1){
        cur_dat <- interpolate_missing_years(cur_dat,method="linear")
      }else{
        cur_dat <- cur_dat
      }
      })

    #Generate a summary of the dataset
    output$summary <- renderPrint({
        dataset <- datasetInput()
          summary(dataset$variable)
  })

  # Show the first "n" observations
    output$view <- renderTable({
    if(input$tableButton == 0) {
        return()
    }else{
      cur_dat <-datasetchosen()
      if(input$interp == 1){
        cur_dat <- interpolate_missing_years(cur_dat,method="linear")
        head(cur_dat, n = input$obs)
      }else{
        head(cur_dat, n = input$obs)
      }}
  })


  # Plot
    plotInput <- reactive({
      dataset <- datasetchosen()
      dataset <- dataset[c("temporal","spatial","value","scenario","variable","unit","model")]
      if ((input$dreg == "all")){
        p <- ggplot(data = dataset, aes(temporal,value))
        p + geom_line(aes(colour = factor(interaction(variable, spatial)),
                          pch = factor(interaction(variable,spatial))))+
          facet_grid(. ~ scenario)+
          theme_bw() +
          labs(pch= "", colour = "") +
          ylab(dataset$unit) +
          xlab("")
      }else if ("all" %in% input$dscen ){
        # with a length of spatial argument here, I'll add a condition for
        # facetting also regionally
        p <- ggplot(data = dataset, aes(temporal,value))
        p + geom_point(aes(colour = factor(interaction(variable,scenario)),
                           pch = factor(interaction(variable,scenario))))+
             #   facet_grid(. ~ scenario)+
                theme_bw() +
                labs(pch= "", colour = "") +
                ylab(dataset$unit) +
                xlab("")
       # p <- ggplot(data = dataset, aes(temporal,value), group_by(scenario)) +
        #  geom_point(pch=20)+
         # ylab(dataset$unit) +
          #xlab("")
      }else if ("all" %in% input$dmod){
        if(length(input$dreg)==1){
        p <- ggplot(data = dataset, aes(temporal,value))
        p + geom_line(aes(colour = factor(interaction(model, scenario)),
                          pch = factor(interaction(scenario,scenario))))+
        #  facet_grid(. ~ scenario)+
          theme_bw() +
          labs(pch= "", colour = "") +
          ylab(dataset$unit) +
          xlab("")
        }else{
          p <- ggplot(data = dataset, aes(temporal,value))
          p + geom_line(aes(colour = factor(interaction(model, scenario)),
                            pch = factor(interaction(scenario,scenario))))+
            facet_grid(. ~ spatial)+
            theme_bw() +
            labs(pch= "", colour = "") +
            ylab(dataset$unit) +
            xlab("")
        }
    #    p <- ggplot(data = dataset, aes(temporal,value))
    #    p + geom_line(aes(colour = factor(variable, spatial)))+
    #      facet_grid(. ~ scenario)+
    #      theme_bw() +
    #      labs(pch= "", colour = "") +
    #      ylab(dataset$unit) +
    #      xlab("")
      }else{
      p <- ggplot(data = dataset, aes(temporal,value))
      p + geom_line(aes(colour = factor(interaction(variable, spatial)),
                            linetype = factor(interaction(variable, spatial))))+
                facet_grid(. ~ scenario)+
                theme_bw() +
                labs(linetype= "", colour = "") +
                ylab(dataset$unit) +
                xlab("")
      }
      })

    # Print plot
      output$graph_spatial <- renderPlot({
      print(plotInput())
  })

    # Save the plot
      output$downloadFigure <- downloadHandler(
      filename =   "curdat.png",
      content = function(filename) {
          dataset <- datasetchosen()
          ggsave(filename, plot = plotInput(), device = "png")
     })

   # Write out Excel
    output$downloadData <- downloadHandler(
        filename =   "curdat.xlsx",
        content = function(filename) {
        dataset <- datasetchosen()
        write.xlsx(dataset, filename)
     })

  # Read additional data from Excel or csv
    output$exter <- reactive({
      if(input$externalButton == 0){
      }else{
        read_excel(paste(inFile$datapath, "input.xlsx", sep=""), 1)
      }
    })

    # Write out Excel for a full source
    output$downloadData <- downloadHandler(
      filename =   "full_source.xlsx",
      content = function(filename) {
        dataset <- datasetchosen()
        write.xlsx(dataset, filename)
      })

  # Map between data sets ()


  # Show model and variable mapping per source

#  Here a function that always shows each model, source combination
#  as a pdf. But also can show every scenario and variable combination for these
#  and regions.

  # Simple unit conversions




})
