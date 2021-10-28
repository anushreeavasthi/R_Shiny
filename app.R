#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)


server = function (input, output, session){
 
    #To download the user inputted file
   
    output$contents <- renderTable({
       
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
       
        req(input$file1)
       
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
       
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
       
    })
   
    #render table inputted by user
    output$tableDT <- DT::renderDataTable(read.csv(input$file1$datapath,
                                                   header = input$header,
                                                   sep = input$sep,
                                                   quote = input$quote),
                                          options= list(paging=T),
                                          rownames=T, filter="top")
   
    #Download the table rendered by user
    completeTable <- reactive({
      tableDT <-  read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
      return(tableDT)
    })
   
   
    output$fulldownload <- downloadHandler(
      filename= "FullTable.csv",
      content= function(file){
        write.csv(completeTable(),file)
      }
    )
   
   
   
   
   
   
    #implementation of observe
    #just copies text from input to output box
    observe({
        addText <- paste("Your initial input value is:", input$myString)
        updateTextInput (session, "myString2", value=addText)
    })
 
   #Plotting the table
    output$plotYourTable <- renderPlot({
        ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_line()
    })
   
    data <- reactive({
        user_brush <- input$user_brush
        sel <- brushedPoints(iris, user_brush)
        return (sel)
    })
   
 
    output$tableSelection <- DT::renderDataTable(DT::datatable(data()))
   
    output$customdownload <- downloadHandler(
        filename= "CustomTable.csv",
        content= function(file){
            write.csv(data(),file)
        }
    )
   
    #Adding graph based on user input
    data <- reactive({
      req(input$file1$datapath) ## ?req #  require that the input is available
     
      inFile <- input$file1$datapath
     
      # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
      # and                              write.csv(iris, "iris.csv")
      df <-  read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
     
     
      # Update inputs (you could create an observer with both updateSel...)
      # You can also constraint your choices. If you wanted select only numeric
      # variables you could set "choices = sapply(df, is.numeric)"
      # It depends on what do you want to do later on.
     
      updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                        choices = names(df), selected = names(df))
      updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                        choices = names(df), selected = names(df)[2])
     
      return(df)
   
    })
   
    #Plotting the user inputs taken
   
    output$MyPlot <- renderPlot({
      x <- data()[, c(input$xcol, input$ycol)]
      plot(x)
     
    })
   
    #Doing linear regression based on user input
    data_for_linear_regression <- reactive({
      req(input$file1$datapath) ## ?req #  require that the input is available
     
    #  inFile <- input$file1$datapath
     
      # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
      # and                              write.csv(iris, "iris.csv")
      df_lr <-  read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
     
     
      # Update inputs (you could create an observer with both updateSel...)
      # You can also constraint your choices. If you wanted select only numeric
      # variables you could set "choices = sapply(df, is.numeric)"
      # It depends on what do you want to do later on.
     
      updateSelectInput(session, inputId = 'xcolumn', label = 'Dependent variable',
                        choices = names(df_lr), selected = names(df_lr))
      updateSelectInput(session, inputId = 'ycolumn', label = 'Independent variable',
                        choices = names(df_lr), selected = names(df_lr)[2])
     
      return(df_lr)
     
    })
   
    extract <- function(text) {
      text <- gsub(" ", "", text)
      split <- strsplit(text, ",", fixed = FALSE)[[1]]
      as.numeric(split)
    }
   
    output$summary <- renderPrint({
      x <- data_for_linear_regression ()[, input$xcolumn ]
      y <- data_for_linear_regression ()[, input$ycolumn ]
      fit <- lm(y ~ x)
      summary(fit)
    })
   
 
   
}


ui = fluidPage(theme= shinytheme("cerulean"),
   
    navbarPage("Data Insights",
               tabPanel("Insert your file",
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                           
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                # Input: Select a file ----
                                fileInput("file1", "Choose CSV File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                # Horizontal line ----
                                tags$hr(),
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE),
                                # Input: Select separator ----
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),
                                # Input: Select quotes ----
                                radioButtons("quote", "Quote",
                                             choices = c(None = "",
                                                         "Double Quote" = '"',
                                                         "Single Quote" = "'"),
                                             selected = '"'),
                               
                                # Horizontal line ----
                                tags$hr(),
                               
                                # Input: Select number of rows to display ----
                                radioButtons("disp", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head")
                               
                            ),
                           
                            # Main panel for displaying outputs ----
                            mainPanel(
                               
                                # Output: Data file ----
                               h1("Here is what you can do next:"),
                               h3("1. Upload or import (option available later) your data table"),
                               h3("2. Go ahead to View data table to see your downloadable data table "),
                               h3("3. Head over to graph view to vary x and y columns and see your plots"),
                               h3("4. Go ahead and perform linear regressions of your choice")
                               
                            )
                           
                        )),
                tabPanel("View Data Table",
                         DT::dataTableOutput("tableDT"),
                         downloadButton(outputId="fulldownload", label="Download Table")),
               tabPanel("View data graphically",
                        pageWithSidebar(
                          headerPanel('My Data plot'),
                          sidebarPanel(
                           
                            # "Empty inputs" - they will be updated after the data is uploaded
                            selectInput('xcol', 'X Variable', ""),
                            selectInput('ycol', 'Y Variable', "", selected = "")
                           
                          ),
                          mainPanel(
                            plotOutput('MyPlot')
                          )
                        )
               ),
               tabPanel("Linear Regression",
                        pageWithSidebar(
                          headerPanel('My Linear plot'),
                          sidebarPanel(
                           
                            # "Empty inputs" - they will be updated after the data is uploaded
                            selectInput('xcolumn', 'X Variable', ""),
                            selectInput('ycolumn', 'Y Variable', "", selected = "")
                           
                          ),
                          mainPanel(
                            tags$b("Compute parameters in R:"),
                            verbatimTextOutput("summary"),
                            br()
                          )
                       
               )
               ),
               tabPanel("View Selection Data Table",
                        plotOutput("plotYourTable", brush= "user_brush"),
                        DT::dataTableOutput("tableSelection"),
                        downloadButton(outputId="customdownload", label="Download selected")),
               tabPanel("Copy input string example",
                        #data input for observe
                        #just copies text from input to output box
                        h1("Output text from input using observe"),
                        textInput("myString","Give a value"),
                        textInput("myString2","Your full output"),
                        checkboxInput("myCheckbox", "Factor X") )
            )
        )



# Run the application
shinyApp(ui = ui, server = server)
