library(shiny)
library(datasets)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(pander)


ui <- shinyUI(fluidPage(
    titlePanel("Quick Data Insights"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"')
                         
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
        tabPanel("Graph view",
                 pageWithSidebar(
                     headerPanel('My First Plot'),
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
        
        tabPanel("Column Insights",
                 fluidPage(
                     titlePanel("My first Shiny app!"),
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("depVar",label="Choose a variable",choice=c("Sepal.Length"=1,
                                                                                  "Sepal.Width"=2,
                                                                                  "Petal.Length"=3,
                                                                                  "Petal.Width"=4), selectize=FALSE),
                         selectInput("indVar",label="Choose a variable",choice=c("Sepal.Length"=1,
                                                                                 "Sepal.Width"=2,
                                                                                 "Petal.Length"=3,
                                                                                 "Petal.Width"=4), selectize=FALSE)),
                         mainPanel(
                             h2("Summary of the variable"),
                             verbatimTextOutput("sum"),
                             plotOutput("box")
                         )
                     ))
        ),
        
        
        tabPanel("Linear Regression",
                 sidebarLayout(
                     sidebarPanel(
                         tags$b("Data:"),
                         textInput("x", "x", value = "90, 100, 90, 80, 87, 75", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
                         textInput("y", "y", value = "950, 1100, 850, 750, 950, 775", placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
                         hr(),
                         tags$b("Plot:"),
                         checkboxInput("se", "Add confidence interval around the regression line", TRUE),
                         textInput("xlab", label = "Axis labels:", value = "x", placeholder = "x label"),
                         textInput("ylab", label = NULL, value = "y", placeholder = "y label"),
                         hr()
                          ),
                     
                     mainPanel(
                         tags$b("Your data:"),
                         DT::dataTableOutput("tbl"),
                         br(),
                         uiOutput("data"),
                         br(),
                         tags$b("Compute parameters by hand:"),
                         uiOutput("by_hand"),
                         br(),
                         tags$b("Compute parameters in R:"),
                         verbatimTextOutput("summary"),
                         br(),
                         tags$b("Regression plot:"),
                         uiOutput("results"),
                         plotlyOutput("plot"),
                         br(),
                         tags$b("Interpretation:"),
                         uiOutput("interpretation"),
                         br(),
                         br()
                     )
                 )
        )
        
    )
)
)


server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it
    
    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    
    # Data output
    output$tbl <- DT::renderDataTable({
        y <- extract(input$y)
        x <- extract(input$x)
        DT::datatable(data.frame(x, y),
                      extensions = "Buttons",
                      options = list(
                          lengthChange = FALSE,
                          dom = "Blfrtip",
                          buttons = c("copy", "csv", "excel", "pdf", "print")
                      )
        )
    })
    
    output$data <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
            "Invalid input or not enough observations"
        } else if (length(x) != length(y)) {
            "Number of observations must be equal for x and y"
        } else {
            withMathJax(
                paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
                br(),
                paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
                br(),
                paste0("\\(n =\\) ", length(x))
            )
        }
    })
    
    
    output$by_hand <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        withMathJax(
            paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 3)),
            br(),
            paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
            br(),
            br(),
            paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
        )
    })
    
    output$summary <- renderPrint({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        summary(fit)
    })
    
    output$results <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        withMathJax(
            paste0(
                "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
                ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
                ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),
                ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
            )
        )
    })
    
    output$interpretation <- renderUI({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                paste0("\\( \\beta_1 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$xlab, " and ", input$ylab, ".")
            )
        } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
                br(),
                paste0("For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0.")
            )
        }
    })
    
    output$plot <- renderPlotly({
        y <- extract(input$y)
        x <- extract(input$x)
        fit <- lm(y ~ x)
        dat <- data.frame(x, y)
        p <- ggplot(dat, aes(x = x, y = y)) +
            geom_point() +
            stat_smooth(method = "lm", se = input$se) +
            ylab(input$ylab) +
            xlab(input$xlab) +
            theme_minimal()
        ggplotly(p)
    })
    
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste("my-report", sep = ".", switch(
                input$format, PDF = "pdf", HTML = "html", Word = "docx"
            ))
        },
        
        content = function(file) {
            src <- normalizePath("report.Rmd")
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, "report.Rmd", overwrite = TRUE)
            
            library(rmarkdown)
            out <- render("report.Rmd", switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )
    
    
    
    data <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        
        inFile <- input$file1 
        
        # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
        # and                              write.csv(iris, "iris.csv")
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted select only numeric
        # variables you could set "choices = sapply(df, is.numeric)"
        # It depends on what do you want to do later on.
        
        updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        #Input variables for linear regression
        updateSelectInput(session, inputId = 'indVar', label = 'X Variable',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'depVar', label = 'Y Variable',
                          choices = names(df), selected = names(df)[2])
        
        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlot({
        # for a histogram: remove the second variable (it has to be numeric as well):
        # x    <- data()[, c(input$xcol, input$ycol)]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        # Correct way:
        # x    <- data()[, input$xcol]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
        # I Since you have two inputs I decided to make a scatterplot
        x <- data()[, c(input$xcol, input$ycol)]
        plot(x)
        
    })

    
    output$sum <- renderPrint({
        
        lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar))})
        summary(lm1)
    })
    
  
})

shinyApp(ui, server)