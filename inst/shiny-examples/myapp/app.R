#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyAce)
library(rpivotTable)
library(corrgram)
# Define UI for application that draws a histogram

ui <- fluidPage(
  h6(" Quick Visual Analytics/BI Authored by:", tags$img(src ="K.JPG", height=100, width=100)),
  verbatimTextOutput("preface"),
  tags$img(src = "T.png"),
  helpText("Copy paste data with variable names from Excel"),
  aceEditor("text", value=
              "Sepal.Length	Sepal.Width	Petal.Length	Petal.Width	Species
5.1	3.5	1.4	0.2	setosa
            4.9	3	1.4	0.2	setosa
            4.7	3.2	1.3	0.2	setosa
            4.6	3.1	1.5	0.2	setosa
            5	3.6	1.4	0.2	setosa
            5.4	3.9	1.7	0.4	setosa
            4.6	3.4	1.4	0.3	setosa
            5	3.4	1.5	0.2	setosa
            4.4	2.9	1.4	0.2	setosa
            4.9	3.1	1.5	0.1	setosa
            5.4	3.7	1.5	0.2	setosa
            4.8	3.4	1.6	0.2	setosa
            4.8	3	1.4	0.1	setosa
            4.3	3	1.1	0.1	setosa
            5.8	4	1.2	0.2	setosa
            5.7	4.4	1.5	0.4	setosa
            5.4	3.9	1.3	0.4	setosa
            5.1	3.5	1.4	0.3	setosa
            5.7	3.8	1.7	0.3	setosa
            5.1	3.8	1.5	0.3	setosa
            5.4	3.4	1.7	0.2	setosa
            5.1	3.7	1.5	0.4	setosa
            4.6	3.6	1	0.2	setosa
            5.1	3.3	1.7	0.5	setosa
            4.8	3.4	1.9	0.2	setosa
            5	3	1.6	0.2	setosa
            5	3.4	1.6	0.4	setosa
            5.2	3.5	1.5	0.2	setosa
            5.2	3.4	1.4	0.2	setosa
            4.7	3.2	1.6	0.2	setosa
            4.8	3.1	1.6	0.2	setosa
            5.4	3.4	1.5	0.4	setosa
            5.2	4.1	1.5	0.1	setosa
            5.5	4.2	1.4	0.2	setosa
            4.9	3.1	1.5	0.2	setosa
            5	3.2	1.2	0.2	setosa
            5.5	3.5	1.3	0.2	setosa
            4.9	3.6	1.4	0.1	setosa
            4.4	3	1.3	0.2	setosa
            5.1	3.4	1.5	0.2	setosa
            5	3.5	1.3	0.3	setosa
            4.5	2.3	1.3	0.3	setosa
            4.4	3.2	1.3	0.2	setosa
            5	3.5	1.6	0.6	setosa
            5.1	3.8	1.9	0.4	setosa
            4.8	3	1.4	0.3	setosa
            5.1	3.8	1.6	0.2	setosa
            4.6	3.2	1.4	0.2	setosa
            5.3	3.7	1.5	0.2	setosa
            5	3.3	1.4	0.2	setosa
            7	3.2	4.7	1.4	versicolor
            6.4	3.2	4.5	1.5	versicolor
            6.9	3.1	4.9	1.5	versicolor
            5.5	2.3	4	1.3	versicolor
            6.5	2.8	4.6	1.5	versicolor
            5.7	2.8	4.5	1.3	versicolor
            6.3	3.3	4.7	1.6	versicolor
            4.9	2.4	3.3	1	versicolor
            6.6	2.9	4.6	1.3	versicolor
            5.2	2.7	3.9	1.4	versicolor
            5	2	3.5	1	versicolor
            5.9	3	4.2	1.5	versicolor
            6	2.2	4	1	versicolor
            6.1	2.9	4.7	1.4	versicolor
            5.6	2.9	3.6	1.3	versicolor
            6.7	3.1	4.4	1.4	versicolor
            5.6	3	4.5	1.5	versicolor
            5.8	2.7	4.1	1	versicolor
            6.2	2.2	4.5	1.5	versicolor
            5.6	2.5	3.9	1.1	versicolor
            5.9	3.2	4.8	1.8	versicolor
            6.1	2.8	4	1.3	versicolor
            6.3	2.5	4.9	1.5	versicolor
            6.1	2.8	4.7	1.2	versicolor
            6.4	2.9	4.3	1.3	versicolor
            6.6	3	4.4	1.4	versicolor
            6.8	2.8	4.8	1.4	versicolor
            6.7	3	5	1.7	versicolor
            6	2.9	4.5	1.5	versicolor
            5.7	2.6	3.5	1	versicolor
            5.5	2.4	3.8	1.1	versicolor
            5.5	2.4	3.7	1	versicolor
            5.8	2.7	3.9	1.2	versicolor
            6	2.7	5.1	1.6	versicolor
            5.4	3	4.5	1.5	versicolor
            6	3.4	4.5	1.6	versicolor
            6.7	3.1	4.7	1.5	versicolor
            6.3	2.3	4.4	1.3	versicolor
            5.6	3	4.1	1.3	versicolor
            5.5	2.5	4	1.3	versicolor
            5.5	2.6	4.4	1.2	versicolor
            6.1	3	4.6	1.4	versicolor
            5.8	2.6	4	1.2	versicolor
            5	2.3	3.3	1	versicolor
            5.6	2.7	4.2	1.3	versicolor
            5.7	3	4.2	1.2	versicolor
            5.7	2.9	4.2	1.3	versicolor
            6.2	2.9	4.3	1.3	versicolor
            5.1	2.5	3	1.1	versicolor
            5.7	2.8	4.1	1.3	versicolor
            6.3	3.3	6	2.5	virginica
            5.8	2.7	5.1	1.9	virginica
            7.1	3	5.9	2.1	virginica
            6.3	2.9	5.6	1.8	virginica
            6.5	3	5.8	2.2	virginica
            7.6	3	6.6	2.1	virginica
            4.9	2.5	4.5	1.7	virginica
            7.3	2.9	6.3	1.8	virginica
            6.7	2.5	5.8	1.8	virginica
            7.2	3.6	6.1	2.5	virginica
            6.5	3.2	5.1	2	virginica
            6.4	2.7	5.3	1.9	virginica
            6.8	3	5.5	2.1	virginica
            5.7	2.5	5	2	virginica
            5.8	2.8	5.1	2.4	virginica
            6.4	3.2	5.3	2.3	virginica
            6.5	3	5.5	1.8	virginica
            7.7	3.8	6.7	2.2	virginica
            7.7	2.6	6.9	2.3	virginica
            6	2.2	5	1.5	virginica
            6.9	3.2	5.7	2.3	virginica
            5.6	2.8	4.9	2	virginica
            7.7	2.8	6.7	2	virginica
            6.3	2.7	4.9	1.8	virginica
            6.7	3.3	5.7	2.1	virginica
            7.2	3.2	6	1.8	virginica
            6.2	2.8	4.8	1.8	virginica
            6.1	3	4.9	1.8	virginica
            6.4	2.8	5.6	2.1	virginica
            7.2	3	5.8	1.6	virginica
            7.4	2.8	6.1	1.9	virginica
            7.9	3.8	6.4	2	virginica
            6.4	2.8	5.6	2.2	virginica
            6.3	2.8	5.1	1.5	virginica
            6.1	2.6	5.6	1.4	virginica
            7.7	3	6.1	2.3	virginica
            6.3	3.4	5.6	2.4	virginica
            6.4	3.1	5.5	1.8	virginica
            6	3	4.8	1.8	virginica
            6.9	3.1	5.4	2.1	virginica
            6.7	3.1	5.6	2.4	virginica
            6.9	3.1	5.1	2.3	virginica
            5.8	2.7	5.1	1.9	virginica
            6.8	3.2	5.9	2.3	virginica
            6.7	3.3	5.7	2.5	virginica
            6.7	3	5.2	2.3	virginica
            6.3	2.5	5	1.9	virginica
            6.5	3	5.2	2	virginica
            6.2	3.4	5.4	2.3	virginica
            5.9	3	5.1	1.8	virginica

            ",  mode="r", theme="white"),
br(),
br(),
br(),
helpText("HOW  TO USE QUICK BI ? "),
h6(" DO AS SHOWN IN EXAMPLE PICTURE "),
tags$img(src = "H.png"),
br(),
br(),
br(),
helpText("SELECT THE DIMENSIONS AND MEASURES AND PLACE IT IN APPROPRIATE POSITIONS "),
br(),
rpivotTableOutput("distPlot"),
br(),
br(),
h6("Histogram for Continuous/Quantitative Variables Only"),
uiOutput("vx"),
sliderInput("binw", label = "Enter the bin width:",
            min = 1, max = 100, value = 5, step = 1),
selectInput("cl", label = "Select Color for the Histogram",
choices = c("red","light blue","green","pink","magenta","violet"), selected = "green"),
plotOutput("plot3"),
h5("PieChart  for Qualitative Variables Only"),
uiOutput("vy"),
plotOutput("plot4"),
h5("Correlation between Quantitative Variables in the dataset :  DarkBlue is highly positively correlated and Red is highly negatively correlated : lighter or faded versions indicates less correlation in that particular direction"),
plotOutput("plot5")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$vx <- renderUI({
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    dataframe$extra<- as.factor("extra1")
    a =  data.frame(split(names(dataframe),sapply(dataframe, function(x) paste(class(x), collapse=" "))))
    if(is.null(a)){return()}

     selectInput("variablex","Select the variable",choices = unique(a[,-1]),selected = unique(a[,-1]) )


  })
  output$vy <- renderUI({
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
    dataframe<- read.table(text = get.text(),header = TRUE)
    a =  data.frame(split(names(dataframe),sapply(dataframe, function(x) paste(class(x), collapse=" "))))
    selectInput("variabley","Select the variable",choices = unique(a$factor),selected = unique(a$factor) )


  })
  output$preface <-renderPrint({

    cat(sprintf("\nDr.  Kartikeya Bolar\n"))
    cat(sprintf("\nAssociate Professor and Area  Co-Chair\n"))
    cat(sprintf("\nOperations and Information Science\n"))
    cat(sprintf("\nT A  Pai Management Institute\n"))

  })
   output$distPlot <- renderRpivotTable({

     get.text <- reactive({input$text})
     if(is.null(get.text())){return ()}
     dataframe<- read.table(text = get.text(),header = TRUE)
     rpivotTable(dataframe)
   })



   output$plot3 <- renderPlot({
    get.text <- reactive({input$text})
    if(is.null(get.text())){return ()}
     dataframe<- read.table(text = get.text(),header = TRUE)
     if (is.null(input$variablex)) return()
     indexnumber =  grep(input$variablex,colnames(dataframe))

     x = dataframe[,indexnumber]

     bins <- seq(min(x), max(x), length.out = input$binw + 1)

     hist(x, breaks = bins, col = input$cl, border = 'black',xlab = input$variablex ,main = "Histogram")
   })

   output$plot4 <- renderPlot({

     get.text <- reactive({input$text})
     if(is.null(get.text())){return ()}
     dataframe<- read.table(text = get.text(),header = TRUE)
         if (is.null(input$variabley)) return()
     indexnumber =  grep(input$variabley,colnames(dataframe))
     x = dataframe[,indexnumber]
     frequency = table(x)
     lbls = paste(names(frequency), "\n", sep="")
     pct = round(frequency/sum(frequency)*100)
     lbls = paste(lbls, pct) # add percents to labels
     lbls = paste(lbls,"%",sep="") # ad % to labels
     pie(frequency,labels = lbls, col=rainbow(length(frequency)))

   })
   output$plot5 <- renderPlot({
     get.text <- reactive({input$text})
     if(is.null(get.text())){return ()}
     dataframe<- read.table(text = get.text(),header = TRUE)
     corrgram(dataframe)

   })


}

# Run the application
shinyApp(ui = ui, server = server)



