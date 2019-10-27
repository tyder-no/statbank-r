# source("shinytut-1.R")

library(shiny)

# Running built-in - we do ut explicitly by pasting the code here
# runExample("01_hello")

#
#runExample("02_text")

#
#runExample("03_reactivity")

# Example 01  ..........  

# Define UI for app that draws a histogram ----
ui01 <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server01 <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")

    })

}

# Example 02 .....................

# Define UI for dataset viewer app ----
ui02 <- fluidPage(

  # App title ----
  titlePanel("Shiny Text"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),

      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view")

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server02 <- function(input, output) {

  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })

  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

}


# Example 03 .....................

# Define UI for dataset viewer app ----
ui03 <- fluidPage(

  # App title ----
  titlePanel("Reactivity"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),

      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),

      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),

      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view")

    )
  )
)

# Define server logic to summarize and view selected dataset ----
server03 <- function(input, output) {

  # Return the requested dataset ----
  # By declaring datasetInput as a reactive expression we ensure
  # that:
  #
  # 1. It is only called when the inputs it depends on changes
  # 2. The computation and result are shared by all the callers,
  #    i.e. it only executes a single time
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })

  # Generate a summary of the dataset ----
  # The output$summary depends on the datasetInput reactive
  # expression, so will be re-executed whenever datasetInput is
  # invalidated, i.e. whenever the input$dataset changes
  output$summary <- renderPrint({
      dataset <- datasetInput()
      summary(dataset)
  })

  # Show the first "n" observations ----
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so it will be re-executed whenever
  # input$dataset or input$obs is changed
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })

}

# Create Shiny app ----



# Create Shiny app ----
#shinyApp(ui = ui01, server = server01)
#shinyApp(ui = ui02, server = server02)
#shinyApp(ui=ui03, server=server03)




                                        #
#runApp("ex_MPG")

#runApp("EduInc_0")

#runApp("EduInc_1")

#                                     runApp("EduInc_1")   #Code license: MIT


## Don't have to prepare the data 

#> load("EduInc_1/ddf.Rdata")
#> table(ddf$utdkode)

#342299 343101 343201 343202 343203 343299 344199 349901 349902 349999 352203 
#     7      2      7     33      1    116      3     14      1      3      1 
#383202 383299 389999 399999 401101 401102 401103 401104 401105 401107 401108 
#     1      1      5      9    565      2    132      7     38      4      1 
#401109 401110 401111 401112 401113 401114 401115 401199 415104 415201 415299 
#     1    226    219    313     19     16      2     95     35     16      1 
#455211 455212 455213 455214 455216 455217 455221 455222 455224 455225 455226 
#     5     24     24      2     84      1      1     15      2     38     20 
#455228 455229 455230 455231 455233 455234 455236 455238 455239 455241 455242 
#    21      6      1     14     13     56     15      2     14      5      1 
#616999 619901 619902 619904 619999 621101 621102 621105 621106 621107 621199 
#     5     18    115      3     50     14    103    100      1      1     19 
#622101 622102 622103 622104 622105 622107 622108 622110 622112 622113 622115 
#   159      1      3     38      1      3      4      1     10      6      1 
#759917 759918 759921 759923 759927 759928 759930 759999 761102 761103 761104 
#     2      1      1      1      1      2      3    107      1      1      1 
#761105 761106 761199 762102 762104 762105 762109 762903 763101 763102 763999 
#     4      1      1      1      3      2      1      1    126      5      5 






#> load("EduInc_1/infDf.Rdata")
#> ddf <- infDf
#> runApp("EduInc_1")

