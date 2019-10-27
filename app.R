#
# taydersbond 20180927

library(shiny)

# Include the business logic
source("eduInc_02.R") ;

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel(h1("Utdanningsveier og inntekt")),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
      sidebarPanel(
          h2("Velg utdanningskode"),
          numericInput("eduCode", 
                        h5(strong("Angi 6-sifra utdanningskode:")), 
                        value = 301110),   
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Antall inntektsgrupper:",
                  min = 1,
                  max = 100,
                  value = 20)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Plot panel ----
      plotOutput(outputId = "distPlot")
    )
  )
)

# Define server logic required to draw the plot panel----
server <- function(input, output) {
    output$distPlot <- renderPlot({
        plotEduCategory(input$eduCode,brks=input$bins)
    })
}

# Main program: Run the shinyApp function
shinyApp(ui = ui, server = server) ;






