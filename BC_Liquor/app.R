library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  
  ##Feature 1: added an image of the BC Liquor Store.
  img(src = "logo.png", height="20%", width="20%"),
  
  titlePanel("BC Liquor Store prices"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput"),
      # Feature 2: sort data by price
      checkboxInput("sortInput", "Sort results by price", TRUE)
    ),
    mainPanel(
      # Feature 3: separate plot and table tabs
      tabsetPanel(
        tabPanel("Plot", plotOutput("coolplot")),
        tabPanel("Table", DT::dataTableOutput("results"))# Feature 4: interactive table
      )
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    # assign it to the unsorted 
    unsorted <-
      bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
    
    # Feature 2: sort data by price
    if (input$sortInput) {
      unsorted %>%
        arrange(Price)
    } else {
      unsorted
    }
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  # Feature 4: interactive table
  output$results <- DT::renderDataTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)

