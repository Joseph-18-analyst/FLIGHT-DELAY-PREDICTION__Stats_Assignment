library(lubridate)
library(shiny)
load(file = 'model.rda')
choices <-  route_shiny$route
choices1 <- origin_airport_shiny$ORIGIN_AIRPORT
choices2 <- destination_airport_shiny$DESTINATION_AIRPORT
ui <- fluidPage(
  # App title ----
  titlePanel("Flights Delay Predictor"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing Scheduled departure ----
      numericInput(inputId = "SCHEDULED_DEPARTURE",
                  label = "SCHEDULED DEPARTURE IN hhmm :",
                  value = 2200),
      numericInput(inputId = "SCHEDULED_ARRIVAL",
                  label = "SCHEDULED ARRIVAL IN hhmm :",
                  value = 2345),
      selectInput(inputId = "AVAILABLE ROUTES",
                  label = "Select from  below available routes 
                ex:AIRLINE ORIGIN DEST.",
                  choices ),
      selectInput(inputId = "AIRLINE",
                  label = "ENTER AIRLINE FROM ROUTE SELECTED",
                  choices = c("AA","AS","B6","DL","EV","F9","HA","MQ","NK","OO","UA","US","VX","WN")),
      selectInput(inputId = "ORIGIN_AIRPORT",
                  label = "ENTER ORIGIN AIRPORT FROM ROUTE SELECTED",
                  choices1 ),
      selectInput(inputId = "DESTINATION_AIRPORT",
                  label = "ENTER DESTINATION AIRPORT FROM ROUTE SELECTED",
                  choices2 ),
      numericInput(inputId = "SCHEDULED_TIME",
                   label = "SCHEDULED TIME GIVEN BY AIRLINES",
                   value = 50),
      numericInput(inputId = "DISTANCE",
                   label = "DISTANCE",
                   value = 100),
      dateInput(inputId = "date",label="Enter date",
                format = "dd-mm-yyyy"),
      br(),
      actionButton(inputId = "Enter", label = "Predict Delay")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h3(textOutput("caption")),
      # Output: 
      tableOutput("view")
      
    )
  )
)

server <- function(input,output, session) {
  
  formulaText <- reactive({
    paste("DELAY PREDICTION IS :")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  observeEvent(input$Enter, {
    t <- data.frame(month(input$date),day(input$date),wday(input$date),input$AIRLINE,input$ORIGIN_AIRPORT,
                    input$DESTINATION_AIRPORT,input$SCHEDULED_DEPARTURE,input$SCHEDULED_TIME,
                    input$DISTANCE,input$SCHEDULED_ARRIVAL)
    
    names(t) <- c("MONTH","DAY","DAY_OF_WEEK","AIRLINE","ORIGIN_AIRPORT","DESTINATION_AIRPORT","SCHEDULED_DEPARTURE",
                  "SCHEDULED_TIME","DISTANCE","SCHEDULED_ARRIVAL" )
                    
    t <- catboost.load_pool(t)
  
    output$view <- renderPrint({
      catboost.predict(model,t)
    })
  })
}
shinyApp(ui = ui, server = server)
