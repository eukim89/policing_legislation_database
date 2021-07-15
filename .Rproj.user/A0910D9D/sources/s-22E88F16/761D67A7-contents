library(shiny)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(DT)

policing_data <- read.xlsx("Completed Policing Legislation-4.xlsx", detectDates = TRUE)
policing_data <- policing_data %>% 
    mutate(Year = substr(policing_data$Date, 1, 4)) %>% 
    mutate(Year = if_else(grepl("\\D", Year), "0", Year))

policing_data$Year <- as.numeric(policing_data$Year)
#### NEED TO FIX YEARS
#### NEED TO FIX STATES (looking a lot like arfwm clinics)
    # if it has a "-", contains "City", or has an extra space, conform to state name
#db_years <- as.vector(unique(policing_data$Year))

ui <- fluidPage(
    dashboardHeader(title = "Policing Legislation Registry"),
    sidebarLayout(
        sidebarPanel(width = 2,
                     h5("Narrow results by:"),
                     selectInput(
                         inputId = "state",
                         label = "State:",
                         choices = c("All", unique(policing_data$State)),
                         #multiple = TRUE,
                         selected = "All"
                         ),
                     
                     ####fix slider after cleaning years
                     sliderInput(
                         inputId = "year",
                         label = "Year", min = 1930, max = 2021,
                         sep = "",
                         value = c(2020, 2021)
                     ),
                     br(),
                     actionButton('select', 'Select')
        ),
        mainPanel(
            width = 10,
            # fluidRow(
            #     infoBoxOutput("rowcount")
            # ),
            fluidRow(
                column(width = 8,
                       box(dataTableOutput("policing_table"), width = NULL)
                )
            )
        )
    )
)

server <- function(input, output) {
    
    filtered_state <- reactive({
        if(input$state == "All"){
            policing_data
        } else {
            policing_data %>% 
                filter(State == input$state)
        }
    })
    
    filtered_year <- reactive({
        filtered_state() %>% 
            filter(Year >= input$year[1] & Year <= input$year[2])
    })
    
    final_filtered <- eventReactive(input$select, {
        filtered_year()
    })
    
    output$policing_table <- renderDataTable({
        datatable(
            data = final_filtered(), rownames = FALSE
        )
    })
    
    output$rowcount <- renderInfoBox({
        infoBox(
            "Number of total observations:",
            as.character(nrow(policing_data))
        )
    })
    
    # year_filter_data <- reactive({
    #     subset(policing_data, Year %in% input$years)
    # })
    # creating a reactive expression so observeEvent can observe multiple inputs
    # toListen <- reactive({
    #     list(input$years,input$states)
    # })
    # 
    # observeEvent(toListen(),{
    #     policing_data_filter <- policing_data %>% 
    #         filter(Year == input$years && State == input$State)
    #     output$policing_table <- renderDataTable({policing_data_filter})
    #     
    # }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # observeEvent(input$years,{
    #     policing_data_year <- policing_data %>% 
    #         filter(Year == input$years)
    #     output$policing_table <- renderDataTable({policing_data_year})
    # })

}

#how to make the table go back to normal after u clear the filters??
#how to clear all filters at once?

# Run the application 
shinyApp(ui = ui, server = server)
