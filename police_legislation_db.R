library(shiny)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(DT)
library(shinyWidgets)

policing_data <- read.xlsx("Completed Policing Legislation-4.xlsx", detectDates = TRUE)
policing_data <- policing_data %>% 
    mutate(Year = substr(policing_data$Date, 1, 4)) %>% 
    mutate(Year = if_else(grepl("\\D", Year), "0", Year))

policing_data$Year <- as.numeric(policing_data$Year)
#### NEED TO FIX YEARS
#### NEED TO FIX STATES (looking a lot like arfwm clinics)

#db_years <- as.vector(unique(policing_data$Year))

# cleaning up column names
policing_data <- rename(policing_data, "LawNum" = `Law.Number.(for.title.of.pdf)`,
                        "Status" = `Status.(failed/enacted/pending)`)

# Cleaning Status column (NOT FINAL; adjust later)
policing_data <- policing_data %>% 
    mutate(Status = if_else(str_detect(Status, "(?i)enacted") == TRUE, "enacted", Status)) %>% 
    mutate(Status = if_else(str_detect(Status, "(?i)pending") == TRUE, "pending", Status)) %>% 
    mutate(Status = if_else(str_detect(Status, "(?i)failed") == TRUE, "failed", Status))


ui <- fluidPage(
    setBackgroundColor(color = "PaleGoldenRod"),
    h2("Policing Legislation Registry"),
    sidebarLayout(
        sidebarPanel(h5("Narrow results by:"),
                     uiOutput('resetable_state'),

                     actionButton("reset_state", "Reset state filters"),
                     br(),
                     br(),
                     
                     uiOutput('resetable_status'),
                     
                     actionButton("reset_status", "Reset status filters"),
                     
                     
                     ####fix slider after cleaning years
                     sliderInput(
                         inputId = "year",
                         label = "Year", min = 1930, max = 2021,
                         sep = "",
                         value = c(2020, 2021)
                     ),
                     # br(),
                     # actionButton('select', 'Select')
        ),
        mainPanel(
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
    
    filtered_status <- reactive({
        if(input$status == "All"){
            filtered_state()
        } else {
            filtered_state() %>% 
                filter(Status == input$status)
        }
    })
    
    filtered_year <- reactive({
        filtered_status() %>% 
            filter(Year >= input$year[1] & Year <= input$year[2])
    })
    
    final_filtered <- eventReactive(input$select, {
        filtered_year()
    })
    
    output$policing_table <- renderDataTable({
        datatable(
            #data = final_filtered(), rownames = FALSE
            data = filtered_year(), rownames = FALSE
        )
    })
    
    # output$rowcount <- renderInfoBox({
    #     infoBox(
    #         "Number of total observations:",
    #         as.character(nrow(policing_data))
    #     )
    # })
    
    output$resetable_state <- renderUI({
        times <- input$reset_state
        div(id = letters[(times %% length(letters))+1],
            selectInput(
                inputId = "state",
                label = "State:",
                choices = c("All", unique(policing_data$State)),
                #multiple = TRUE,
                selected = "All"
            )
        )

    })
    output$resetable_status <- renderUI({
        times <- input$reset_status
        div(id = letters[(times %% length(letters))+1],
            selectInput(
                inputId = "status",
                label = "Status (failed/enacted/pending):",
                choices = c("All", unique(policing_data$Status)),
                #multiple = TRUE,
                selected = "All"
            )
        )
    })

}

#how to make the table go back to normal after u clear the filters??
#how to clear all filters at once?

# Run the application 
shinyApp(ui = ui, server = server)
