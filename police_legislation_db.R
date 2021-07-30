library(shiny)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(DT)
library(shinyWidgets)

policing_data <- read.xlsx("Completed Policing Legislation-4SB.xlsx", detectDates = TRUE)

# Creating Year column (taking last 4 characters of Date column)
policing_data <- policing_data %>% 
    mutate(Year = substr(policing_data$Date, 1, 4)) %>% 
    mutate(Year = if_else(grepl("\\D", Year), "0", Year))

# Changing Year column class to numeric
policing_data$Year <- as.numeric(policing_data$Year)

# cleaning up column names
policing_data <- rename(policing_data, "LawNum" = `Law.Number.(for.title.of.pdf)`,
                        "Status" = `Status.(failed/enacted/pending)`,
                        "Local" = Local.Level)

# Cleaning Status column
policing_data <- policing_data %>% 
    mutate(Status = if_else(str_detect(Status, "(?i)enacted") == TRUE, "enacted", Status)) %>% 
    mutate(Status = if_else(str_detect(Status, "(?i)pending") == TRUE, "pending", Status)) %>% 
    mutate(Status = if_else(str_detect(Status, "(?i)failed") == TRUE, "failed", Status))

# Changing empty spaces in Local column to NA
policing_data$Local[policing_data$Local == " "] <- NA

# Cleaning Local column typo
policing_data <- policing_data %>% 
    mutate(Local = if_else(Local == "Berkeley City Counci;", "Berkeley City Council", Local))

# Cleaning State column typo
policing_data <- policing_data %>% 
    mutate(State = if_else(str_detect(State, "Minne"), "Minnesota", State),
           State = if_else(str_detect(State, "fornia"), "California", State))

# Removing extra spaces in Local and State columns
policing_data$State <- trimws(policing_data$State)
policing_data$Local <- trimws(policing_data$Local)

ui <- fluidPage(
    setBackgroundColor(color = "LemonChiffon"),
    h2("Policing Legislation Registry"),
    sidebarLayout(
        sidebarPanel(h5("Narrow results by:"),
                     uiOutput('resetable_state'),

                     actionButton("reset_state", "Reset state filter"),
                     br(),
                     br(),
                     
                     uiOutput('resetable_local'),
                     actionButton("reset_local", "Reset city/county filter"),
                     br(),
                     br(),
                     
                     uiOutput('resetable_status'),
                     
                     actionButton("reset_status", "Reset status filter"),
                     br(),
                     
                     ####fix slider after cleaning years
                     sliderInput(
                         inputId = "year",
                         label = "Year", min = min(Year), max = max(Year),
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
            ),
            fluidRow(
                box(plotOutput("policing_legislation_plot"))
            )
        )
    )
)

server <- function(input, output) {
    
    # output$policing_legislation_plot <- renderPlot(
    #     policing_data %>% 
    #         filter(Year %in% c(2021, 2020, 2019)) %>% 
    #         ggplot(mapping = aes(x = Year)) +
    #         geom_bar() +
    #         labs(title = "Legislation by Year")
    # )
    
    filtered_state <- reactive({
        if(input$state == "All"){
            policing_data
        } else {
            policing_data %>% 
                filter(State == input$state)
        }
    })
    
    filtered_local <- reactive({
        if(input$local == "All"){
            filtered_state()
        } else {
            filtered_state() %>% 
                filter(Local == input$local)
        }
    })
    
    filtered_status <- reactive({
        if(input$status == "All"){
            filtered_local()
        } else {
            filtered_local() %>% 
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
    
    output$resetable_local <- renderUI({
        times <- input$reset_local
        div(id = letters[(times %% length(letters))+1],
            selectInput(
                inputId = "local",
                label = "City/county:",
                #choices = c(unique(filtered_state()$Local))
                choices = c("All",  unique(policing_data$Local)),
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
#how to clear all filters at once?
#the local filter i have in mind: possible choices for local input change based on what state
#is selected

# Run the application 
shinyApp(ui = ui, server = server)
