library(shiny)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(DT)
library(shinyWidgets)

policing_data <- read.xlsx("Completed Policing Legislation-4SB.xlsx", detectDates = TRUE)
topics_data <- read.csv("topics_data.csv")

# cleaning up column names
policing_data <- rename(policing_data, "LawNum" = `Law.Number.(for.title.of.pdf)`,
                        "Status" = `Status.(failed/enacted/pending)`,
                        "Local" = Local.Level)

# Creating Year column (taking last 4 characters of Date column)
# For observations with typo in Date column/missing Date, used first 4 characters of law number
policing_data <- policing_data %>% 
    mutate(Year = substr(policing_data$Date, 1, 4)) %>% 
    mutate(Year = if_else(Year > "2100" | Year < "2017" | is.na(Year), substr(policing_data$LawNum, 1, 4), Year))

# Fixing typo in Year column
policing_data <- policing_data %>% 
    mutate(Year = if_else(LawNum == "WI 21 2020", "2020", Year))

# Changing Year column class to numeric
#NAs introduced by coercion here
policing_data$Year <- as.numeric(policing_data$Year)

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

policing_data$Topic <- 
    str_replace(policing_data$Topic, "\\bbody cam\\b", "body cameras")

# Changing order of columns (making Notes column last, moving Year next to Date)
policing_data <- policing_data %>% 
    select(State:Title, Year, Date:Notes)
    
ui <- fluidPage(
    setBackgroundColor(color = "LemonChiffon"),
    h2("Policing Legislation Registry"),
    sidebarLayout(
        sidebarPanel(h4("Narrow results by:"),
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
                     br(),
                     
                     uiOutput('resetable_topic'),
                     actionButton("reset_topic", "Reset topic filter"),
                     br(),
                     br(),
                     
                     sliderInput(
                         inputId = "year",
                         label = "Year",
                         min = min(policing_data$Year, na.rm = T),
                         max = max(policing_data$Year, na.rm = T),
                         sep = "",
                         value = c(min(policing_data$Year, na.rm = T), 2021)
                     ),
        ),
        mainPanel(
            fluidRow(
                column(width = 8,
                       box(div(DT::dataTableOutput("policing_table"),
                               style = "width: 75%"), width = NULL)
                )
            )
        )
    )
)

server <- function(input, output, session) {

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
    
    filtered_topic <- reactive({
        if(input$topic == "All"){
            filtered_status()
        } else {
            filtered_status() %>% 
                filter(str_detect(Topic, fixed(as.character(input$topic), ignore_case = TRUE)))
        }
    })
    
    filtered_year <- reactive({
        filtered_topic() %>% 
            filter(Year >= input$year[1] & Year <= input$year[2])
    })
    
    output$policing_table <- DT::renderDataTable({
        datatable(
            # options = list(
            #     scrollX=TRUE,
            #     autoWidth = TRUE
            # ),
            data = filtered_year(), rownames = FALSE
        )
    })

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
    
    ### How to make Local filter auto-update choices depending on what state is selected?
    # local_choices <- reactiveValues()
    # 
    # observe({
    #     if(input$state == "All"){
    #         local_choices$local <- unique(policing_data$Local)
    #     } else {
    #         tmp <- policing_data %>% 
    #             filter(State == input$State) 
    #         local_choices$local <- unique(tmp$Local[!is.na(tmp$Local)])
    #     }
    #     updateSelectInput(session, "local", choices = local_choice$local)
    # })
    
    output$resetable_local <- renderUI({
        # lchoices <- local_choices$local
        times <- input$reset_local
        div(id = letters[(times %% length(letters))+1],
            selectInput(
                inputId = "local",
                label = "City/county:",
                #choices = c(unique(filtered_state()$Local))
                choices = c("All", unique(policing_data$Local)),
                #choices = "",
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
                # There's an NA choice since some observations don't have a Status listed
                #multiple = TRUE,
                selected = "All"
            )
        )
    })
    
    output$resetable_topic <- renderUI({
        times <- input$reset_topic
        div(id = letters[(times %% length(letters))+1],
            selectInput(
                inputId = "topic",
                label = "Topic of bill:",
                choices = c("All", as.character(unique(topics_data$topic))),
                selected = "All"
            )
        )
    })
}
#clearing all filters at once?
#the local filter i have in mind: possible choices for local input change based on what state
#is selected

# Run the application 
shinyApp(ui = ui, server = server)
