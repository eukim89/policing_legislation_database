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

### Getting unique topics list ###
topics_data <- as.data.frame(unique(unlist(strsplit(as.character(policing_data$Topic), ";")))) %>% 
    rename("topic" = "unique(unlist(strsplit(as.character(policing_data$Topic), \";\")))") 
# Renaming column to topic
# Chose to make a separate topics_data data frame to just have a database of topics
#and leave the original data the same

topics_data$topic <- as.character(gsub('\\s$', "", topics_data$topic))
topics_data$topic <- as.character(gsub('^\\s', "", topics_data$topic)) 
# Removing trailing and leading white spaces

topics_data <- as.data.frame(unique(topics_data$topic)) %>% 
    rename("topic" = "unique(topics_data$topic)")
# Renaming column to topic

topics_data$topic[topics_data$topic == ""] <- NA
#Removing "" observations

### Cleaning unique topics list ###
topics_data <- topics_data %>% 
    mutate(topic = if_else(str_detect(topic, "(?i)bail"), "bail", topic),
           topic = if_else(str_detect(topic, "(?i)arrest"), "arrest", topic),
           topic = if_else(str_detect(topic, "(?i)body cam"), "body cameras", topic),
           topic = if_else(str_detect(topic, "\\bcertification\\b"), "certification", topic),
           topic = if_else(str_detect(topic, "(?i)civil liab"), "civil liability", topic),
           topic = if_else(str_detect(topic, "(?i)criminal liab"), "criminal liability", topic),
           topic = if_else(str_detect(topic, "(?i)data|(?i)data coll"), "data collection", topic),
           topic = if_else(str_detect(topic, "^de\\w*ification$"), "decertification", topic),
           topic = if_else(str_detect(topic, "biometric"), "biometric data", topic),
           topic = if_else(str_detect(topic, "^dis\\w*ity$"), "disability", topic),
           topic = if_else(str_detect(topic, "^disc\\w*ine$"), "discipline", topic),
           topic = if_else(str_detect(topic, "disciplinary|discipinary"), "disciplinary record disclosure", topic),
           topic = if_else(str_detect(topic, "duty to reprot"), "duty to report", topic),
           topic = if_else(str_detect(topic, "firearm|fire arms"), "firearms", topic),
           topic = if_else(str_detect(topic, "hiring"), "hiring", topic),
           topic = if_else(str_detect(topic, "indigenous"), "indigenous affairs", topic),
           topic = if_else(str_detect(topic, "prosecution of police|proseuction|prosecution"), "investigation/prosecution of police", topic),
           topic = if_else(str_detect(topic, "knock"), "no knock warrants", topic),
           topic = if_else(str_detect(topic, "oversight"), "oversight", topic),
           topic = if_else(str_detect(topic, "^p\\w*nel"), "personnel management", topic),
           topic = if_else(str_detect(topic, "ice in school"), "police in schools", topic),
           topic = if_else(str_detect(topic, "protocal"), "protocol", topic),
           topic = if_else(str_detect(topic, "immunity"), "qualified immunity", topic),
           topic = if_else(str_detect(topic, "profil"), "racial profiling", topic),
           topic = if_else(str_detect(topic, "sexual as"), "sexual assault", topic),
           topic = if_else(str_detect(topic, "offender"), "sex offenders", topic),
           topic = if_else(str_detect(topic, "standards"), "standards", topic),
           topic = if_else(str_detect(topic, "study com"), "study commission", topic),
           topic = if_else(str_detect(topic, "training"), "training", topic),
           topic = if_else(str_detect(topic, "use of"), "use of force", topic),
           topic = if_else(str_detect(topic, "search"), "search warrants", topic),
           topic = if_else(str_detect(topic, "youth prog"), "youth programs", topic)) %>% 
    distinct() %>% 
    # Consolidating topics
    na.omit() %>% 
    arrange(topic)
    # arrange in alphabetical order

policing_data$Topic <- 
    str_replace(policing_data$Topic, "\\bbody cam\\b", "body cameras")
    
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
                     # br(),
                     # actionButton('select', 'Select')
        ),
        mainPanel(
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
    
    final_filtered <- eventReactive(input$select, {
        filtered_year()
    })
    
    output$policing_table <- renderDataTable({
        datatable(
            #data = final_filtered(), rownames = FALSE
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
                # There's an NA choice since some observations don't have a Status listed
                #multiple = TRUE,
                selected = "All"
            )
        )
    })
    
    output$resetable_topic <- renderUI({
        times <- input$reste_topic
        div(id = letters[(times %% length(letters))+1],
            selectInput(
                inputId = "topic",
                label = "Topic of bill:",
                choices = c("All", unique(topics_data$topic)),
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
