library(shiny)
library(multilex)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(stringr)
library(gt)
library(here)

ml_connect() # connect to multilex database

# set custom ggplot2 theme
theme_set(
    theme_minimal() +
        theme(
            axis.text = element_text(colour = "black"), 
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.border = element_rect(
                fill = NA,
                colour = "grey", 
                size = 0.75
            )
        )
)

# define user interface 
ui <- fluidPage(
    
    navbarPage(
        "multilex",
        tabPanel(
            "Participants",
            titlePanel("Participants"),
            br(),
            
            fluidRow(
                column(
                    width = 3,
                    wellPanel(
                        selectInput("longitudinal", label = "Longitudinal?", selected = "all", choices = c("all", "no", "first", "last")),
                        sliderInput("age_range", label = "Age (months)", value = c(0, 54), min = 0, max = 54, dragRange = TRUE),
                        selectInput("lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                        selectInput("sex", label = "Sex", choices = c("Female", "Male", "Other"), selected = c("Female", "Male", "Other"), multiple = TRUE, selectize = TRUE),
                        sliderInput("time_stamp", label = "Time stamp", value = c(as.Date("2019-10-22"), today()), min = as.Date("2019-10-22"), max = today(), dragRange = TRUE),
                        sliderInput("date_birth", label = "Time stamp", value = c(as.Date("2015-09-30"), today()), min = as.Date("2015-09-30"), max = today(), dragRange = TRUE),
                        actionButton("filter_button", label = "Apply", class = "btn-primary")
                    )
                ),
                column(width = 9, plotOutput("participants_n_plot"))
            ),
            br(),
            
            fluidRow(
                column(width = 2, tableOutput("participants_n_age_table")),
                column(width = 10, plotOutput("participants_n_age_plot"))
            ),
            br()
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # main multilex data
    ml_data <- reactive({
        list.files(here("data"), full.names = TRUE) %>% 
            map(readRDS) %>% 
            set_names(str_remove(list.files(here("data")), ".rds"))
    })
    
    # reactive logs (trigger upon button click)
    logs <- eventReactive(
        input$filter_button, {
            ml_data()$logs %>% 
                get_longitudinal(input$longitudinal) %>% 
                # get only complete responses from participants in the selected 
                # age range, language profile, and date range
                filter(
                    completed,
                    between(age, input$age_range[1], input$age_range[2]),
                    lp %in% input$lp,
                    between(time_stamp, input$time_stamp[1], input$time_stamp[2]),
                    between(date_birth, input$date_birth[1], input$date_birth[2]),
                    sex %in% input$sex
                )  
        })
    
    # plot with N by LP, and dominance
    output$participants_n_plot <- renderPlot({
        
        logs() %>% 
            # compute sample sizes
            count(lp, dominance) %>%
            # make plot
            ggplot() + 
            aes(dominance, n, fill = dominance, label = n) +
            facet_wrap(vars(lp)) + 
            geom_col(position = position_dodge(width = 1)) +
            geom_text(colour = "white", position = position_stack(vjust = 0.5)) +
            labs(
                x = "Language profile", 
                y = "Number of responses", 
                colour = "Dominance", 
                fill = "Dominance"
            ) +
            scale_fill_brewer(palette = "Set1") + 
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank(),
                legend.position = "none"
            )
        
    }, res = 96)
    
    # table with N by age, LP, and dominance
    output$participants_n_age_table <- renderTable({
        logs() %>%
            mutate(age_group = cut_width(round(age, 0), 4)) %>% 
            count(age_group, lp) %>% 
            pivot_wider(names_from = lp, values_from = n, values_fill = 0) %>% 
            mutate(Total = rowSums(across(where(is.numeric)))) %>% 
            rename(Age = age_group)
    }, width = "100%", na = "-", digits = 0)
    
    # plot with N by age, LP, and dominance
    output$participants_n_age_plot <- renderPlot({
        logs() %>% 
            mutate(age_group = round(age, 0)) %>% 
            count(age_group, lp, dominance) %>% 
            ggplot() + 
            aes(age_group, n, fill = dominance) +
            facet_wrap(vars(lp)) + 
            geom_col(colour = "white") +
            labs(
                x = "Age (months)", 
                y = "Number of responses", 
                colour = "Dominance", 
                fill = "Dominance"
            ) +
            scale_fill_brewer(palette = "Set1") + 
            theme(
                legend.position = "top"
            )
        
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
