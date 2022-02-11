library(shiny)
library(bslib)
library(thematic)
library(shinycssloaders)
library(multilex)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(stringr)
library(scales)
library(gt)
library(here)

ml_connect() # connect to multilex database

ml_data <- get_multilex()
ml_data$logs <- arrange(ml_data$logs, time_stamp)

# # set custom ggplot2 theme
# theme_set(
#     theme_minimal() +
#         theme(
#             axis.text = element_text(colour = "black"), 
#             panel.grid.major.x = element_blank(), 
#             panel.grid.minor.x = element_blank(), 
#             panel.border = element_rect(
#                 fill = NA,
#                 colour = "grey", 
#                 size = 0.75
#             )
#         )
# )

# define user interface 
ui <- fluidPage(
    # theme = bs_theme(
    #     bg = "white",
    #     fg = "#c8102e",
    #     primary = "#c8102e",
    #     secondary = "#1b3187", 
    # ),
    

    navbarPage(
        "multilex",
        
        tabPanel(
            "Participants",
            titlePanel("Participants"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("participants_id", label = "Participant (ID)", selected = unique(ml_data$logs$id)[1:20], choices = unique(ml_data$logs$id), multiple = TRUE, selectize = TRUE),
                    selectInput("participants_is_longitudinal", label = "Longitudinal?", selected = "all", choices = c("all", "no", "first", "last")),
                    sliderInput("participants_age_range", label = "Age (months)", value = c(0, 54), min = 0, max = 54, dragRange = TRUE),
                    selectInput("participants_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    dateRangeInput("participants_time_stamp", label = "Time stamp", start = as.Date("2019-10-22"), end = today(), min = as.Date("2019-10-22"), max = today()),
                    dateRangeInput("participants_date_birth", label = "Time stamp", start = as.Date("2015-09-30"), end = today(), min = as.Date("2015-09-30"), max = today()),
                    actionButton("participants_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 10,
                    withSpinner(tableOutput("participants_id_table"))
                )
            )
        ),
        
        tabPanel(
            "Responses",
            titlePanel("Responses"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("responses_is_longitudinal", label = "Longitudinal?", selected = "all", choices = c("all", "no", "first", "last")),
                    sliderInput("responses_age_range", label = "Age (months)", value = c(0, 54), min = 0, max = 54, dragRange = TRUE),
                    selectInput("responses_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    dateRangeInput("responses_time_stamp", label = "Time stamp", start = as.Date("2019-10-22"), end = today(), min = as.Date("2019-10-22"), max = today()),
                    dateRangeInput("responses_date_birth", label = "Time stamp", start = as.Date("2015-09-30"), end = today(), min = as.Date("2015-09-30"), max = today()),
                    actionButton("responses_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 10,
                    tabsetPanel(
                        id = "responses_panel", selected = "Summary", type = "pills",
                        tabPanel("Summary", plotOutput("responses_n_plot")),
                        tabPanel(
                            "Age", 
                            column(width = 2, withSpinner(tableOutput("responses_n_age_table"))),
                            column(width = 10, withSpinner(plotOutput("responses_n_age_plot")))
                        )
                    )
                )
            )
        ),
        
        tabPanel(
            "Vocabulary",
            titlePanel("Vocabulary"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("vocabulary_is_longitudinal", label = "Longitudinal?", selected = "all", choices = c("all", "no", "first", "last")),
                    sliderInput("vocabulary_age_range", label = "Age (months)", value = c(0, 54), min = 0, max = 54, dragRange = TRUE),
                    selectInput("vocabulary_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    sliderInput("vocabulary_time_stamp", label = "Time stamp", value = c(as.Date("2019-10-22"), today()), min = as.Date("2019-10-22"), max = today(), dragRange = TRUE),
                    sliderInput("vocabulary_date_birth", label = "Time stamp", value = c(as.Date("2015-09-30"), today()), min = as.Date("2015-09-30"), max = today(), dragRange = TRUE),
                    selectInput("vocabulary_type", label = "Type", selected = "all", choices = c("Comprehension" = "understands", "Production" = "produces"), multiple = FALSE),
                    actionButton("vocabulary_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 10,
                    tabsetPanel(
                        id = "vocabulary_panel", selected = "Total", type = "pills",
                        tabPanel(
                            "Total",
                            column(width = 4, withSpinner(plotOutput("vocabulary_total_plot", hover = "vocabulary_total_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_total_data")))
                        ),
                        tabPanel(
                            "By language",
                            column(width = 6, withSpinner(plotOutput("vocabulary_dominance_plot", hover = "vocabulary_dominance_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_dominance_data")))
                        ),
                        tabPanel(
                            "Conceptual",
                            column(width = 4, withSpinner(plotOutput("vocabulary_conceptual_plot", hover = "vocabulary_conceptual_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_conceptual_data")))
                        ),                        
                        tabPanel(
                            "Translation equivalents",
                            column(width = 4, withSpinner(plotOutput("vocabulary_te_plot", hover = "vocabulary_te_plot_hover"))),
                            column(width = 2, withSpinner(tableOutput("vocabulary_te_data")))
                        )                      
                    )
                )
            )
        ),
        tabPanel(
            "Norms"
        ),
        navbarMenu(
            "Other",
            tabPanel("Authors"),
            tabPanel("GitHub"),
            tabPanel("multilex R package")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # set ggplot theme
    thematic_shiny()
    
    # main multilex data
    ml_data <- reactive({ get_multilex() })
    
    # reactive logs (trigger upon button click)
    logs <- eventReactive(
        input$responses_filter_button, ignoreNULL = FALSE, {
            ml_data()$logs %>% 
                get_longitudinal(input$responses_is_longitudinal) %>% 
                # get only complete responses from participants in the selected 
                # age range, language profile, and date range
                filter(
                    completed,
                    between(age, input$responses_age_range[1], input$responses_age_range[2]),
                    lp %in% input$responses_lp,
                    between(time_stamp, input$responses_time_stamp[1], input$responses_time_stamp[2]),
                    between(date_birth, input$responses_date_birth[1], input$responses_date_birth[2])
                )  
        })
    
    vocabulary <- eventReactive(
        input$vocabulary_filter_button, ignoreNULL = FALSE, {
            
            logs <- ml_data()$logs %>% 
                get_longitudinal(input$vocabulary_is_longitudinal) %>% 
                # get only complete responses from participants in the selected 
                # age range, language profile, and date range
                filter(
                    completed,
                    between(age, input$vocabulary_age_range[1], input$vocabulary_age_range[2]),
                    lp %in% input$vocabulary_lp,
                    between(time_stamp, input$vocabulary_time_stamp[1], input$vocabulary_time_stamp[2]),
                    between(date_birth, input$vocabulary_date_birth[1], input$vocabulary_date_birth[2])
                )  
            
            left_join(logs, ml_data()$vocabulary) %>% 
                filter(type %in% input$vocabulary_type)
            
        })
    
    # participant searcher
    participants <- eventReactive(
        input$participants_filter_button, ignoreNULL = FALSE, {
            ml_data()$logs %>% 
                get_longitudinal(input$participants_is_longitudinal) %>% 
                # get only complete responses from participants in the selected 
                # age range, language profile, and date range
                filter(
                    completed,
                    id %in% input$participants_id,
                    between(age, input$participants_age_range[1], input$participants_age_range[2]),
                    lp %in% input$participants_lp,
                    between(time_stamp, input$participants_time_stamp[1], input$participants_time_stamp[2]),
                    between(date_birth, input$participants_date_birth[1], input$participants_date_birth[2])
                )  
    })
    
    # table with N by age, LP, and dominance
    output$participants_id_table <- renderTable({
        participants() %>%
            select(
                id, id_db, code, time, version, time_stamp, date_birth, age,
                lp, doe_catalan, doe_spanish, doe_others
            ) %>% 
            arrange(desc(time_stamp)) %>% 
            mutate_at(vars(starts_with("doe")), percent) %>% 
            mutate(age = round(age, 2)) %>% 
            head(20)
    }, width = "100%", na = "-")
    
    # table with N by age, LP, and dominance
    output$responses_n_age_table <- renderTable({
        logs() %>%
            mutate(age_group = cut_width(round(age, 0), 4)) %>% 
            count(age_group, lp) %>% 
            pivot_wider(names_from = lp, values_from = n, values_fill = 0) %>% 
            mutate(Total = rowSums(across(where(is.numeric)))) %>% 
            rename(Age = age_group)
    }, width = "100%", na = "-", digits = 0)
    
    # plot with N by LP, and dominance
    output$responses_n_plot <- renderPlot({
        
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
            theme(
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank(),
                legend.position = "none"
            )
        
    }, res = 96)
    
    # table with N by age, LP, and dominance
    output$responses_n_age_table <- renderTable({
        logs() %>%
            mutate(age_group = cut_width(round(age, 0), 4)) %>% 
            count(age_group, lp) %>% 
            pivot_wider(names_from = lp, values_from = n, values_fill = 0) %>% 
            mutate(Total = rowSums(across(where(is.numeric)))) %>% 
            rename(Age = age_group)
    }, width = "100%", na = "-", digits = 0)
    
    # plot with N by age, LP, and dominance
    output$responses_n_age_plot <- renderPlot({
        logs() %>% 
            mutate(age_group = round(age, 0)) %>% 
            count(age_group, lp, dominance) %>% 
            ggplot() + 
            aes(age_group, n, fill = dominance) +
            facet_wrap(vars(lp)) + 
            geom_col() +
            labs(
                x = "Age (months)", 
                y = "Number of responses", 
                colour = "Dominance", 
                fill = "Dominance"
            ) +
            theme(
                legend.position = "top"
            )
        
    }, res = 96)
    
    # plot vocabulary size by age
    output$vocabulary_total_plot <- renderPlot({
        vocabulary() %>% 
            ggplot() + 
            aes(age, vocab_prop_total, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_total_data <- renderTable({
        nearPoints(
            vocabulary() %>% 
                select(id, age, vocab_prop_total),
            input$vocabulary_total_plot_hover,
            xvar = "age",
            yvar = "vocab_prop_total"
        )
    })
    
    
    output$vocabulary_dominance_plot <- renderPlot({
        vocabulary() %>% 
            pivot_longer(
                c(vocab_prop_dominance_l1, vocab_prop_dominance_l2),
                names_transform = list(name = ~str_to_sentence(str_remove(., "vocab_prop_dominance_")))
            ) %>% 
            ggplot() + 
            facet_wrap(vars(name)) +
            aes(age, vocab_prop_total, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_dominance_data <- renderTable({
        nearPoints(
            vocabulary() %>% 
                pivot_longer(
                    c(vocab_prop_dominance_l1, vocab_prop_dominance_l2),
                    names_transform = list(name = ~str_to_sentence(str_remove(., "vocab_prop_dominance_")))
                ) %>% 
                select(id, age, name, value),
            input$vocabulary_dominance_plot_hover, 
            xvar = "age", 
            yvar = "value"
        )
    })
    
    # plot vocabulary size by age
    output$vocabulary_conceptual_plot <- renderPlot({
        vocabulary() %>% 
            ggplot() + 
            aes(age, vocab_prop_conceptual, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_conceptual_data <- renderTable({
        nearPoints(
            vocabulary()  %>% 
                select(id, age, vocab_prop_conceptual), 
            input$vocabulary_conceptual_plot_hover,
            xvar = "age", 
            yvar = "vocab_prop_conceptual"
        )
    })
    
    output$vocabulary_te_plot <- renderPlot({
        vocabulary() %>% 
            ggplot() + 
            aes(age, vocab_prop_conceptual, colour = lp) +
            geom_point(size = 1, alpha = 0.5) + 
            labs(
                x = "Age (months)", 
                y = "Vocabulary size", 
                colour = "Language profile", 
                fill = "Language profile"
            ) +
            scale_y_continuous(limits = c(0, 1), labels = percent) + 
            theme(
                legend.position = "top",
                legend.title = element_blank()
            )
        
    }, res = 96)
    
    output$vocabulary_te_data <- renderTable({
        nearPoints(
            vocabulary() %>% 
                select(id, age, vocab_prop_te),
            input$vocabulary_te_plot_hover, 
            xvar = "age", 
            yvar = "vocab_prop_te"
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
