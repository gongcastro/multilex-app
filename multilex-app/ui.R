library(shiny)
library(arrow)
library(bslib)
library(shinycssloaders)
library(thematic)
library(multilex)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(patchwork)
library(ggsci)
library(lubridate)
library(purrr)
library(stringr)
library(scales)
library(gt)
library(tictoc)
library(here)

# render graphics using Cairo
options(shiny.usecairo = TRUE)

theme_set(theme_ggdist())

# connect to multilex database and download data
ml_data <- get_multilex()
ml_data$logs <- arrange(ml_data$logs, time_stamp)

# define user interface 
ui <- fluidPage(
    
    navbarPage(
        "multilex",
        
        tabPanel(
            "Participants",
            icon = icon("user"),
            titlePanel("Participants"),
            br(),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("participants_id", label = "Participant (ID)", selected = unique(arrange(ml_data$logs, desc(time_stamp))$id[1:20]), choices = unique(ml_data$logs$id), multiple = TRUE, selectize = TRUE),
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
            icon = icon("database"),
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
            icon = icon("comment"),
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
            "Norms",
            icon = icon("signal"),
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    selectInput("norms_label", label = "Item", selected = "casa / casa", choices = pull(collect(distinct(ml_data$predictions, label)), label)),
                    checkboxInput("norms_show_uncertainty", label = "Show uncertainty?", value = TRUE),
                    # sliderInput("norms_age_range", label = "Age (months)", value = c(8, 40), min = 8, max = 40, dragRange = TRUE),
                    # selectInput("norms_lp", label = "Language profile", choices = c("Monolingual", "Bilingual", "Other"), selected = c("Monolingual", "Bilingual", "Other"), multiple = TRUE, selectize = TRUE),
                    # selectInput("norms_type", label = "Type", selected = "Comprehension", choices = c("Comprehension" = "understands", "Production" = "produces"), multiple = FALSE),
                    # selectInput("norms_item_dominance", label = "Dominance", selected = "L1", choices = c("L1", "L2"), multiple = TRUE),
                    # wellPanel(
                    #     "Prior: Beta(α, β)",
                    #     sliderInput("norms_prior_alpha", label = "α", value = 5, min = 0, max = 100, step = 0.1, ticks = TRUE),
                    #     sliderInput("norms_prior_beta", label = "β", value = 5, min = 0, max = 1000, step = 0.1, ticks = TRUE),
                    # ),
                    actionButton("norms_filter_button", label = "Apply", class = "btn-primary")
                ),
                mainPanel(
                    width = 10,
                    tabsetPanel(
                        id = "norms_panel", selected = "MCMC (model estimates)", type = "pills",
                        # tabPanel(
                        #     "Grid approximation",
                        #     withSpinner(plotOutput("norms_plot"))
                        # ),
                        tabPanel(
                            "MCMC (model estimates)",
                            withSpinner(plotOutput("norms_plot_model"))
                        )
                    )
                )
            )
        ),
        navbarMenu(
            "Other",
            tabPanel(
                "Authors",
                icon = icon("pen"),
                column(
                    width = 4,
                    h2("Gonzalo Garcia-Castro"),
                    fluidRow(
                        a(href = "https://orcid.org/0000-0001-6938-2498", icon("orcid")),
                        a(href = "https://github.com/gongcastro", icon("github"))
                        
                    )
                ),
                column(
                    width = 4,
                    h2("Daniela S. Avila-Varela"),
                    fluidRow(
                        a(href = "https://orcid.org/0000-0002-3518-8117", icon("orcid")),
                        a(href = "https://github.com/DanielaAvilaVarela", icon("github"))
                        
                    )
                ),
                column(
                    width = 4,
                    h2("Núria Sebastian-Galles"),
                    fluidRow(
                        a(href = "https://orcid.org/0000-0002-8553-4209", icon("orcid")),
                        a(href = "https://github.com/nuriasebastiangalles", icon("github"))
                        
                    )
                )
            ),
            tabPanel(
                a(href = "https://github.com/gongcastro/multilex-app", icon("github"), "GitHub")
            ),
            tabPanel(
                a(href = "https://gongcastro.github.io/multilex/", icon("r-project"), "R package"),
            )
        )
    )
)  

